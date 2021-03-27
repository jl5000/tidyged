


#' Add an Individual record to a tidyged object
#'
#' @details If you need to add further information about this individual (e.g. names), use the 
#' add_indi_* functions.
#' 
#' @param gedcom A tidyged object.
#' @param sex The sex of the individual. Either "M" (male), "F" (female), "U" (undetermined),
#' "X" (intersex), or "N" (not recorded).
#' @param user_reference_number A unique user-defined number or text that the submitter 
#' uses to identify this record. You can supply more than one in a vector.
#' @param user_reference_type A user-defined definition of the user_reference_number(s). If this
#' parameter is used, there must be a reference type for every reference number defined.
#' @param indi_notes A character vector of notes accompanying this Individual record.
#' These could be xrefs to existing Note records.
#' @param multimedia_links A character vector of multimedia file references accompanying this 
#' Individual record. These could be xrefs to existing Multimedia records.
#' @param qn A shortcut to quickly define a name for this individual. This is a shortcut for
#' the add_indi_names() function (which you should really use instead), but this is useful
#' for quick demonstrations.
#'
#' @return An updated tidyged object including the Individual record.
#' @export
#' @tests
#' expect_snapshot_value(add_indi(gedcom(subm("Me")),
#'                                      sex = "M", user_reference_number = 1234,
#'                                      user_reference_type = "something",
#'                                      indi_notes = c("Note1", "Note 2")) %>% 
#'                        tidyged.internals::remove_dates_for_tests(), "json2")
add_indi <- function(gedcom,
                     sex = "U",
                     user_reference_number = character(),
                     user_reference_type = character(),
                     indi_notes = character(),
                     multimedia_links = character(),
                     qn = character()) {
  
  xref <- tidyged.internals::assign_xref_indi(gedcom)
  
  indiv_notes <- purrr::map(indi_notes, tidyged.internals::NOTE_STRUCTURE)
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_media(gedcom), tags = "FILE") %>% 
    purrr::map(tidyged.internals::MULTIMEDIA_LINK)

  ind_record <- tidyged.internals::INDIVIDUAL_RECORD(xref_indi = xref,
                                                     sex_value = sex,
                                                     user_reference_number = user_reference_number,
                                                     user_reference_type = user_reference_type,
                                                     notes = indiv_notes,
                                                     multimedia_links = media_links) 
  
  temp <- gedcom %>%
    tibble::add_row(ind_record, .before = nrow(.)) %>% 
    set_active_record(xref)
  
  if (length(qn) > 0) 
    temp <- add_indi_names(temp, name_pieces(given = qn))
  
  temp
}


#' Remove an Individual record from a tidyged object
#' 
#' This function removes an active Individual record from the tidyged object.
#' 
#' At a minimum it will also remove references to this individual in Family group records.
#' If remove_associations is TRUE (default) it will remove associations with this
#' individual in other Individual records.
#' 
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param remove_associations Whether to also remove associations with this individual in 
#' other individual records. Defaults to TRUE. You shouldn't really leave dead links to
#' individual records that no longer exist.
#'
#' @return An updated tidyged object excluding the selected Individual record.
#' 
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) %>% add_indi() %>% remove_indi())
remove_indi <- function(gedcom, 
                        individual = character(),
                        remove_associations = TRUE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  if(remove_associations) gedcom <- tidyged.internals::remove_section(gedcom, 1, "ASSO", xref)

  gedcom %>% 
    dplyr::filter(record != xref, !(value == xref & tag != "ASSO")) %>%
    null_active_record()
}

