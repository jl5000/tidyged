


#' Add an Individual record to a tidyged object
#'
#' @details If you need to add further information about this individual (e.g. names), use the 
#' add_indi_* functions.
#' 
#' @param gedcom A tidyged object.
#' @param quick_name A shortcut to quickly define a name for this individual. This is a shortcut for
#' the add_indi_names() function (which you should really use instead), but this is useful
#' for quick demonstrations.
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
#'
#' @return An updated tidyged object including the Individual record.
#' @export
#' @tests
#' expect_snapshot_value(add_indi(gedcom(subm("Me")),
#'                                      sex = "M", user_reference_number = 1234,
#'                                      user_reference_type = "something",
#'                                      indi_notes = c("Note1", "Note 2")) %>% 
#'                        remove_dates_for_tests(), "json2")
add_indi <- function(gedcom,
                     quick_name = character(),
                     sex = "U",
                     user_reference_number = character(),
                     user_reference_type = character(),
                     indi_notes = character(),
                     multimedia_links = character()) {
  
  xref <- assign_xref(.pkgenv$xref_prefix_indi, gedcom = gedcom)
  
  indiv_notes <- purrr::map(indi_notes, tidyged.internals::NOTE_STRUCTURE)
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_multimedia(gedcom), tags = "FILE") %>% 
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
  
  if (length(quick_name) > 0) 
    temp <- add_indi_names(temp, given = quick_name)
  
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
#' other individual records. Defaults to TRUE.
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
  
  if(remove_associations) gedcom <- remove_section(gedcom, 1, "ASSO", xref)

  gedcom %>% 
    dplyr::filter(record != xref, value != xref) %>% 
    null_active_record()
}



#' Identify all descendants for an individual
#' 
#' This function identifies records in an entire branch of the family tree below a certain individual.
#' 
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param include_individual Whether to also include the individual themselves.
#' @param include_spouses Whether to also include all spouses of this individual (and their descendants).
#' @param include_families Whether to also include all Family Group records where this individual is a spouse.
#'
#' @return A vector of xrefs of descendants.
#' @export
identify_descendants <- function(gedcom,
                                 individual = character(),
                                 include_individual = FALSE,
                                 include_spouses = FALSE,
                                 include_families = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  return_xrefs <- NULL
  
  spou_xref <- get_spouses(gedcom, xref)
  chil_xref <- get_children(gedcom, xref)
  fams_xref <- get_families_as_spouse(gedcom, xref)
  
  # if spouse is to be included, add their children to be included
  if (include_spouses) {
    # we don't use purrr::map here because the return values could vary in length
    spou_chil <- NULL
    for(i in seq_along(spou_xref)) {
      spou_chil <- c(spou_chil, get_children(gedcom, spou_xref[i]))
    }
    chil_xref <- unique(c(chil_xref, spou_chil))
  }
  
  #deal with family groups first (while the individuals are still in them)
  if (include_families) return_xrefs <- c(return_xrefs, fams_xref)
  if (include_spouses) return_xrefs <- c(return_xrefs, spou_xref)
  if (include_individual) return_xrefs <- c(return_xrefs, xref)
  
  # identify children
  for(i in seq_along(chil_xref)) {
    return_xrefs <- c(return_xrefs,
                      identify_descendants(gedcom, chil_xref[i], TRUE, TRUE,TRUE))
  }
  
  return_xrefs
}