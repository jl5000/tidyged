


#' Add an Individual record to a tidygedcom object
#'
#' This function adds a new record for an individual.
#'
#' This function will automatically assign a unique xref for this record. Most users
#' will only need to use the sex and individual_notes parameters (and of course gedcom).
#' 
#' If you need to add further information about this individual (e.g. names), use the 
#' add_individual_* functions.
#' 
#' @param gedcom A tidygedcom object.
#' @param sex The sex of the individual. Either "M" (male), "F" (female), "U" (undetermined),
#' "X" (intersex), or "N" (not recorded).
#' @param user_reference_number A user-defined number or text that the submitter uses to identify 
#' this record. See the Gedcom 5.5.5 Specification for more details.
#' @param user_reference_type A user-defined definition of the user_reference_number.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param individual_notes A character vector of notes accompanying this Individual record.
#' These could be xrefs to existing Note records.
#' @param multimedia_links A character vector of multimedia file references accompanying this 
#' Individual record. These could be xrefs to existing Multimedia records.
#'
#' @return An updated tidygedcom object including the Individual record.
#' @export
#' @tests
#' expect_snapshot_value(add_individual(gedcom(subm("Me")),
#'                                      sex = "M", user_reference_number = 1234,
#'                                      user_reference_type = "something",
#'                                      automated_record_id = "5678",
#'                                      individual_notes = c("Note1", "Note 2")) %>% 
#'                        remove_dates_for_tests(), "json2")
add_individual <- function(gedcom,
                           sex = "U",
                           user_reference_number = character(),
                           user_reference_type = character(),
                           automated_record_id = character(),
                           individual_notes = character(),
                           multimedia_links = character()) {
  
  xref <- tidygedcom.internals::assign_xref(.pkgenv$xref_prefix_indi, gedcom = gedcom)
  
  indi_notes <- purrr::map(individual_notes, ~ if(grepl(xref_pattern(), .x)) {
    tidygedcom.internals::NOTE_STRUCTURE(xref_note = .x) 
  } else { 
    tidygedcom.internals::NOTE_STRUCTURE(user_text = .x) 
  }  )
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_multimedia(gedcom), tags = "FILE") %>% 
    purrr::map(tidygedcom.internals::MULTIMEDIA_LINK)

  ind_record <- tidygedcom.internals::INDIVIDUAL_RECORD(xref_indi = xref,
                                                        sex_value = sex,
                                                        user_reference_number = user_reference_number,
                                                        user_reference_type = user_reference_type,
                                                        automated_record_id = automated_record_id,
                                                        notes = indi_notes,
                                                        multimedia_links = media_links) 
  
  gedcom %>%
    tibble::add_row(ind_record, .before = nrow(.)) %>% 
    set_active_record(xref)
}


#' Remove an Individual record from a tidygedcom object
#' 
#' This function removes an active Individual record from the tidygedcom object.
#' 
#' At a minimum it will also remove references to this individual in Family group records.
#' If remove_associations is TRUE (default) it will remove associations with this
#' individual in other Individual records.
#' 
#' @param gedcom A tidygedcom object.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param remove_associations Whether to also remove associations with this individual in 
#' other individual records. Defaults to TRUE.
#'
#' @return An updated tidygedcom object excluding the active Individual record.
#' 
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) %>% add_individual() %>% remove_individual())
remove_individual <- function(gedcom, 
                              xref = character(),
                              remove_associations = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_individual)
  
  if(remove_associations) {
    gedcom <- remove_section(gedcom, 1, "ASSO", xref)
    message("Associations with ", get_individual_name(gedcom, xref),
            " also removed")
  }
  
  gedcom %>% 
    dplyr::filter(record != xref, value != xref) %>% 
    null_active_record()
}


