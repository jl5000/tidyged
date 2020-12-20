


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
add_individual <- function(gedcom,
                           sex = "U",
                           user_reference_number = character(),
                           user_reference_type = character(),
                           automated_record_id = character(),
                           individual_notes = character(),
                           multimedia_links = character()) {
  
  xref <- assign_xref(.pkgenv$xref_prefix_indi, gedcom = gedcom)
  
  indi_notes <- purrr::map(individual_notes, ~ if(grepl(xref_pattern(), .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(user_text = .x) }  )
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_multimedia(gedcom), tags = "FILE") %>% 
    purrr::map(MULTIMEDIA_LINK)
  
  ind_record <- INDIVIDUAL_RECORD(xref_indi = xref,
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
#' @param remove_associations Whether to also remove associations with this individual in 
#' other individual records. Defaults to TRUE.
#'
#' @return An updated tidygedcom object excluding the active Individual record.
#' 
#' @export
remove_individual <- function(gedcom, remove_associations = TRUE) {
  
  check_active_record_valid(gedcom, .pkgenv$record_string_indi, is_individual)
  active_record <- get_active_record(gedcom)
  
  if(remove_associations) {
    gedcom <- remove_section(gedcom, 1, "ASSO", active_record)
    message("Associations with ", get_individual_name(gedcom, active_record),
            " also removed")
  }
  
  gedcom %>% 
    dplyr::filter(record != active_record, value != active_record) %>% 
    null_active_record()
}


