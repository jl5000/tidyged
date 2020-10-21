

#' Add a Submission record to a tidygedcom object
#'
#' @details The sending system uses a submission record to send instructions and 
#' information to the receiving system. Each GEDCOM transmission file should have only one
#' submission record. 
#' 
#' This function will automatically assign a unique xref for this record.
#' 
#' The function will automatically split the submission_notes onto separate lines if the 
#' character limit in the Gedcom standard is exceeded.
#'
#' @param gedcom A tidygedcom object.
#' @param name_of_family_file Name under which family names for ordinances are stored 
#' in the temple's family file.
#' @param temple_code See the Gedcom 5.5.1 Standard for more details.
#' @param generations_of_ancestors The number of generations of ancestors included in this file.
#' @param generations_of_descendants The number of generations of descendants included in this file. 
#' @param ordinance_process_flag See the Gedcom 5.5.1 Standard for more details. 
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param submission_notes A character vector of notes accompanying this Submission record.
#' These could be xrefs to existing Note records.
#'
#' @return An updated tidygedcom object including the Submission record.
#' @export
add_submission <- function(gedcom,
                           name_of_family_file = character(),
                           temple_code = character(),
                           generations_of_ancestors = character(),
                           generations_of_descendants = character(),
                           ordinance_process_flag = character(),
                           automated_record_id = character(),
                           submission_notes = character()) {
  
  if(num_subn(gedcom) > 0) {
    warning("Submission not added because one already exists")
    return(gedcom)
  }
  
  xref <- assign_xref(xref_prefix_subn(), gedcom = gedcom)
  
  xref_subm <- dplyr::filter(gedcom, record == "HD", tag == "SUBM")$value
  
  subn_notes <- purrr::map(submission_notes, ~ if(grepl(xref_pattern, .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  subn_record <- SUBMISSION_RECORD(xref_subn = xref,
                                   xref_subm = xref_subm,
                                   name_of_family_file = name_of_family_file,
                                   temple_code = temple_code,
                                   generations_of_ancestors = generations_of_ancestors,
                                   generations_of_descendants = generations_of_descendants,
                                   ordinance_process_flag = ordinance_process_flag,
                                   automated_record_id = automated_record_id,
                                   notes = subn_notes)
  
  gedcom %>% 
    tibble::add_row(subn_record, .before = nrow(.)) %>% 
    set_active_record(xref)
}

#' Remove a Submission record from a tidygedcom object
#'
#' @param gedcom A tidygedcom object.
#'
#' @return An updated tidygedcom object excluding the Submission record.
#' @export
remove_submission <- function(gedcom) {
  
  xref <- dplyr::filter(gedcom, tag == record_tag_subn())$record
  
  if(length(xref) == 1) {
  
    gedcom %>% 
      dplyr::filter(record != xref, value != xref) %>% 
      null_active_record()
  }
}
