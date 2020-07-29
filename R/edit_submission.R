
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
  
  subn_notes <- purrr::map(submission_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
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


remove_submission <- function(gedcom) {
  
  xref <- dplyr::filter(gedcom, tag == record_tag_subn())$record
  
  if(length(xref) == 1) {
  
    gedcom %>% 
      dplyr::filter(record != xref, value != xref) %>% 
      null_active_record()
  }
}
