
add_note <- function(gedcom,
                     text,
                     user_reference_number = character(),
                     user_reference_type = character(),
                     automated_record_id = character()) {
  
  xref <- assign_xref(xref_prefix_note(), gedcom = gedcom)
  
  note_record <- NOTE_RECORD(xref_note = xref,
                             submitter_text = text,
                             user_reference_number = user_reference_number,
                             user_reference_type = user_reference_type,
                             automated_record_id = automated_record_id)
  
  gedcom %>% 
    tibble::add_row(note_record, .before = nrow(.)) %>% 
    set_active_record(xref)
}


remove_note <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_note(), is_note)
  
  gedcom %>% 
    dplyr::filter(record != get_active_record(.), value != get_active_record(.)) %>% 
    null_active_record()
}
