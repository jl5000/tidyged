
add_multimedia <- function(gedcom,
                           file_reference,
                           format,
                           source_media = character(),
                           title = character(),
                           user_reference_number = character(),
                           user_reference_type = character(),
                           automated_record_id = character(),
                           multimedia_notes = character()) {
  
  xref <- assign_xref(xref_prefix_obje(), gedcom = gedcom)
  
  media_notes <- purrr::map(multimedia_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  media_record <- MULTIMEDIA_RECORD(xref_obje = xref,
                                    multimedia_file_reference = file_reference,
                                    multimedia_format = format,
                                    source_media_type = source_media,
                                    descriptive_title = title,
                                    user_reference_number = user_reference_number,
                                    user_reference_type = user_reference_type,
                                    automated_record_id = automated_record_id,
                                    notes = media_notes)
  
  gedcom %>% 
    tibble::add_row(media_record, .before = nrow(.)) %>% 
    set_active_record(xref)
}

update_multimedia <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_obje(), is_multimedia)
  
  
}

remove_multimedia <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_obje(), is_multimedia)
  
  gedcom %>% 
    dplyr::filter(record != get_active_record(.), value != get_active_record(.)) %>% 
    null_active_record()
}
