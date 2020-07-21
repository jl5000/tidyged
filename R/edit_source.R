
add_source <- function(gedcom,
                       events_recorded = character(),
                       date_period_covered = date_period(),
                       jurisdiction = character(),
                       responsible_agency = character(),
                       originator = character(),
                       title = character(),
                       short_title = character(),
                       publication_detail = character(),
                       source_text = character(),
                       user_reference_number = character(),
                       user_reference_type = character(),
                       automated_record_id = character()) {
  
  xref <- assign_xref(xref_prefix_sour(), gedcom = gedcom)
  
  sour_record <- SOURCE_RECORD(xref_sour = xref,
                               events_recorded = events_recorded,
                               date_period_covered = date_period_covered,
                               source_jurisdiction_place = jurisdiction,
                               responsible_agency = responsible_agency,
                               source_originator = originator,
                               source_descriptive_title = title,
                               source_filed_by_entry = short_title,
                               source_publication_facts = publication_detail,
                               text_from_source = source_text,
                               user_reference_number = user_reference_number,
                               user_reference_type = user_reference_type,
                               automated_record_id = automated_record_id)
    
    gedcom %>% 
      tibble::add_row(sour_record, .before = nrow(.)) %>% 
      set_active_record(xref)
}

add_source_data_notes <- function(gedcom) {
  
  
}

add_source_repository_citation <- function(gedcom) {
  
  
}

update_source <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_sour(), is_source)
  
}

remove_source <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_sour(), is_source)
  
  
  
  
  
  null_active_record(gedcom)
}
