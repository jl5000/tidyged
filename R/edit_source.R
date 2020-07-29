
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
                       automated_record_id = character(),
                       data_notes = character(),
                       source_notes = character()) {
  
  xref <- assign_xref(xref_prefix_sour(), gedcom = gedcom)
  
  dat_notes <- purrr::map(data_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  sour_notes <- purrr::map(source_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  sour_record <- SOURCE_RECORD(xref_sour = xref,
                               events_recorded = events_recorded,
                               date_period_covered = date_period_covered,
                               source_jurisdiction_place = jurisdiction,
                               responsible_agency = responsible_agency,
                               data_notes = dat_notes,
                               source_originator = originator,
                               source_descriptive_title = title,
                               source_filed_by_entry = short_title,
                               source_publication_facts = publication_detail,
                               text_from_source = source_text,
                               user_reference_number = user_reference_number,
                               user_reference_type = user_reference_type,
                               automated_record_id = automated_record_id,
                               notes = sour_notes)
    
    gedcom %>% 
      tibble::add_row(sour_record, .before = nrow(.)) %>% 
      set_active_record(xref)
}


add_source_repository_citation <- function(gedcom,
                                           repository,
                                           call_number = character(),
                                           media_type = character(),
                                           citation_notes = character()) {
  
  check_active_record_valid(gedcom, record_string_sour(), is_source)
  
  repo_xref <- find_xref(gedcom, xrefs_repositories(gedcom), "NAME", repository)
  
  cit_notes <- purrr::map(citation_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  citation <- SOURCE_REPOSITORY_CITATION(xref_repo = repo_xref,
                                         notes = cit_notes,
                                         source_call_number = call_number,
                                         source_media_type = media_type) %>% add_levels(1)
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "SOUR")
  
  gedcom %>%
    tibble::add_row(citation, .before = next_row) %>% 
    finalise()
  
}


remove_source <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_sour(), is_source)
  #TODO: Remove source_citation
  gedcom %>% 
    dplyr::filter(record != get_active_record(.), value != get_active_record(.)) %>% 
    null_active_record()
}
