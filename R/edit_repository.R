
add_repository <- function(gedcom,
                           name,
                           address_first_line = character(),
                           city = character(),
                           state = character(),
                           postal_code = character(),
                           country = character(),
                           phone_number = character(),
                           email = character(),
                           fax = character(),
                           web_page = character(),
                           user_reference_number = character(),
                           user_reference_type = character(),
                           automated_record_id = character(),
                           repository_notes = character()) {
  
  xref <- assign_xref(xref_prefix_repo(), gedcom = gedcom)
  
  address_lines <- c(address_first_line, city, state, postal_code, country)
  
  if(length(address_lines) > 4) address_lines <- address_lines[1:4]
    
  if(length(address_lines) == 0) {
    
    address <- tibble::tibble()
    
  } else {
    
    address <- ADDRESS_STRUCTURE(all_address_lines = address_lines,
                                 address_city = city,
                                 address_state = state,
                                 address_postal_code = postal_code,
                                 address_country = country,
                                 phone_number = phone_number,
                                 address_email = email,
                                 address_fax = fax,
                                 address_web_page = web_page)
  }
  
  repo_notes <- purrr::map(repository_notes, ~ ifelse(grepl("^@.{1,20}@$", .x),
                                                      NOTE_STRUCTURE(xref_note = .x),
                                                      NOTE_STRUCTURE(submitter_text = .x)))
  
  repo_record <- REPOSITORY_RECORD(xref_repo = xref,
                                   name_of_repository = name,
                                   address = address,
                                   user_reference_number = user_reference_number,
                                   user_reference_type = user_reference_type,
                                   automated_record_id = automated_record_id,
                                   notes = repo_notes)
  
  gedcom %>% 
    tibble::add_row(repo_record, .before = nrow(.)) %>% 
    set_active_record(xref)
}

update_repository <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_repo(), is_repository)
  
}

remove_repository <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_repo(), is_repository)
  #TODO: Remove source_repository_citation 
  gedcom %>% 
    dplyr::filter(record != get_active_record(.), value != get_active_record(.)) %>% 
    null_active_record()
}
