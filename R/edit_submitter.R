
#' @export
add_submitter <- function(gedcom,
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
                          language_preference = character(),
                          submitter_registered_rfn = character(),
                          automated_record_id = character()) {
  
  xref <- assign_xref(xref_prefix_subm(), gedcom = gedcom)
  
  address_lines <- c(address_first_line, city, state, postal_code, country)
  
  if(length(address_lines) > 4) address_lines <- address_lines[1:4]
  
  if(length(address_lines) == 0) {
    
    address <- character()
    
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
  
  subm_record <- SUBMITTER_RECORD(xref_subm = xref,
                                  submitter_name = name,
                                  address = address,
                                  language_preference = language_preference,
                                  submitter_registered_rfn = submitter_registered_rfn,
                                  automated_record_id = automated_record_id)
  
  gedcom %>% 
    tibble::add_row(subm_record, .before = nrow(.)) %>% 
    set_active_record(xref)
  
}

#' @export
update_submitter <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_subm(), is_submitter)
  
  
  
  
  
}

#' @export
remove_submitter <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_subm(), is_submitter)
  
  
  
  null_active_record(gedcom)
}

#' @export
subm <- function(name = unname(Sys.info()["user"]),
                 address_first_line = character(),
                 city = character(),
                 state = character(),
                 postal_code = character(),
                 country = character(),
                 phone_number = character(),
                 email = character(),
                 fax = character(),
                 web_page = character(),
                 language_preference = character(),
                 submitter_registered_rfn = character(),
                 automated_record_id = character()) {
  
  address_lines <- c(address_first_line, city, state, postal_code, country)
  
  if(length(address_lines) > 4) address_lines <- address_lines[1:4]
  
  if(length(address_lines) == 0) {
    
    address <- character()
    
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
  
  #Shortcut used in gedcom function
  SUBMITTER_RECORD(xref_subm = assign_xref(xref_prefix_subm(), 1),
                   submitter_name = name,
                   address = address,
                   language_preference = language_preference,
                   submitter_registered_rfn = submitter_registered_rfn,
                   automated_record_id = automated_record_id)
  
}
