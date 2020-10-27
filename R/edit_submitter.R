

#' Add a Submitter record to a tidygedcom object
#'
#' @details The submitter record identifies an individual or organization that contributed 
#' information contained in the GEDCOM transmission.
#' 
#' This function will automatically assign a unique xref for this record.
#' 
#' The function will automatically split the submitter_notes onto separate lines if the 
#' character limit in the Gedcom standard is exceeded.
#'
#' @param gedcom A tidygedcom object.
#' @param name The name of the submitter.
#' @param address_first_line The first line of the submitter address.
#' @param city The city of the submitter.
#' @param state The state/county of the submitter.
#' @param postal_code The postal code of the submitter.
#' @param country The country of the submitter.
#' @param phone_number A character vector containing up to three phone numbers of the submitter.
#' @param email A character vector containing up to three email addresses of the submitter.
#' @param fax A character vector containing up to three fax numbers of the submitter.
#' @param web_page A character vector containing up to three web pages of the submitter.
#' @param language_preference The language in which the submitter prefers to communicate. 
#' @param submitter_registered_rfn See the Gedcom 5.5.1 Standard for more details.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param submitter_notes A character vector of notes accompanying this Submitter record.
#' These could be xrefs to existing Note records.
#'
#' @return An updated tidygedcom object including the Submitter record.
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
                          automated_record_id = character(),
                          submitter_notes = character()) {
  
  xref <- assign_xref(xref_prefix_subm(), gedcom = gedcom)
  
  address_lines <- c(address_first_line, city, state, postal_code, country)
  
  if(length(address_lines) > 4) address_lines <- address_lines[1:4]
  
  if(length(address_lines) == 0) {
    
    address <- ADDRESS_STRUCTURE(character())
    
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
  
  subm_notes <- purrr::map(submitter_notes, ~ if(grepl(xref_pattern, .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  subm_record <- SUBMITTER_RECORD(xref_subm = xref,
                                  submitter_name = name,
                                  address = address,
                                  language_preference = language_preference,
                                  submitter_registered_rfn = submitter_registered_rfn,
                                  automated_record_id = automated_record_id,
                                  notes = subm_notes)
  
  gedcom %>% 
    tibble::add_row(subm_record, .before = nrow(.)) %>% 
    set_active_record(xref)
  
}


#' Remove a Submitter record from a tidygedcom object
#'
#' @param gedcom A tidygedcom object.
#'
#' @return An updated tidygedcom object excluding the active Submitter record.
#' @export
remove_submitter <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_subm(), is_submitter)
  
  if(dplyr::filter(gedcom, record == "HD", tag == "SUBM")$value == get_active_record(gedcom))
    stop("Submitter is GEDCOM file submitter - cannot remove")
  
  gedcom %>% 
    dplyr::filter(record != get_active_record(.), value != get_active_record(.)) %>% 
    null_active_record()
}


#' Define a Submitter record for a new tidygedcom object
#'
#' @details 
#' This function is supposed to be used in the gedcom() function to define a
#' new tidygedcom object.
#' 
#' This submitter record identifies the individual or organization that contributed 
#' information contained in the GEDCOM transmission.
#' 
#' The function will automatically split the submitter_notes onto separate lines if the 
#' character limit in the Gedcom standard is exceeded.
#'
#' @param name The name of the submitter.
#' @param address_first_line The first line of the submitter address.
#' @param city The city of the submitter.
#' @param state The state/county of the submitter.
#' @param postal_code The postal code of the submitter.
#' @param country The country of the submitter.
#' @param phone_number A character vector containing up to three phone numbers of the submitter.
#' @param email A character vector containing up to three email addresses of the submitter.
#' @param fax A character vector containing up to three fax numbers of the submitter.
#' @param web_page A character vector containing up to three web pages of the submitter.
#' @param language_preference The language in which the submitter prefers to communicate. 
#' @param submitter_registered_rfn See the Gedcom 5.5.1 Standard for more details.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param submitter_notes A character vector of notes accompanying this Submitter record.
#' These could be xrefs to existing Note records.
#'
#' @return A Submitter record to be incorporated into a new tidygedcom object.
#' @export
subm <- function(name = unname(Sys.info()["user"]),
                 local_address_lines = character(),
                 city = character(),
                 state = character(),
                 postal_code = character(),
                 country = character(),
                 phone_number = character(),
                 email = character(),
                 fax = character(),
                 web_page = character(),
                 automated_record_id = character(),
                 submitter_notes = character()) {
  
  if(length(local_address_lines) > 3) local_address_lines <- local_address_lines[1:3]
  
  address <- ADDRESS_STRUCTURE(local_address_lines = local_address_lines,
                               address_city = city,
                               address_state = state,
                               address_postal_code = postal_code,
                               address_country = country,
                               phone_number = phone_number,
                               address_email = email,
                               address_fax = fax,
                               address_web_page = web_page)
  
  subm_notes <- purrr::map(submitter_notes, ~ if(grepl(xref_pattern, .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(user_text = .x) }  )
  
  SUBMITTER_RECORD(xref_subm = assign_xref(xref_prefix_subm(), 1),
                   submitter_name = name,
                   address = address,
                   automated_record_id = automated_record_id,
                   notes = subm_notes)
  
}
