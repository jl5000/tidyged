

#' Add a Repository record to a tidygedcom object
#'
#' @details This function will automatically assign a unique xref for this record. Most users
#' will only need to use the sex, submitters, and individual_notes parameters (and of course gedcom).
#' 
#' If you need to add further information about this individual (e.g. names), use the 
#' add_individual_* functions.
#' 
#' The function will automatically split the individual_notes onto separate lines if the 
#' character limit in the Gedcom standard is exceeded.
#'
#' @param gedcom A tidygedcom object.
#' @param name The name of the repository.
#' @param address_first_line The first line of the repository address.
#' @param city The city of the repository.
#' @param state The state/county of the repository.
#' @param postal_code The postal code of the repository.
#' @param country The country of the repository.
#' @param phone_number A character vector containing up to three phone numbers of the repository.
#' @param email A character vector containing up to three email addresses of the repository.
#' @param fax A character vector containing up to three fax numbers of the repository.
#' @param web_page A character vector containing up to three web pages of the repository.
#' @param user_reference_number A user-defined number or text that the submitter uses to identify 
#' this record. See the Gedcom 5.5.1 Standard for more details.
#' @param user_reference_type A user-defined definition of the user_reference_number.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param repository_notes A character vector of notes accompanying this Repository record.
#' These could be xrefs to existing Note records.
#'
#' @return An updated tidygedcom object including the Repository record.
#' @export
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
  
  repo_notes <- purrr::map(repository_notes, ~ if(grepl(xref_pattern, .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
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

#' Remove a Repository record from a tidygedcom object
#'
#' @param gedcom A tidygedcom object.
#'
#' @return An updated tidygedcom object excluding the active Repository record.
#' @export
remove_repository <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_repo(), is_repository)
  
  gedcom %>% 
    remove_section(1, "REPO", get_active_record(.)) %>% 
    dplyr::filter(record != get_active_record(.), value != get_active_record(.)) %>% 
    null_active_record()
}
