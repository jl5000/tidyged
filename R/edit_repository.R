

#' Add a Repository record to a tidygedcom object
#'
#' @details This function will automatically assign a unique xref for this record. Most users
#' will only need to use the sex, submitters, and individual_notes parameters (and of course gedcom).
#' 
#' If you need to add further information about this individual (e.g. names), use the 
#' add_individual_* functions.
#' 
#' @param gedcom A tidygedcom object.
#' @param name The name of the repository.
#' @param local_address_lines The first lines of the repository address.
#' @param city The city of the repository.
#' @param state The state/county of the repository.
#' @param postal_code The postal code of the repository.
#' @param country The country of the repository.
#' @param phone_number A character vector containing up to three phone numbers of the repository.
#' @param email A character vector containing up to three email addresses of the repository.
#' @param fax A character vector containing up to three fax numbers of the repository.
#' @param web_page A character vector containing up to three web pages of the repository.
#' @param user_reference_number A user-defined number or text that the submitter uses to identify 
#' this record. See the Gedcom 5.5.5 Specification for more details.
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
                           local_address_lines = character(),
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
  
  xref <- assign_xref(.pkgenv$xref_prefix_repo, gedcom = gedcom)
  
  if(length(local_address_lines) > 3) local_address_lines <- local_address_lines[1:3]
  
  address <- tidygedcom.internals::ADDRESS_STRUCTURE(local_address_lines = local_address_lines,
                               address_city = city,
                               address_state = state,
                               address_postal_code = postal_code,
                               address_country = country,
                               phone_number = phone_number,
                               address_email = email,
                               address_fax = fax,
                               address_web_page = web_page)
  
  repo_notes <- purrr::map(repository_notes, ~ if(grepl(xref_pattern(), .x)) {
    tidygedcom.internals::NOTE_STRUCTURE(xref_note = .x) 
  } else { 
    tidygedcom.internals::NOTE_STRUCTURE(user_text = .x) 
  }  )
  
  repo_record <- tidygedcom.internals::REPOSITORY_RECORD(xref_repo = xref,
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
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#'
#' @return An updated tidygedcom object excluding the active Repository record.
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) %>% add_repository("text") %>% remove_repository())
remove_repository <- function(gedcom, xref = character()) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_repo, is_repository)
  
  gedcom %>% 
    remove_section(1, "REPO", xref) %>% 
    dplyr::filter(record != xref, value != xref) %>% 
    null_active_record()
}
