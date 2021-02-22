

#' Add a Repository record to a tidyged object
#' 
#' @param gedcom A tidyged object.
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
#' @param user_reference_number A unique user-defined number or text that the submitter 
#' uses to identify this record. You can supply more than one in a vector.
#' @param user_reference_type A user-defined definition of the user_reference_number(s). If this
#' parameter is used, there must be a reference type for every reference number defined.
#' @param repo_notes A character vector of notes accompanying this Repository record.
#' These could be xrefs to existing Note records.
#'
#' @return An updated tidyged object including the Repository record.
#' @export
add_repo <- function(gedcom,
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
                     repo_notes = character()) {
  
  xref <- tidyged.internals::assign_xref_repo(gedcom)
  
  if(length(local_address_lines) > 3) local_address_lines <- local_address_lines[1:3]
  
  address <- tidyged.internals::ADDRESS_STRUCTURE(local_address_lines = local_address_lines,
                                                  address_city = city,
                                                  address_state = state,
                                                  address_postal_code = postal_code,
                                                  address_country = country,
                                                  phone_number = phone_number,
                                                  address_email = email,
                                                  address_fax = fax,
                                                  address_web_page = web_page)
  
  repos_notes <- purrr::map(repo_notes, tidyged.internals::NOTE_STRUCTURE)
  
  repo_record <- tidyged.internals::REPOSITORY_RECORD(xref_repo = xref,
                                                      name_of_repository = name,
                                                      address = address,
                                                      user_reference_number = user_reference_number,
                                                      user_reference_type = user_reference_type,
                                                      notes = repos_notes)
  
  gedcom %>% 
    tibble::add_row(repo_record, .before = nrow(.)) %>% 
    set_active_record(xref)
}

#' Remove a Repository record from a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param repository The xref or name of a Repository record to act on if one is 
#' not activated (will override active record).
#'
#' @return An updated tidyged object excluding the selected Repository record.
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) %>% add_repo("text") %>% remove_repo())
remove_repo <- function(gedcom, repository = character()) {
  
  xref <- get_valid_xref(gedcom, repository, .pkgenv$record_string_repo, is_repo)
  
  gedcom %>% 
    tidyged.internals::remove_section(1, "REPO", xref) %>% 
    dplyr::filter(record != xref, value != xref) %>% 
    null_active_record()
}
