

#' Add a Repository record to a tidyged object
#' 
#' @param gedcom A tidyged object.
#' @param name The name of the repository.
#' @param repo_address An address() object giving the address of the repository.
#' @param user_reference_numbers A unique user-defined number or text that the submitter 
#' uses to identify this record. You can supply more than one in a vector. You can also define a
#' user reference type by using a named vector (e.g c(id1 = "123A", id2 = "456B")).
#' @param repo_notes A character vector of notes accompanying this Repository record.
#' These could be xrefs to existing Note records.
#'
#' @return An updated tidyged object including the Repository record.
#' @export
add_repo <- function(gedcom,
                     name,
                     repo_address = address(),
                     user_reference_numbers = character(),
                     repo_notes = character()) {
  
  xref <- tidyged.internals::assign_xref_repo(gedcom)
  
  repos_notes <- create_note_structures(gedcom, repo_notes)
  
  repo_record <- tidyged.internals::REPOSITORY_RECORD(xref_repo = xref,
                                                      name_of_repository = name,
                                                      address = repo_address,
                                                      user_reference_number = user_reference_numbers,
                                                      notes = repos_notes)
  
  gedcom |> 
    tibble::add_row(repo_record, .before = nrow(gedcom)) |> 
    set_active_record(xref)
}

#' Remove a Repository record from a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param repository The xref of a Repository record to act on if one is 
#' not activated (will override active record).
#'
#' @return An updated tidyged object excluding the selected Repository record.
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) |> add_repo("text") |> remove_repo())
remove_repo <- function(gedcom, repository = character()) {
  
  xref <- get_valid_xref(gedcom, repository, .pkgenv$record_string_repo, is_repo)
  
  gedcom |> 
    tidyged.internals::remove_section(1, "REPO", xref) |> 
    dplyr::filter(record != xref, value != xref) |> 
    null_active_record()
}
