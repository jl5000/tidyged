

#' Add a Source record to a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param events_recorded An enumeration of the different kinds of events that were recorded 
#' in this source. Each enumeration is separated by a comma. See the Gedcom 5.5.5 Specification for 
#' more details. 
#' @param date_period_covered A date_period() object indicating the time period of events
#' covered by this source.
#' @param jurisdiction A character string indicating the lowest level jurisdiction encompassing
#' all places named in this source. See the Gedcom 5.5.5 Specification for more details. 
#' @param responsible_agency The organisation, institution, corporation, person, or other 
#' entity that has responsibility for the source data.
#' @param originator The person, agency, or entity who created the record. 
#' @param title The title of the source work, record, or item and, when appropriate, the title of the 
#' larger work or series of which it is a part.
#' @param short_title An abbreviated or shortened version of the title (if required).
#' @param publication_detail When and where the source record was created. For published works, 
#' this includes information such as the city of publication, name of the publisher, and year of 
#' publication.
#' @param source_text A verbatim copy of relevant text contained within the source. 
#' This indicates notes or text that are actually contained in the source document, 
#' not the submitter's opinion about the source.
#' @param user_reference_numbers A unique user-defined number or text that the submitter 
#' uses to identify this record. You can supply more than one in a vector. You can also define a
#' user reference type by using a named vector (e.g c(id1 = "123A", id2 = "456B")).
#' @param data_notes A character vector of notes associated with the data in this Source record.
#' These could be xrefs to existing Note records.
#' @param sour_notes A character vector of notes accompanying this Source record.
#' These could be xrefs to existing Note records.
#' @param multimedia_links A character vector of Multimedia record xrefs accompanying this 
#' Source record.
#'
#' @return An updated tidyged object including the Source record.
#' @export
add_sour <- function(gedcom,
                     events_recorded = character(),
                     date_period_covered = date_period(),
                     jurisdiction = character(),
                     responsible_agency = character(),
                     originator = character(),
                     title = character(),
                     short_title = character(),
                     publication_detail = character(),
                     source_text = character(),
                     user_reference_numbers = character(),
                     data_notes = character(),
                     sour_notes = character(),
                     multimedia_links = character()) {
  
  xref <- tidyged.internals::assign_xref_sour(gedcom)
  
  dat_notes <- create_note_structures(gedcom, data_notes)
  source_notes <- create_note_structures(gedcom, sour_notes)
  media_links <- create_multimedia_links(gedcom, multimedia_links)
  
  sour_record <- tidyged.internals::SOURCE_RECORD(xref_sour = xref,
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
                                                  user_reference_number = user_reference_numbers,
                                                  notes = source_notes,
                                                  multimedia_links = media_links)
  
  message("Added Source: ", xref)
  
  gedcom |> 
    tibble::add_row(sour_record, .before = nrow(gedcom)) |> 
    set_active_record(xref)
}


#' Add a source repository citation to a Source record
#'
#' This structure is used within a source record to point to a name and 
#' address record of the holder of the source document.
#' 
#' @param gedcom A tidyged object.
#' @param repository A character string identifying the repository xref.
#' @param call_number An identification or reference description used to file 
#' and retrieve items from the holdings of a repository.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param update_date_changed Whether to add/update the change date for the record.
#'
#' @return An updated tidyged object with an expanded Source record including
#' this repository citation.
#' @export
#' @tests
#' expect_snapshot_value(
#'                  gedcom(subm("Me")) |> 
#'                  add_repo(name = "The library") |> 
#'                  add_sour() |> 
#'                  add_sour_repo_citation("@R1@") |>
#'                  remove_dates_for_tests(), "json2")
add_sour_repo_citation <- function(gedcom,
                                   repository,
                                   call_number = character(),
                                   xref = character(),
                                   update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_sour, is_sour)
  
  repo_xref <- repository
  
  citation <- tidyged.internals::SOURCE_REPOSITORY_CITATION(xref_repo = repo_xref,
                                                               source_call_number = call_number) |> 
    tidyged.internals::add_levels(1)
  
  next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, "SOUR")
  
  gedcom <- tibble::add_row(gedcom, citation, .before = next_row)
  
  if(update_date_changed) gedcom <- update_change_date(gedcom, xref)
  
  activate_sour(gedcom, xref)
  
}

#' @tests
#' expect_equal(gedcom(subm("Me")) |> 
#'                  add_repo(name = "The library") |> 
#'                  add_sour() |>
#'                  remove_dates_for_tests(),
#'              gedcom(subm("Me")) |> 
#'                  add_repo(name = "The library") |> 
#'                  add_sour() |> 
#'                  add_sour_repo_citation("@R1@") |>
#'                  remove_sour_repo_citation("@R1@") |> 
#'                  remove_dates_for_tests())
remove_sour_repo_citation <- function(gedcom,
                                      repository) {
  
  xref <- get_valid_xref(gedcom, character(), .pkgenv$record_string_sour, is_sour)
  
  tidyged.internals::remove_section(gedcom, 1, "REPO", repository, xrefs = xref) |> 
    activate_sour(xref)
  
}

#' Remove a Source record from a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param sour The xref or title of a Source record to act on if one is not 
#' activated (will override active record).
#'
#' @return An updated tidyged object excluding the selected Source record.
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) |> add_sour(title = "text") |> remove_sour())
remove_sour <- function(gedcom,
                        sour = character()) {
  
  xref <- get_valid_xref(gedcom, sour, .pkgenv$record_string_sour, is_sour)
  
  gedcom |> 
    tidyged.internals::remove_section(1, "SOUR", xref) |> 
    tidyged.internals::remove_section(2, "SOUR", xref) |>
    dplyr::filter(record != xref, value != xref) |> 
    null_active_record()
}
