

#' Add a Source record to a tidygedcom object
#'
#' @details This function will automatically assign a unique xref for this record. Most users
#' will only need to use the originator, title, publication_detail, data_notes,
#' and source_notes parameters (and of course gedcom).
#' 
#' The function will automatically split the originator, title, publication_detail, source_text,
#' data_notes and source_notes onto separate lines if the character limit in the Gedcom 
#' standard is exceeded.
#'
#' @param gedcom A tidygedcom object.
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
#' @param user_reference_number A user-defined number or text that the submitter uses to identify 
#' this record. See the Gedcom 5.5.5 Specification for more details.
#' @param user_reference_type A user-defined definition of the user_reference_number.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param data_notes A character vector of notes associated with the data in this Source record.
#' These could be xrefs to existing Note records.
#' @param source_notes A character vector of notes accompanying this Source record.
#' These could be xrefs to existing Note records.
#' @param multimedia_links A character vector of multimedia file references accompanying this
#' source. These could be xrefs to existing Multimedia records.
#'
#' @return An updated tidygedcom object including the Source record.
#' @export
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
                       source_notes = character(),
                       multimedia_links = character()) {
  
  xref <- assign_xref(.pkgenv$xref_prefix_sour, gedcom = gedcom)
  
  dat_notes <- purrr::map(data_notes, ~ if(grepl(xref_pattern(), .x)) {
    tidygedcom.internals::NOTE_STRUCTURE(xref_note = .x) 
  } else { 
    tidygedcom.internals::NOTE_STRUCTURE(user_text = .x) 
  }  )
  
  sour_notes <- purrr::map(source_notes, ~ if(grepl(xref_pattern(), .x)) {
    tidygedcom.internals::NOTE_STRUCTURE(xref_note = .x) 
  } else { 
    tidygedcom.internals::NOTE_STRUCTURE(user_text = .x) 
  }  )
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_multimedia(gedcom), tags = "FILE") %>% 
    purrr::map(tidygedcom.internals::MULTIMEDIA_LINK)
  
  sour_record <- tidygedcom.internals::SOURCE_RECORD(xref_sour = xref,
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
                                                     notes = sour_notes,
                                                     multimedia_links = media_links)
  
    gedcom %>% 
      tibble::add_row(sour_record, .before = nrow(.)) %>% 
      set_active_record(xref)
}


#' Add a source repository citation to a Source record
#'
#' This structure is used within a source record to point to a name and 
#' address record of the holder of the source document.
#' 
#' @param gedcom A tidygedcom object.
#' @param repository A character string identifying the repository. This can either 
#' be an xref or a regular expression to match to a repository name.
#' @param call_number An identification or reference description used to file 
#' and retrieve items from the holdings of a repository.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param update_date_changed Whether to add/update the change date for the record.
#'
#' @return An updated tidygedcom object with an expanded Source record including
#' this repository citation.
#' @export
#' @tests
#' expect_snapshot_value(
#'                  gedcom(subm("Me")) %>% 
#'                  add_repository(name = "The library") %>% 
#'                  add_source() %>% 
#'                  add_source_repository_citation("library") %>%
#'                  remove_dates_for_tests(), "json2")
add_source_repository_citation <- function(gedcom,
                                           repository,
                                           call_number = character(),
                                           xref = character(),
                                           update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_sour, is_source)
  
  repo_xref <- find_xref(gedcom, xrefs_repositories(gedcom), "NAME", repository)
  
  citation <- tidygedcom.internals::SOURCE_REPOSITORY_CITATION(xref_repo = repo_xref,
                                                               source_call_number = call_number) %>% 
    tidygedcom.internals::add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  remove_section(gedcom, 1, "CHAN", "", xrefs = xref)
    citation <- dplyr::bind_rows(citation, tidygedcom.internals::CHANGE_DATE() %>% 
                                   tidygedcom.internals::add_levels(1))
  }
  
  next_row <- find_insertion_point(gedcom, xref, 0, "SOUR")
  
  gedcom %>%
    tibble::add_row(citation, .before = next_row) %>% 
    tidygedcom.internals::finalise()
  
}

#' @tests
#' expect_equal(gedcom(subm("Me")) %>% 
#'                  add_repository(name = "The library") %>% 
#'                  add_source() %>%
#'                  remove_dates_for_tests(),
#'              gedcom(subm("Me")) %>% 
#'                  add_repository(name = "The library") %>% 
#'                  add_source() %>% 
#'                  add_source_repository_citation("library") %>%
#'                  remove_source_repository_citation("library") %>% 
#'                  remove_dates_for_tests())
remove_source_repository_citation <- function(gedcom,
                                              repository,
                                              xref = character()) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_sour, is_source)
  
  repo_xref <- find_xref(gedcom, xrefs_repositories(gedcom), "NAME", repository)
  
  remove_section(gedcom, 1, "REPO", repo_xref, xrefs = xref) %>% 
    activate_source_record(xref)
  
}

#' Remove a Source record from a tidygedcom object
#'
#' @param gedcom A tidygedcom object.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#'
#' @return An updated tidygedcom object excluding the active Source record.
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) %>% add_source(title = "text") %>% remove_source())
remove_source <- function(gedcom,
                          xref = character()) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_sour, is_source)
  
  gedcom %>% 
    remove_section(1, "SOUR", xref) %>% 
    remove_section(2, "SOUR", xref) %>%
    dplyr::filter(record != xref, value != xref) %>% 
    null_active_record()
}
