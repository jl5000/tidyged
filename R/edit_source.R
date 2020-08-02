

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
#' in this source. Each enumeration is separated by a comma. See the Gedcom 5.5.1 Standard for 
#' more details. 
#' @param date_period_covered A date_period() object indicating the time period of events
#' covered by this source.
#' @param jurisdiction A character string indicating the lowest level jurisdiction encompassing
#' all places named in this source. See the Gedcom 5.5.1 Standard for more details. 
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
#' this record. See the Gedcom 5.5.1 Standard for more details.
#' @param user_reference_type A user-defined definition of the user_reference_number.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param data_notes A character vector of notes associated with the data in this Source record.
#' These could be xrefs to existing Note records.
#' @param source_notes A character vector of notes accompanying this Source record.
#' These could be xrefs to existing Note records.
#'
#' @return An updated tidygedcom object including the Source record.
#' 
#' @seealso [add_source_repository_citation()]
#' 
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

#' Remove a Source record from a tidygedcom object
#'
#' @param gedcom A tidygedcom object.
#'
#' @return An updated tidygedcom object excluding the active Source record.
#' @export
remove_source <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_sour(), is_source)
  #TODO: Remove source_citation
  gedcom %>% 
    dplyr::filter(record != get_active_record(.), value != get_active_record(.)) %>% 
    null_active_record()
}
