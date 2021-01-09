
#' Add a Note record to a tidygedcom object
#'
#' @details This function will automatically assign a unique xref for this record. Most users
#' will only need to use the text parameter (and of course gedcom).
#' 
#' @param gedcom A tidygedcom object.
#' @param text A character string containing the text of the note.
#' @param user_reference_number A user-defined number or text that the submitter uses to identify 
#' this record. See the Gedcom 5.5.5 Specification for more details.
#' @param user_reference_type A user-defined definition of the user_reference_number.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system.
#'
#' @return An updated tidygedcom object including the Note record.
#' @export
add_note <- function(gedcom,
                     text,
                     user_reference_number = character(),
                     user_reference_type = character(),
                     automated_record_id = character()) {
  
  xref <- assign_xref(.pkgenv$xref_prefix_note, gedcom = gedcom)
  
  note_record <- tidygedcom.internals::NOTE_RECORD(xref_note = xref,
                                                   user_text = text,
                                                   user_reference_number = user_reference_number,
                                                   user_reference_type = user_reference_type,
                                                   automated_record_id = automated_record_id)
  
  gedcom %>% 
    tibble::add_row(note_record, .before = nrow(.)) %>% 
    set_active_record(xref)
}

#' Remove a Note record from a tidygedcom object
#'
#' @param gedcom A tidygedcom object.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#'
#' @return An updated tidygedcom object excluding the active Note record.
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) %>% add_note("text") %>% remove_note())
remove_note <- function(gedcom,
                        xref = character()) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_note, is_note)
  
  gedcom %>% 
    dplyr::filter(record != xref, value != xref) %>% 
    null_active_record()
}


#' Consolidate duplicated notes
#'
#' @param gedcom A tidygedcom object.
#' @param min_occurences How many duplicates to prompt creating a new Note record.
#'
#' @return A tidygedcom object with all notes consolidated.
#' @export
consolidate_notes <- function(gedcom, min_occurences = 2) {
  
  note_dupes <- gedcom %>% 
    #get all note structures
    dplyr::filter(level > 0, tag == "NOTE") %>% 
    dplyr::group_by(value) %>% 
    dplyr::filter(dplyr::n() >= min_occurences) %>% 
    dplyr::ungroup() %>% 
    dplyr::pull(value) %>% 
    unique()
  
  for(note in note_dupes) {
    
    existing_notes <- xrefs_notes(gedcom)
    
    # get xrefs of existing note record
    xref <- gedcom %>%
      dplyr::filter(level == 0, tag == "NOTE", value == note) %>% 
      dplyr::pull(record)
    
    # if it doesn't exist create it
    if(length(xref) == 0) {
      gedcom <- gedcom %>% 
        add_note(note)
      
      new_notes <- xrefs_notes(gedcom)
      
      xref <- dplyr::setdiff(new_notes, existing_notes)
    } 
    
    # change notes to references
    gedcom <- gedcom %>% 
      dplyr::mutate(value = dplyr::if_else(level > 0 & tag == "NOTE" & value == note,
                                    xref,
                                    value))
  }
  
  gedcom
}
