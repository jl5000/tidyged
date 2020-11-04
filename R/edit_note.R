
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
  
  note_record <- NOTE_RECORD(xref_note = xref,
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
#'
#' @return An updated tidygedcom object excluding the active Note record.
#' @export
remove_note <- function(gedcom) {
  
  check_active_record_valid(gedcom, .pkgenv$record_string_note, is_note)
  
  gedcom %>% 
    dplyr::filter(record != get_active_record(.), value != get_active_record(.)) %>% 
    null_active_record()
}
