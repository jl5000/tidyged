
#' Add a Note record to a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param text A character string containing the text of the note.
#' @param user_reference_number A unique user-defined number or text that the submitter 
#' uses to identify this record. You can supply more than one in a vector.
#' @param user_reference_type A user-defined definition of the user_reference_number(s). If this
#' parameter is used, there must be a reference type for every reference number defined.
#'
#' @return An updated tidyged object including the Note record.
#' @export
add_note <- function(gedcom,
                     text,
                     user_reference_number = character(),
                     user_reference_type = character()) {
  
  xref <- tidyged.internals::assign_xref_note(gedcom)
  
  note_record <- tidyged.internals::NOTE_RECORD(xref_note = xref,
                                                   user_text = text,
                                                   user_reference_number = user_reference_number,
                                                   user_reference_type = user_reference_type)
  
  gedcom %>% 
    tibble::add_row(note_record, .before = nrow(.)) %>% 
    set_active_record(xref)
}

#' Remove a Note record from a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param note The xref or excerpt of a Note record to act on if one is 
#' not activated (will override active record).
#'
#' @return An updated tidyged object excluding the selected Note record.
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) %>% add_note("text") %>% remove_note())
remove_note <- function(gedcom,
                        note = character()) {
  
  xref <- get_valid_xref(gedcom, note, .pkgenv$record_string_note, is_note)
  
  gedcom %>% 
    dplyr::filter(record != xref, value != xref) %>% 
    null_active_record()
}

