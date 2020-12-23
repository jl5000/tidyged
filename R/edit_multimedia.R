


#' Add a Multimedia record to a tidygedcom object
#'
#' @details This function will automatically assign a unique xref for this record. Most users
#' will only need to use the file_reference, format, source_media, title, and 
#' multimedia_notes parameters (and of course gedcom).
#' 
#' @param gedcom A tidygedcom object.
#' @param file_reference A character vector of references for the files, typically a filepath or URL.
#' @param format A character vector indicating the format of each multimedia file. 
#' Currently limited to one of: "AAC", "AVI", "BMP", "ePub", "FLAC", "GIF", "JPEG", "JPG", 
#' "MKV", "mobi", "MP3", "PCX", "PDF", "PNG", "TIFF", "TIF", "WAV".
#' A format must be defined for each file reference given.
#' @param source_media A code that indicates the type of material in which the referenced 
#' source is stored. Must be one of: audio, book, card, electronic, fiche, film, magazine,
#' manuscript, map, newspaper, photo, tombstone, video. If this is defined, it must be a character
#' vector the same size as file_reference and format.
#' @param title The title of the multimedia file(s). If this is defined, it must be a character
#' vector the same size as file_reference and format.
#' @param user_reference_number A user-defined number or text that the submitter uses to identify 
#' this record. See the Gedcom 5.5.5 Specification for more details.
#' @param user_reference_type A user-defined definition of the user_reference_number.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param multimedia_notes A character vector of notes accompanying this Multimedia record. These could be
#' xrefs to existing Note records.
#'
#' @return An updated tidygedcom object including the Multimedia record.
#' @export
add_multimedia <- function(gedcom,
                           file_reference,
                           format,
                           source_media = character(),
                           title = character(),
                           user_reference_number = character(),
                           user_reference_type = character(),
                           automated_record_id = character(),
                           multimedia_notes = character()) {
  
  xref <- assign_xref(.pkgenv$xref_prefix_obje, gedcom = gedcom)
  
  media_notes <- purrr::map(multimedia_notes, ~ if(grepl(xref_pattern(), .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(user_text = .x) }  )
  
  media_record <- MULTIMEDIA_RECORD(xref_obje = xref,
                                    multimedia_file_reference = file_reference,
                                    multimedia_format = format,
                                    source_media_type = source_media,
                                    descriptive_title = title,
                                    user_reference_number = user_reference_number,
                                    user_reference_type = user_reference_type,
                                    automated_record_id = automated_record_id,
                                    notes = media_notes)
  
  gedcom %>% 
    tibble::add_row(media_record, .before = nrow(.)) %>% 
    set_active_record(xref)
}

#' Remove a Multimedia record from a tidygedcom object
#'
#' @param gedcom A tidygedcom object.
#'
#' @return An updated tidygedcom object excluding the active Multimedia record.
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) %>% 
#'              add_multimedia("test", "BMP") %>% 
#'              remove_multimedia())
remove_multimedia <- function(gedcom) {
  
  check_active_record_valid(gedcom, .pkgenv$record_string_obje, is_multimedia)
  
  gedcom %>% 
    dplyr::filter(record != get_active_record(.), value != get_active_record(.)) %>% 
    null_active_record()
}
