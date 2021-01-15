


#' Add a Multimedia record to a tidyged object
#'
#' @details This function will automatically assign a unique xref for this record. Most users
#' will only need to use the file_reference, format, source_media, title, and 
#' multimedia_notes parameters (and of course gedcom).
#' 
#' @param gedcom A tidyged object.
#' @param file_reference A reference for the file, typically a filepath or URL.
#' @param format A string indicating the format of the multimedia file. 
#' Currently limited to one of: "AAC", "AVI", "BMP", "ePub", "FLAC", "GIF", "JPEG", "JPG", 
#' "MKV", "mobi", "MP3", "PCX", "PDF", "PNG", "TIFF", "TIF", "WAV".
#' @param source_media A code that indicates the type of material in which the referenced 
#' source is stored. Must be one of: audio, book, card, electronic, fiche, film, magazine,
#' manuscript, map, newspaper, photo, tombstone, video.
#' @param title The title of the multimedia file.
#' @param user_reference_number A user-defined number or text that the submitter uses to identify 
#' this record. See the Gedcom 5.5.5 Specification for more details.
#' @param user_reference_type A user-defined definition of the user_reference_number.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param multimedia_notes A character vector of notes accompanying this Multimedia record. These could be
#' xrefs to existing Note records.
#'
#' @return An updated tidyged object including the Multimedia record.
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
  
  media_notes <- purrr::map(multimedia_notes, tidyged.internals::NOTE_STRUCTURE)
  
  media_record <- tidyged.internals::MULTIMEDIA_RECORD(xref_obje = xref,
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

#' Remove a Multimedia record from a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param multimedia The xref or file reference of a Multimedia record to act 
#' on if one is not activated (will override active record).
#'
#' @return An updated tidyged object excluding the selected Multimedia record.
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) %>% 
#'              add_multimedia("test", "BMP") %>% 
#'              remove_multimedia())
remove_multimedia <- function(gedcom, multimedia = character()) {
  
  xref <- get_valid_xref(gedcom, multimedia, .pkgenv$record_string_obje, is_multimedia)
  
  gedcom %>% 
    dplyr::filter(record != xref, value != xref) %>% 
    null_active_record()
}
