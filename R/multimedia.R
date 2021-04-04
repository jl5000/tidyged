


#' Add a Multimedia record to a tidyged object
#' 
#' @details
#' The formats allowed are currently limited to:
#' 
#' "AAC", "AVI", "BMP", "ePub", "FLAC", "GIF", "JPEG", 
#' "MKV", "mobi", "MP3", "PCX", "PDF", "PNG", "TIFF", "WAV".
#' 
#' The source media must be one of:
#' 
#' audio, book, card, electronic, fiche, film, magazine,
#' manuscript, map, newspaper, photo, tombstone, video.
#'
#' @param gedcom A tidyged object.
#' @param file_reference A reference for the file, typically a filepath or URL.
#' @param format A string indicating the format of the multimedia file. See Details.
#' @param source_media A code that indicates the type of material in which the referenced 
#' source is stored. See Details.
#' @param title The title of the multimedia file.
#' @param user_reference_number A unique user-defined number or text that the submitter 
#' uses to identify this record. You can supply more than one in a vector.
#' @param user_reference_type A user-defined definition of the user_reference_number(s). If this
#' parameter is used, there must be a reference type for every reference number defined.
#' @param media_notes A character vector of notes accompanying this Multimedia record. These could be
#' xrefs to existing Note records.
#'
#' @return An updated tidyged object including the Multimedia record.
#' @export
add_media <- function(gedcom,
                      file_reference,
                      format,
                      source_media = character(),
                      title = character(),
                      user_reference_number = character(),
                      user_reference_type = character(),
                      media_notes = character()) {
  
  xref <- tidyged.internals::assign_xref_media(gedcom)
  
  mmedia_notes <- purrr::map(media_notes, tidyged.internals::NOTE_STRUCTURE)
  
  media_record <- tidyged.internals::MULTIMEDIA_RECORD(xref_obje = xref,
                                                       multimedia_file_reference = file_reference,
                                                       multimedia_format = format,
                                                       source_media_type = source_media,
                                                       descriptive_title = title,
                                                       user_reference_number = user_reference_number,
                                                       user_reference_type = user_reference_type,
                                                       notes = mmedia_notes)
  
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
#'              add_media("test", "BMP") %>% 
#'              remove_media())
remove_media <- function(gedcom, multimedia = character()) {
  
  xref <- get_valid_xref(gedcom, multimedia, .pkgenv$record_string_obje, is_media)
  
  gedcom %>% 
    dplyr::filter(record != xref, value != xref) %>% 
    null_active_record()
}
