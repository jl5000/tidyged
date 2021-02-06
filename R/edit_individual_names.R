

#' Add a personal name (and components) to an Individual record
#' 
#' This function can be applied to an Individual record several times to record
#' personal names.
#'
#' @param gedcom A tidyged object.
#' @param name The full name of the individual. The order of the name parts should 
#' be the order that the person would, by custom of their culture, have used when
#' giving it to a recorder. The surname, if known, should be enclosed between two 
#' forward slash (/) characters. 
#' @param type The name type, e.g. "birth", "aka", "maiden".
#' @param prefix The name prefix, e.g. Cmdr.
#' @param given The given name or earned name. Different given names are separated 
#' by a comma.
#' @param nickname A descriptive or familiar name used in connection with one's 
#' proper name.
#' @param surname_prefix Surname prefix or article used in a family name. 
#' Different surname articles are separated by a comma, for example in the name 
#' "de la Cruz", this value would be "de, la".
#' @param surname Surname or family name. Different surnames are separated by a comma.
#' @param suffix Non-indexing name piece that appears after the given name and surname 
#' parts, e.g. Jr. Different name suffix parts are separated by a comma.
#' @param name_notes A character vector of notes accompanying this name.
#' These could be xrefs to existing Note records.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param update_date_changed Whether to add/update the change date for the record.
#'
#' @return An updated tidyged object with an expanded Individual record including
#' these names.
#' @export
add_indi_names <- function(gedcom,
                           name,
                           type = character(),
                           prefix = character(),
                           given = character(),
                           nickname = character(),
                           surname_prefix = character(),
                           surname = character(),
                           suffix = character(),
                           name_notes = character(),
                           xref = character(),
                           update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  nam_notes <- purrr::map(name_notes, tidyged.internals::NOTE_STRUCTURE)
  
  name_pieces <- tidyged.internals::PERSONAL_NAME_PIECES(name_piece_prefix = prefix,
                                                         name_piece_given = given, 
                                                         name_piece_nickname = nickname, 
                                                         name_piece_surname_prefix = surname_prefix,
                                                         name_piece_surname = surname,
                                                         name_piece_suffix = suffix,
                                                         notes = nam_notes)
  
  name_str <- tidyged.internals::PERSONAL_NAME_STRUCTURE(name_personal = name,
                                                         name_type = type,
                                                         name_pieces = name_pieces) %>% 
    tidyged.internals::add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  remove_section(gedcom, 1, "CHAN", "", xrefs = xref)
    name_str <- dplyr::bind_rows(name_str, tidyged.internals::CHANGE_DATE() %>% 
                                   tidyged.internals::add_levels(1))
  }
  
  next_row <- find_insertion_point(gedcom, xref, 0, "INDI")
  
  gedcom %>%
    tibble::add_row(name_str, .before = next_row) %>% 
    tidyged.internals::finalise() %>% 
    activate_indi(xref)
}

#' Add a variation of a personal name to an Individual record
#' 
#' @param gedcom A tidyged object.
#' @param primary_name The name for which this is a variation. This is treated as a
#' regex pattern to match to existing names. 
#' @param variation_name The full name variation. The surname, if known, should be enclosed between two 
#' forward slash (/) characters. 
#' @param type Indicates the method used in transforming the text to the variation.
#' @param phonetic_variation Whether the name variation is a phonetic variation
#' (TRUE, default) or a romanised variation (FALSE).
#' @param prefix The name prefix, e.g. Cmdr.
#' @param given The given name or earned name. Different given names are separated 
#' by a comma.
#' @param nickname A descriptive or familiar name used in connection with one's 
#' proper name.
#' @param surname_prefix Surname prefix or article used in a family name. 
#' Different surname articles are separated by a comma, for example in the name 
#' "de la Cruz", this value would be "de, la".
#' @param surname Surname or family name. Different surnames are separated by a comma.
#' @param suffix Non-indexing name piece that appears after the given name and surname 
#' parts, e.g. Jr. Different name suffix parts are separated by a comma.
#' @param variation_notes A character vector of notes accompanying this name variation.
#' These could be xrefs to existing Note records.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param update_date_changed Whether to add/update the change date for the record.
#'
#' @return An updated tidyged object with an expanded Individual record including
#' these name variants.
#' @export
#' @tests
#' expect_snapshot_value(
#'                gedcom(subm("Me")) %>% 
#'                add_indi() %>% 
#'                add_indi_names("Joe /Bloggs/") %>% 
#'                add_indi_names_var("Joe Bloggs", "JB", nickname = "JB", type = "tests", 
#'                                          phonetic_variation = FALSE) %>% 
#'                remove_dates_for_tests(), "json2")
add_indi_names_var <- function(gedcom,
                               primary_name,
                               variation_name,
                               type,
                               phonetic_variation = TRUE,
                               prefix = character(),
                               given = character(),
                               nickname = character(),
                               surname_prefix = character(),
                               surname = character(),
                               suffix = character(),
                               variation_notes = character(),
                               xref = character(),
                               update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  name_notes <- purrr::map(variation_notes, tidyged.internals::NOTE_STRUCTURE)
  
  if(phonetic_variation) {
    
    name_phonetic_var <- variation_name
    phonetisation_method <- type
    phon_name_pieces <- list(tidyged.internals::PERSONAL_NAME_PIECES(name_piece_prefix = prefix,
                                                                     name_piece_given = given, 
                                                                     name_piece_nickname = nickname, 
                                                                     name_piece_surname_prefix = surname_prefix,
                                                                     name_piece_surname = surname,
                                                                     name_piece_suffix = suffix,
                                                                     notes = name_notes))
    name_romanised_var <- character()
    romanisation_method <- character()
    rom_name_pieces <- list()
    
  } else {
    
    name_romanised_var <- variation_name
    romanisation_method <- type
    rom_name_pieces <- list(tidyged.internals::PERSONAL_NAME_PIECES(name_piece_prefix = prefix,
                                                                    name_piece_given = given, 
                                                                    name_piece_nickname = nickname, 
                                                                    name_piece_surname_prefix = surname_prefix,
                                                                    name_piece_surname = surname,
                                                                    name_piece_suffix = suffix,
                                                                    notes = name_notes))
    name_phonetic_var <- character()
    phonetisation_method <- character()
    phon_name_pieces <- list()
    
  }
  
  # We don't need the first three arguments of this structure, so we've manipulated
  # the inputs to ensure we can omit them.
  # As we're entering an empty PERSONAL_NAME_PIECES object, it will derive a surname
  # piece from part between forward slashes in name_personal. This then allows us to
  # remove these lines explicitly.
  name_str <- tidyged.internals::PERSONAL_NAME_STRUCTURE(name_personal = "line /filtered out/ below",
                                      name_type = character(),
                                      name_pieces = tidyged.internals::PERSONAL_NAME_PIECES(), 
                                      name_phonetic = name_phonetic_var,
                                      phonetisation_method = phonetisation_method,
                                      phonetic_name_pieces = phon_name_pieces,
                                      name_romanised = name_romanised_var,
                                      romanisation_method = romanisation_method,
                                      romanised_name_pieces = rom_name_pieces) %>% 
    dplyr::filter(tag != "NAME") %>%
    dplyr::filter(!(tag == "SURN" & value == "filtered out")) %>% 
    tidyged.internals::add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  remove_section(gedcom, 1, "CHAN", "", xrefs = xref)
    name_str <- dplyr::bind_rows(name_str, tidyged.internals::CHANGE_DATE() %>% 
                                   tidyged.internals::add_levels(1))
  }
  
  next_row <- gedcom %>% 
    temporarily_remove_name_slashes() %>% 
    find_insertion_point(xref, 1, "NAME", primary_name)
  
  gedcom %>%
    tibble::add_row(name_str, .before = next_row) %>% 
    tidyged.internals::finalise() %>% 
    activate_indi(xref)
}


#' Remove a personal name (and components) from an Individual record
#'
#' @param gedcom A tidyged object.
#' @param name The personal name to remove.
#'
#' @return An updated tidyged object with an Individual record excluding
#' this personal name (and components).
#' @export
#' @tests
#' expect_equal(gedcom(subm()) %>% 
#'                add_indi(),
#'              gedcom(subm()) %>% 
#'                add_indi() %>% 
#'                add_indi_names("Joe Bloggs", given = "Joe", surname = "Bloggs") %>% 
#'                remove_indi_name("Joe Bloggs"))  
remove_indi_name <- function(gedcom,
                                   name) {
  
  xref <- get_valid_xref(gedcom, character(), .pkgenv$record_string_indi, is_indi)
  
  remove_section(gedcom, 1, "NAME", name, xrefs = xref) %>% 
    activate_indi(xref)
  
}


#' Remove a variation of a personal name from an Individual record
#'
#' @param gedcom A tidyged object.
#' @param variation_name The personal name variation to remove.
#' @param phonetic_variation Whether the name variation is a phonetic variation
#' (TRUE, default) or a romanised variation (FALSE).
#'
#' @return An updated tidyged object with an Individual record excluding
#' this personal name variation (and components).
#' @export
#' @tests
#' expect_equal(gedcom(subm()) %>% 
#'                add_indi() %>% 
#'                add_indi_names("Joe Bloggs", given = "Joe", surname = "Bloggs"),
#'              gedcom(subm()) %>% 
#'                add_indi() %>% 
#'                add_indi_names("Joe Bloggs", given = "Joe", surname = "Bloggs") %>% 
#'                add_indi_names_var("Joe Bloggs", "Jo /Blogs/", "spelling error") %>% 
#'                remove_indi_name_var("Jo /Blogs/"))
remove_indi_name_var <- function(gedcom,
                                       variation_name,
                                       phonetic_variation = TRUE) {
  
  xref <- get_valid_xref(gedcom, character(), .pkgenv$record_string_indi, is_indi)
  
  remove_section(gedcom, 2, dplyr::if_else(phonetic_variation, "FONE", "ROMN"), 
                 variation_name, xrefs = xref) %>% 
    activate_indi(xref)
  
}


#' Make an Individual name appear first in the Individual record
#'
#' @param gedcom A tidyged object.
#' @param name The personal name to move.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#'
#' @return An updated tidyged object with the promoted name in the Individual record
#' @export
primary_indi_name <- function(gedcom, name, xref = character()) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  rows_to_move <- identify_section(gedcom, 1, "NAME", name)
  
  section <- gedcom[rows_to_move,]
  gedcom <- gedcom[-rows_to_move,]
  
  next_row <- identify_section(gedcom, 0, "INDI", "", xrefs = xref)[2]

  gedcom %>%
    tibble::add_row(section, .before = next_row) %>% 
    activate_indi(xref)
}
