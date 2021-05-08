

#' Add a personal name (and components) to an Individual record
#' 
#' This function can be applied to an Individual record several times to record
#' personal names. The personal name is constructed automatically from the components.
#'
#' @param gedcom A tidyged object.
#' @param names A name_pieces() object defining the components of the personal name.
#' @param type The name type, e.g. "birth", "aka", "maiden".
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param update_date_changed Whether to add/update the change date for the record.
#'
#' @return An updated tidyged object with an expanded Individual record including
#' these names.
#' @export
add_indi_names <- function(gedcom,
                           names = name_pieces(),
                           type = character(),
                           xref = character(),
                           update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  given <- dplyr::filter(names, tag == "GIVN")$value
  surname_prefix <- dplyr::filter(names, tag == "SPFX")$value
  surname <- dplyr::filter(names, tag == "SURN")$value
  suffix <- dplyr::filter(names, tag == "NSFX")$value
  
  name <- tidyged.internals::construct_full_name(given, surname_prefix, surname, suffix)
  
  name_str <- tidyged.internals::PERSONAL_NAME_STRUCTURE(name_personal = name,
                                                         name_type = type,
                                                         name_pieces = names) %>% 
    tidyged.internals::add_levels(1)
  
  next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, "INDI")
  
  gedcom <- tibble::add_row(gedcom, name_str, .before = next_row)
  
  if(update_date_changed) gedcom <- update_change_date(gedcom, xref)
  
  activate_indi(gedcom, xref)
}

#' Add a variation of a personal name to an Individual record
#' 
#' @param gedcom A tidyged object.
#' @param primary_name The name for which this is a variation. This must be given exactly 
#' (without forward slashes). 
#' @param method Indicates the method used in transforming the text to the variation.
#' @param names_var A name_pieces() object defining the components of the name variation.
#' @param phonetic_variation Whether the name variation is a phonetic variation
#' (TRUE, default) or a romanised variation (FALSE).
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
#'                add_indi_names(name_pieces(given = "Joe", surname = "Bloggs")) %>% 
#'                add_indi_names_var("Joe Bloggs", names_var = name_pieces(given = "JB"), 
#'                                   method = "tests", phonetic_variation = FALSE) %>% 
#'                tidyged.internals::remove_dates_for_tests(), "json2")
add_indi_names_var <- function(gedcom,
                               primary_name,
                               method,
                               names_var = name_pieces(),
                               phonetic_variation = TRUE,
                               xref = character(),
                               update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  given <- dplyr::filter(names_var, tag == "GIVN")$value
  surname_prefix <- dplyr::filter(names_var, tag == "SPFX")$value
  surname <- dplyr::filter(names_var, tag == "SURN")$value
  suffix <- dplyr::filter(names_var, tag == "NSFX")$value
  
  if(phonetic_variation) {
    
    name_phonetic_var <- tidyged.internals::construct_full_name(given, surname_prefix, surname, suffix)
    phonetisation_method <- method
    phon_name_pieces <- list(names_var)
    name_romanised_var <- character()
    romanisation_method <- character()
    rom_name_pieces <- list()
    
  } else {
    
    name_romanised_var <- tidyged.internals::construct_full_name(given, surname_prefix, surname, suffix)
    romanisation_method <- method
    rom_name_pieces <- list(names_var)
    name_phonetic_var <- character()
    phonetisation_method <- character()
    phon_name_pieces <- list()
    
  }
  
  # We don't need the first three arguments of this structure, so we've manipulated
  # the inputs to ensure we can omit them.
  # As we're entering an empty PERSONAL_NAME_PIECES object, it will derive a surname
  # piece from part between forward slashes in name_personal. This then allows us to
  # remove these lines explicitly.
  name_str <- tidyged.internals::PERSONAL_NAME_STRUCTURE(name_personal = "line filtered out below",
                                      name_type = character(),
                                      name_pieces = name_pieces(surname = "filtered out"), 
                                      name_phonetic = name_phonetic_var,
                                      phonetisation_method = phonetisation_method,
                                      phonetic_name_pieces = phon_name_pieces,
                                      name_romanised = name_romanised_var,
                                      romanisation_method = romanisation_method,
                                      romanised_name_pieces = rom_name_pieces) %>% 
    dplyr::filter(tag != "NAME") %>%
    dplyr::filter(!(tag == "SURN" & value == "filtered out")) %>% 
    tidyged.internals::add_levels(1)
  
  next_row <- gedcom %>% 
    temporarily_remove_name_slashes() %>% 
    tidyged.internals::find_insertion_point(xref, 1, "NAME", primary_name)
  
  gedcom <- tibble::add_row(gedcom, name_str, .before = next_row)
  
  if(update_date_changed) gedcom <- update_change_date(gedcom, xref)
  
  activate_indi(gedcom, xref)
  
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
#'                add_indi_names(name_pieces(given = "Joe", surname = "Bloggs")) %>% 
#'                remove_indi_name("Joe Bloggs"))  
remove_indi_name <- function(gedcom,
                              name) {
  
  xref <- get_valid_xref(gedcom, character(), .pkgenv$record_string_indi, is_indi)
  
  rows_to_remove <- temporarily_remove_name_slashes(gedcom) %>% 
    tidyged.internals::identify_section(1, "NAME", name, xrefs = xref)
  
  gedcom %>%
    dplyr::slice(-rows_to_remove) %>% 
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
#'                add_indi_names(name_pieces(given = "Joe", surname = "Bloggs")),
#'              gedcom(subm()) %>% 
#'                add_indi() %>% 
#'                add_indi_names(name_pieces(given = "Joe", surname = "Bloggs")) %>% 
#'                add_indi_names_var("Joe Bloggs", names_var = name_pieces(given = "Jo", surname = "Blogs"), 
#'                                   method = "spelling error") %>% 
#'                remove_indi_name_var("Jo Blogs"))
remove_indi_name_var <- function(gedcom,
                                       variation_name,
                                       phonetic_variation = TRUE) {
  
  xref <- get_valid_xref(gedcom, character(), .pkgenv$record_string_indi, is_indi)
  
  rows_to_remove <- temporarily_remove_name_slashes(gedcom) %>% 
    tidyged.internals::identify_section(2, dplyr::if_else(phonetic_variation, "FONE", "ROMN"), 
                     variation_name, xrefs = xref)
  
  gedcom %>%
    dplyr::slice(-rows_to_remove) %>% 
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
  
  rows_to_move <- temporarily_remove_name_slashes(gedcom) %>% 
    tidyged.internals::identify_section(1, "NAME", name)
  
  section <- gedcom[rows_to_move,]
  gedcom <- gedcom[-rows_to_move,]
  
  next_row <- tidyged.internals::identify_section(gedcom, 0, "INDI", "", xrefs = xref)[2]

  gedcom %>%
    tibble::add_row(section, .before = next_row) %>% 
    activate_indi(xref)
}
