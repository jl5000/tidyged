

#' Add a personal name (and components) to an Individual record
#' 
#' This function can be applied to an Individual record several times to record
#' personal names.
#'
#' @param gedcom A tidygedcom object.
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
#'
#' @return An updated tidygedcom object with an expanded Individual record including
#' these names.
#' @export
add_individual_names <- function(gedcom,
                                 name,
                                 type = character(),
                                 prefix = character(),
                                 given = character(),
                                 nickname = character(),
                                 surname_prefix = character(),
                                 surname = character(),
                                 suffix = character(),
                                 name_notes = character()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  nam_notes <- purrr::map(name_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  name_pieces <- PERSONAL_NAME_PIECES(name_piece_prefix = prefix,
                                      name_piece_given = given, 
                                      name_piece_nickname = nickname, 
                                      name_piece_surname_prefix = surname_prefix,
                                      name_piece_surname = surname,
                                      name_piece_suffix = suffix,
                                      notes = nam_notes)
  
  name_str <- PERSONAL_NAME_STRUCTURE(name_personal = name,
                                      name_type = type,
                                      name_pieces = name_pieces) %>% add_levels(1)
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(name_str, .before = next_row) %>% 
    finalise()
}

#' Add a variation of a personal name to an Individual record
#' 
#' @param gedcom A tidygedcom object.
#' @param primary_name The name for which this is a variation. This is treated as a
#' regex pattern to match to existing names. 
#' @param variation_name The full name variation.
#' @param type Indicates the method used in transforming the text to the variation.
#' @param phonetic_variation Whether the name variation is a phonetic variation
#' (TRUE, default) or a romanized variation (FALSE).
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
#'
#' @return An updated tidygedcom object with an expanded Individual record including
#' these name variants.
#' @export
add_individual_names_var <- function(gedcom,
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
                                     variation_notes = character()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  name_notes <- purrr::map(variation_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  if(phonetic_variation) {
    
    name_phonetic_var <- variation_name
    phonetic_type <- type
    phon_name_pieces <- list(PERSONAL_NAME_PIECES(name_piece_prefix = prefix,
                                                  name_piece_given = given, 
                                                  name_piece_nickname = nickname, 
                                                  name_piece_surname_prefix = surname_prefix,
                                                  name_piece_surname = surname,
                                                  name_piece_suffix = suffix,
                                                  notes = name_notes))
    name_romanized_var <- character()
    romanized_type <- character()
    rom_name_pieces <- list()
    
  } else {
    
    name_romanized_var <- variation_name
    romanized_type <- type
    rom_name_pieces <- list(PERSONAL_NAME_PIECES(name_piece_prefix = prefix,
                                                 name_piece_given = given, 
                                                 name_piece_nickname = nickname, 
                                                 name_piece_surname_prefix = surname_prefix,
                                                 name_piece_surname = surname,
                                                 name_piece_suffix = suffix,
                                                 notes = name_notes))
    name_phonetic_var <- character()
    phonetic_type <- character()
    phon_name_pieces <- list()
    
  }
  
  name_str <- PERSONAL_NAME_STRUCTURE(name_personal = "line filtered out below",
                                      name_type = character(),
                                      name_pieces = PERSONAL_NAME_PIECES(), 
                                      name_phonetic_variation = name_phonetic_var,
                                      phonetic_type = phonetic_type,
                                      phonetic_name_pieces = phon_name_pieces,
                                      name_romanized_variation = name_romanized_var,
                                      romanized_type = romanized_type,
                                      romanized_name_pieces = rom_name_pieces) %>% 
    dplyr::filter(tag != "NAME") %>%
    add_levels(1)
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 1, "NAME", primary_name)
  
  gedcom %>%
    tibble::add_row(name_str, .before = next_row) %>% 
    finalise()
}


#' Remove a personal name (and components) from an Individual record
#'
#' @param gedcom A tidygedcom object.
#' @param name The personal name to remove.
#'
#' @return An updated tidygedcom object with an Individual record excluding
#' this personal name (and components).
#' @export
remove_individual_name <- function(gedcom,
                                    name) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  remove_section(gedcom, 1, "NAME", name,
                 xrefs = get_active_record(gedcom))
  
}


#' Remove a variation of a personal name from an Individual record
#'
#' @param gedcom A tidygedcom object.
#' @param variation_name The personal name variation to remove.
#' @param phonetic_variation Whether the name variation is a phonetic variation
#' (TRUE, default) or a romanized variation (FALSE).
#'
#' @return An updated tidygedcom object with an Individual record excluding
#' this personal name variation (and components).
#' @export
remove_individual_name_var <- function(gedcom,
                                       variation_name,
                                       phonetic_variation = TRUE) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  remove_section(gedcom, 2, if_else(phonetic_variation, "FONE", "ROMN"), 
                 variation_name, xrefs = get_active_record(gedcom))
  
}
