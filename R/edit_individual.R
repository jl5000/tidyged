


#' Add an Individual record to a tidygedcom object
#'
#' @details This function will automatically assign a unique xref for this record. Most users
#' will only need to use the sex, submitters, and individual_notes parameters (and of course gedcom).
#' 
#' If you need to add further information about this individual (e.g. names), use the 
#' add_individual_* functions.
#' 
#' The function will automatically split the individual_notes onto separate lines if the 
#' character limit in the Gedcom standard is exceeded.
#'
#' @param gedcom A tidygedcom object.
#' @param sex The sex of the individual. Either "M" (male), "F" (female), or "U" (undetermined).
#' @param submitters A character vector of submitters of this record. A submitter can either be
#' referenced by an xref or by a regular expression to match to a submitter name.
#' @param aliases A character vector of other Individual records that are aliases of this
#' individual. An individual can either be referenced by an xref or by a regular expression 
#' to match to an individual name.
#' @param submitters_interested_in_ancestors A character vector of submitters interested in 
#' ancestors of this individual. A submitter can either be referenced by an xref or by a 
#' regular expression to match to a submitter name.
#' @param submitters_interested_in_descendants A character vector of submitters interested in 
#' descendants of this individual. A submitter can either be referenced by an xref or by a 
#' regular expression to match to a submitter name.
#' @param permanent_record_file_number The record number that uniquely identifies this record 
#' within a registered network resource. See the Gedcom 5.5.1 Standard for more details.
#' @param ancestral_file_number A unique permanent record number of an individual record 
#' contained in the Family History Department's Ancestral File.
#' @param user_reference_number A user-defined number or text that the submitter uses to identify 
#' this record. See the Gedcom 5.5.1 Standard for more details.
#' @param user_reference_type A user-defined definition of the user_reference_number.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param restriction_notice Only for Ancestral File usage. See the Gedcom 5.5.1 Standard for more 
#' details.
#' @param individual_notes A character vector of notes accompanying this Individual record.
#' These could be xrefs to existing Note records.
#'
#' @return An updated tidygedcom object including the Individual record.
#' 
#' @seealso [add_individual_names()], [add_individual_names_var()], 
#' [add_individual_event()], [add_individual_attribute()], 
#' [add_individual_association()], [add_individual_family_link_as_spouse()],
#' [add_individual_family_link_as_child()]
#' 
#' @export
add_individual <- function(gedcom,
                           sex = character(),
                           submitters = character(),
                           aliases = character(),
                           submitters_interested_in_ancestors = character(),
                           submitters_interested_in_descendants = character(),
                           permanent_record_file_number = character(),
                           ancestral_file_number = character(),
                           user_reference_number = character(),
                           user_reference_type = character(),
                           automated_record_id = character(),
                           restriction_notice = character(),
                           individual_notes = character()) {
  
  xref <- assign_xref(xref_prefix_indi(), gedcom = gedcom)
  
  xrefs_subm <- purrr::map_chr(submitters, find_xref, 
                               gedcom = gedcom, record_xrefs = xrefs_submitters(gedcom), tags = "NAME")
  
  xrefs_alia <- purrr::map_chr(aliases, find_xref, 
                               gedcom = gedcom, record_xrefs = xrefs_individuals(gedcom), 
                               tags = c("NAME", "ROMN", "FONE"))
  
  xrefs_subm_interested_in_ancestors <- 
    purrr::map_chr(submitters_interested_in_ancestors, find_xref, 
                   gedcom = gedcom, record_xrefs = xrefs_submitters(gedcom), tags = "NAME")
  
  xrefs_subm_interested_in_descendants <- 
    purrr::map_chr(submitters_interested_in_descendants, find_xref, 
                   gedcom = gedcom, record_xrefs = xrefs_submitters(gedcom), tags = "NAME")
  
  indi_notes <- purrr::map(individual_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  ind_record <- INDIVIDUAL_RECORD(xref_indi = xref,
                                  restriction_notice = restriction_notice,
                                  sex_value = sex,
                                  xrefs_subm = xrefs_subm,
                                  xrefs_alia = xrefs_alia,
                                  xrefs_subm_interested_in_ancestors = xrefs_subm_interested_in_ancestors,
                                  xrefs_subm_interested_in_descendants = xrefs_subm_interested_in_descendants,
                                  permanent_record_file_number = permanent_record_file_number,
                                  ancestral_file_number = ancestral_file_number,
                                  user_reference_number = user_reference_number,
                                  user_reference_type = user_reference_type,
                                  automated_record_id = automated_record_id,
                                  notes = indi_notes) 
  
  gedcom %>%
    tibble::add_row(ind_record, .before = nrow(.)) %>% 
    set_active_record(xref)
}


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



#' Add an event associated with an individual
#'
#' @param gedcom A tidygedcom object.
#' @param event_type A code signifying the type of event.
#' @param event_classification A descriptive word or phrase used to further classify the 
#' event type. This should be used whenever the EVEN event_type is used (but can also be used
#' with others).
#' @param event_date A date_value() object giving the date of the event.
#' @param age_at_event A character string that indicates the age in years, months, and days 
#' that the individual was at the time of the event. Any combination of these is permitted. 
#' Any labels must come after their corresponding number, for example; "4y 8m 10d".
#' The string can also be "CHILD", "INFANT", or "STILLBORN".
#' @param event_notes A character vector of notes accompanying the event.
#' These could be xrefs to existing Note records.
#' @param place_name The jurisdictional name of the place where the event took place. 
#' Jurisdictions are separated by commas, for example, "Cove, Cache, Utah, USA."
#' @param place_hierarchy 
#' @param place_phonetic_variation A character vector of phonetic variations of the place name.
#' @param phonetic_type A character vector giving the method used in transforming the text to 
#' the corresponding phonetic variation. If this argument is used, it must be the same size
#' as the place_phonetic_variation argument.
#' @param place_romanized_variation A character vector of romanized variations of the place name. 
#' @param romanized_type A character vector giving the method used in transforming the text to 
#' the corresponding romanized variation. If this argument is used, it must be the same size
#' as the place_romanized_variation argument.
#' @param place_latitude The value specifying the latitudinal coordinate of the event place. 
#' The latitude coordinate is the direction North or South from the equator in degrees and 
#' fraction of degrees carried out to give the desired accuracy. 
#' For example: 18 degrees, 9 minutes, and 3.4 seconds North would be formatted as "N18.150944"
#' @param place_longitude The value specifying the longitudinal coordinate of the event place. 
#' The longitude coordinate is Degrees and fraction of degrees east or west of the zero or 
#' base meridian coordinate. For example:
#' 168 degrees, 9 minutes, and 3.4 seconds East would be formatted as "E168.150944". 
#' @param place_notes A character vector of notes accompanying the event place.
#' These could be xrefs to existing Note records.
#' @param address_first_line The first line of the event address.
#' @param city The city of the event address.
#' @param state The state/county of the event address.
#' @param postal_code The postal code of the event address.
#' @param country The country of the event address.
#' @param phone_number A character vector containing up to three phone numbers of the event address.
#' @param email A character vector containing up to three email addresses of the event address.
#' @param fax A character vector containing up to three fax numbers of the event address.
#' @param web_page A character vector containing up to three web pages of the event address.
#' @param responsible_agency The organisation, institution, corporation, person, or other 
#' entity that has responsibility for the event data.
#' @param religious_affiliation A name of the religion with which this event was affiliated.
#' @param cause_of_event Used in special cases to record the reasons which precipitated an event. 
#' Normally this will be used for a death event to show cause of death, such as might be listed 
#' on a death certificate.
#' @param restriction_notice Only for Ancestral File usage. See the Gedcom 5.5.1 Standard for more 
#' details.
#' @param family_xref The xref of the family associated of which this individual is a child.
#' Only used for birth, christening, or adoption events.
#' @param adopting_parent A code which shows which parent in the associated family adopted this 
#' individual. Either "HUSB", "WIFE", or "BOTH".
#'
#' @return An updated tidygedcom object with an expanded Individual record including
#' this event.
add_individual_event <- function(gedcom,
                                 event_type,
                                 event_classification = character(),
                                 event_date = date_value(),
                                 age_at_event = character(),
                                 event_notes = character(),
                                 place_name = character(),
                                 place_hierarchy = character(),
                                 place_phonetic_variation = character(),
                                 phonetic_type = character(),
                                 place_romanized_variation = character(),
                                 romanized_type = character(),
                                 place_latitude = character(),
                                 place_longitude = character(),
                                 place_notes = character(),
                                 address_first_line = character(),
                                 city = character(),
                                 state = character(),
                                 postal_code = character(),
                                 country = character(),
                                 phone_number = character(),
                                 email = character(),
                                 fax = character(),
                                 web_page = character(),
                                 responsible_agency = character(),
                                 religious_affiliation = character(),
                                 cause_of_event = character(),
                                 restriction_notice = character(),
                                 family_xref = character(),
                                 adopting_parent = character()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  address_lines <- c(address_first_line, city, state, postal_code, country)
  
  if(length(address_lines) > 4) address_lines <- address_lines[1:4]
  
  if(length(address_lines) == 0) {
    
    event_address <- ADDRESS_STRUCTURE(character())
    
  } else {
    
    event_address <- ADDRESS_STRUCTURE(all_address_lines = address_lines,
                                       address_city = city,
                                       address_state = state,
                                       address_postal_code = postal_code,
                                       address_country = country,
                                       phone_number = phone_number,
                                       address_email = email,
                                       address_fax = fax,
                                       address_web_page = web_page)
  }
  
  
  plac_notes <- purrr::map(place_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  if(length(place_name) == 0) {
    
    event_place <- PLACE_STRUCTURE(character())
    
  } else {
    
    event_place <- PLACE_STRUCTURE(place_name = place_name,
                                   place_hierarchy = place_hierarchy,
                                   place_phonetic_variation = place_phonetic_variation,
                                   phonetic_type = phonetic_type,
                                   place_romanized_variation = place_romanized_variation,
                                   romanized_type = romanized_type,
                                   place_latitude = place_latitude,
                                   place_longitude = place_longitude,
                                   notes = plac_notes)
  }
  
  even_notes <- purrr::map(event_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  details1 <- EVENT_DETAIL(event_or_fact_classification = event_classification,
                          date = event_date,
                          place = event_place,
                          address = event_address,
                          responsible_agency = responsible_agency,
                          religious_affiliation = religious_affiliation,
                          cause_of_event = cause_of_event,
                          restriction_notice = restriction_notice,
                          notes = even_notes)
  
  details2 <- INDIVIDUAL_EVENT_DETAIL(event_details = details1,
                                      age_at_event = age_at_event)
    
  event_str <- INDIVIDUAL_EVENT_STRUCTURE(event_type_individual = event_type,
                                         individual_event_details = details2,
                                         xref_fam = family_xref,
                                         adopted_by_which_parent = adopting_parent) %>% add_levels(1)
  
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(event_str, .before = next_row) %>% 
    finalise()
  
}

#' @export
#' @rdname add_individual_event
add_individual_event_birth <- purrr::partial(add_individual_event, event_type = "BIRT")
#' @export
#' @rdname add_individual_event
add_individual_event_christening <- purrr::partial(add_individual_event, event_type = "CHR")
#' @export
#' @rdname add_individual_event
add_individual_event_death <- purrr::partial(add_individual_event, event_type = "DEAT")
#' @export
#' @rdname add_individual_event
add_individual_event_burial <- purrr::partial(add_individual_event, event_type = "BURI")
#' @export
#' @rdname add_individual_event
add_individual_event_cremation <- purrr::partial(add_individual_event, event_type = "CREM")
#' @export
#' @rdname add_individual_event
add_individual_event_adoption <- purrr::partial(add_individual_event, event_type = "ADOP")
#' @export
#' @rdname add_individual_event
add_individual_event_baptism <- purrr::partial(add_individual_event, event_type = "BAPM")
#' @export
#' @rdname add_individual_event
add_individual_event_bar_mitzvah <- purrr::partial(add_individual_event, event_type = "BARM")
#' @export
#' @rdname add_individual_event
add_individual_event_bas_mitzvah <- purrr::partial(add_individual_event, event_type = "BASM")
#' @export
#' @rdname add_individual_event
add_individual_event_blessing <- purrr::partial(add_individual_event, event_type = "BLES")
#' @export
#' @rdname add_individual_event
add_individual_event_adult_christening <- purrr::partial(add_individual_event, event_type = "CHRA")
#' @export
#' @rdname add_individual_event
add_individual_event_confirmation <- purrr::partial(add_individual_event, event_type = "CONF")
#' @export
#' @rdname add_individual_event
add_individual_event_first_communion <- purrr::partial(add_individual_event, event_type = "FCOM")
#' @export
#' @rdname add_individual_event
add_individual_event_ordination <- purrr::partial(add_individual_event, event_type = "ORDN")
#' @export
#' @rdname add_individual_event
add_individual_event_naturalization <- purrr::partial(add_individual_event, event_type = "NATU")
#' @export
#' @rdname add_individual_event
add_individual_event_emigration <- purrr::partial(add_individual_event, event_type = "EMIG")
#' @export
#' @rdname add_individual_event
add_individual_event_immigration <- purrr::partial(add_individual_event, event_type = "IMMI")
#' @export
#' @rdname add_individual_event
add_individual_event_census <- purrr::partial(add_individual_event, event_type = "CENS")
#' @export
#' @rdname add_individual_event
add_individual_event_probate <- purrr::partial(add_individual_event, event_type = "PROB")
#' @export
#' @rdname add_individual_event
add_individual_event_will <- purrr::partial(add_individual_event, event_type = "WILL")
#' @export
#' @rdname add_individual_event
add_individual_event_graduation <- purrr::partial(add_individual_event, event_type = "GRAD")
#' @export
#' @rdname add_individual_event
add_individual_event_retirement <- purrr::partial(add_individual_event, event_type = "RETI")
#' @export
#' @rdname add_individual_event
add_individual_event_other <- purrr::partial(add_individual_event, event_type = "EVEN")


#' @export
add_individual_attribute <- function(gedcom,
                                     attribute_type,
                                     attribute_descriptor,
                                     fact_classification = character(),
                                     event_date = date_value(),
                                     age_at_event = character(),
                                     event_notes = character(),
                                     place_name = character(),
                                     place_hierarchy = character(),
                                     place_phonetic_variation = character(),
                                     phonetic_type = character(),
                                     place_romanized_variation = character(),
                                     romanized_type = character(),
                                     place_latitude = character(),
                                     place_longitude = character(),
                                     place_notes = character(),
                                     address_first_line = character(),
                                     city = character(),
                                     state = character(),
                                     postal_code = character(),
                                     country = character(),
                                     phone_number = character(),
                                     email = character(),
                                     fax = character(),
                                     web_page = character(),
                                     responsible_agency = character(),
                                     religious_affiliation = character(),
                                     cause_of_event = character(),
                                     restriction_notice = character()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  address_lines <- c(address_first_line, city, state, postal_code, country)
  
  if(length(address_lines) > 4) address_lines <- address_lines[1:4]
  
  if(length(address_lines) == 0) {
    
    event_address <- ADDRESS_STRUCTURE(character())
    
  } else {
    
    event_address <- ADDRESS_STRUCTURE(all_address_lines = address_lines,
                                       address_city = city,
                                       address_state = state,
                                       address_postal_code = postal_code,
                                       address_country = country,
                                       phone_number = phone_number,
                                       address_email = email,
                                       address_fax = fax,
                                       address_web_page = web_page)
  }
  
  
  plac_notes <- purrr::map(place_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  if(length(place_name) == 0) {
    
    event_place <- PLACE_STRUCTURE(character())
    
  } else {
    
    event_place <- PLACE_STRUCTURE(place_name = place_name,
                                   place_hierarchy = place_hierarchy,
                                   place_phonetic_variation = place_phonetic_variation,
                                   phonetic_type = phonetic_type,
                                   place_romanized_variation = place_romanized_variation,
                                   romanized_type = romanized_type,
                                   place_latitude = place_latitude,
                                   place_longitude = place_longitude,
                                   notes = plac_notes)
  }
  
  even_notes <- purrr::map(event_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  details1 <- EVENT_DETAIL(event_or_fact_classification = fact_classification,
                           date = event_date,
                           place = event_place,
                           address = event_address,
                           responsible_agency = responsible_agency,
                           religious_affiliation = religious_affiliation,
                           cause_of_event = cause_of_event,
                           restriction_notice = restriction_notice,
                           notes = even_notes)
  
  details2 <- INDIVIDUAL_EVENT_DETAIL(event_details = details1,
                                      age_at_event = age_at_event)
  
  attribute_str <- INDIVIDUAL_ATTRIBUTE_STRUCTURE(attribute_type = attribute_type,
                                                  attribute_descriptor = attribute_descriptor,
                                                  individual_event_details = details2) %>% add_levels(1)
  
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(attribute_str, .before = next_row) %>% 
    finalise()
  
  
}


#' @export
#' @rdname add_individual_attribute
add_individual_attribute_caste <- purrr::partial(add_individual_attribute, attribute_type = "CAST")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_phys_descr <- purrr::partial(add_individual_attribute, attribute_type = "DSCR")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_education <- purrr::partial(add_individual_attribute, attribute_type = "EDUC")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_national_id <- purrr::partial(add_individual_attribute, attribute_type = "IDNO")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_nationality <- purrr::partial(add_individual_attribute, attribute_type = "NATI")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_num_children <- purrr::partial(add_individual_attribute, attribute_type = "NCHI")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_num_marriages <- purrr::partial(add_individual_attribute, attribute_type = "NMR")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_occupation <- purrr::partial(add_individual_attribute, attribute_type = "OCCU")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_possessions <- purrr::partial(add_individual_attribute, attribute_type = "PROP")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_religion <- purrr::partial(add_individual_attribute, attribute_type = "RELI")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_residence <- purrr::partial(add_individual_attribute, attribute_type = "RESI")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_social_sec_num <- purrr::partial(add_individual_attribute, attribute_type = "SSN")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_nobility_title <- purrr::partial(add_individual_attribute, attribute_type = "TITL")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_other <- purrr::partial(add_individual_attribute, attribute_type = "FACT")



#' @export
add_individual_association <- function(gedcom,
                                       associated_with,
                                       association,
                                       association_notes = character()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  indi_xref <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), associated_with)
  
  asso_notes <- purrr::map(association_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  asso_str <- ASSOCIATION_STRUCTURE(xref_indi = indi_xref,
                                    relation_is_descriptor = association,
                                    notes = asso_notes) %>% add_levels(1)
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(asso_str, .before = next_row) %>% 
    finalise()
  
}

#' @export
add_individual_family_link_as_spouse <- function(gedcom, 
                                                 family_xref,
                                                 linkage_notes = character()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  link_notes <- purrr::map(linkage_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  link <- SPOUSE_TO_FAMILY_LINK(xref_fam = family_xref, notes = link_notes) %>% add_levels(1)
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(link, .before = next_row) %>% 
    finalise()
}

#' @export
add_individual_family_link_as_child <- function(gedcom, 
                                                family_xref,
                                                linkage_type = character(),
                                                linkage_status = character(),
                                                linkage_notes = character()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  link_notes <- purrr::map(linkage_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  link <- CHILD_TO_FAMILY_LINK(xref_fam = family_xref,
                               pedigree_linkage_type = linkage_type,
                               child_linkage_status = linkage_status,
                               notes = link_notes) %>% add_levels(1)
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(link, .before = next_row) %>% 
    finalise()
}

#' Remove an Individual record from a tidygedcom object
#'
#' @param gedcom A tidygedcom object.
#' @param remove_aliases Whether to also remove the individual records given as aliases of
#' this individual. Defaults to FALSE.
#' @param remove_associations Whether to also remove associations with this individual in 
#' other individual records. Defaults to TRUE.
#'
#' @return An updated tidygedcom object excluding the active Individual record.
#' 
#' @export
remove_individual <- function(gedcom, remove_aliases = FALSE, remove_associations = TRUE) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  #TODO: Need to remove associations and aliases
  gedcom %>% 
    dplyr::filter(record != get_active_record(.), value != get_active_record(.)) %>% 
    null_active_record()
}


