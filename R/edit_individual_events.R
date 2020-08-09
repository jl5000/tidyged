
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

#' Add an event associated with an individual
#'
#' @name add_individual_event
#' @param gedcom A tidygedcom object.
#' @param event_classification A descriptive word or phrase used to further classify this 
#' event. This should be used whenever the 'other' event is used (but can also be used
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
#' @param place_hierarchy This shows the jurisdictional entities that are named in a sequence 
#' from the lowest to the highest jurisdiction. The jurisdictions are separated by commas, 
#' and any jurisdiction's name that is missing is still accounted for by a comma.
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
#' @export
add_individual_event_birth <- purrr::partial(add_individual_event, event_type = "BIRT")
#' @export
#' @name add_individual_event
#' @rdname add_individual_event_birth
add_individual_event_christening <- purrr::partial(add_individual_event, event_type = "CHR")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_death <- purrr::partial(add_individual_event, event_type = "DEAT")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_burial <- purrr::partial(add_individual_event, event_type = "BURI")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_cremation <- purrr::partial(add_individual_event, event_type = "CREM")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_adoption <- purrr::partial(add_individual_event, event_type = "ADOP")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_baptism <- purrr::partial(add_individual_event, event_type = "BAPM")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_bar_mitzvah <- purrr::partial(add_individual_event, event_type = "BARM")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_bas_mitzvah <- purrr::partial(add_individual_event, event_type = "BASM")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_blessing <- purrr::partial(add_individual_event, event_type = "BLES")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_adult_christening <- purrr::partial(add_individual_event, event_type = "CHRA")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_confirmation <- purrr::partial(add_individual_event, event_type = "CONF")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_first_communion <- purrr::partial(add_individual_event, event_type = "FCOM")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_ordination <- purrr::partial(add_individual_event, event_type = "ORDN")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_naturalization <- purrr::partial(add_individual_event, event_type = "NATU")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_emigration <- purrr::partial(add_individual_event, event_type = "EMIG")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_immigration <- purrr::partial(add_individual_event, event_type = "IMMI")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_census <- purrr::partial(add_individual_event, event_type = "CENS")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_probate <- purrr::partial(add_individual_event, event_type = "PROB")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_will <- purrr::partial(add_individual_event, event_type = "WILL")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_graduation <- purrr::partial(add_individual_event, event_type = "GRAD")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_retirement <- purrr::partial(add_individual_event, event_type = "RETI")
#' @export
#' @rdname add_individual_event_birth
add_individual_event_other <- purrr::partial(add_individual_event, event_type = "EVEN")

