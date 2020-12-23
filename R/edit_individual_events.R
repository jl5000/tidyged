
#' Add an event associated with an individual
#'
#' @param gedcom A tidygedcom object.
#' 
#' @param event_type The type of event. This should be automatically populated with the appropriate
#' event function.
#' @param event_descriptor Text describing the type of event. This should only be used for "other" events.
#' @param family_xref The xref of the family associated of which this individual is a child.
#' Only used for birth, christening, or adoption events.
#' @param adopting_parent A code which shows which parent in the associated family adopted this 
#' individual. Either "HUSB", "WIFE", or "BOTH".
#' 
#' @param age_at_event A character string that indicates the age in years, months, and days 
#' that the individual was at the time of the event. Any combination of these is permitted. 
#' Any labels must come after their corresponding number, for example; "4y 8m 10d".
#' The string can also be "CHILD", "INFANT", or "STILLBORN".
#' 
#' @param event_classification A descriptive word or phrase used to further classify this 
#' event. This should be used whenever the 'other' event is used (but can also be used
#' with others).
#' @param event_date A date_value() object giving the date of the event.
#' @param place_name The jurisdictional name of the place where the event took place. 
#' Jurisdictions are separated by commas, for example, "Cove, Cache, Utah, USA."
#' @param place_phonetic_variation A character vector of phonetic variations of the place name.
#' @param phonetisation_method A character vector giving the method used in transforming the text to 
#' the corresponding phonetic variation. If this argument is used, it must be the same size
#' as the place_phonetic_variation argument.
#' @param place_romanised_variation A character vector of romanized variations of the place name. 
#' @param romanisation_method A character vector giving the method used in transforming the text to 
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
#' @param local_address_lines The first line of the event address.
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
#' @param event_cause Used in special cases to record the reasons which precipitated an event. 
#' Normally this will be used for a death event to show cause of death, such as might be listed 
#' on a death certificate.
#' @param event_notes A character vector of notes accompanying the event.
#' These could be xrefs to existing Note records.
#' @param multimedia_links A character vector of multimedia file references accompanying this
#' event. These could be xrefs to existing Multimedia records.
#' @param update_date_changed Whether to add/update the change date for the record.
#' @param ... See arguments for main function. The attribute_type/event_type do not need to be populated.
#' @return An updated tidygedcom object with an expanded Individual record including
#' this event.
#' @export
add_individual_event <- function(gedcom,
                                 event_type,
                                 event_descriptor = "",
                                 event_classification = character(),
                                 event_date = date_value(),
                                 event_cause = character(),
                                 age_at_event = character(),
                                 event_notes = character(),
                                 place_name = character(),
                                 place_phonetic_variation = character(),
                                 phonetisation_method = character(),
                                 place_romanised_variation = character(),
                                 romanisation_method = character(),
                                 place_latitude = character(),
                                 place_longitude = character(),
                                 place_notes = character(),
                                 local_address_lines = character(),
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
                                 family_xref = character(),
                                 adopting_parent = character(),
                                 multimedia_links = character(),
                                 update_date_changed = TRUE) {
  
  check_active_record_valid(gedcom, .pkgenv$record_string_indi, is_individual)
  
  if(length(local_address_lines) > 3) local_address_lines <- local_address_lines[1:3]
  
  event_address <- ADDRESS_STRUCTURE(local_address_lines = local_address_lines,
                                     address_city = city,
                                     address_state = state,
                                     address_postal_code = postal_code,
                                     address_country = country,
                                     phone_number = phone_number,
                                     address_email = email,
                                     address_fax = fax,
                                     address_web_page = web_page)
  
  plac_notes <- purrr::map(place_notes, ~ if(grepl(xref_pattern(), .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(user_text = .x) }  )
  
  if(length(place_name) == 0) {
    
    event_place <- PLACE_STRUCTURE(character())
    
  } else {
    
    event_place <- PLACE_STRUCTURE(place_name = place_name,
                                   place_phonetic_variation = place_phonetic_variation,
                                   phonetisation_method = phonetisation_method,
                                   place_romanised_variation = place_romanised_variation,
                                   romanisation_method = romanisation_method,
                                   place_latitude = place_latitude,
                                   place_longitude = place_longitude,
                                   notes = plac_notes)
  }
  
  even_notes <- purrr::map(event_notes, ~ if(grepl(xref_pattern(), .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(user_text = .x) }  )
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_multimedia(gedcom), tags = "FILE") %>% 
    purrr::map(MULTIMEDIA_LINK)
  
  details1 <- EVENT_DETAIL(event_or_fact_classification = event_classification,
                           date = event_date,
                           place = event_place,
                           address = event_address,
                           responsible_agency = responsible_agency,
                           religious_affiliation = religious_affiliation,
                           cause_of_event = event_cause,
                           notes = even_notes,
                           multimedia_links = media_links)
  
  details2 <- INDIVIDUAL_EVENT_DETAIL(event_details = details1,
                                      age_at_event = age_at_event)
  
  event_str <- INDIVIDUAL_EVENT_STRUCTURE(event_type_individual = event_type,
                                          event_descriptor = event_descriptor,
                                          individual_event_details = details2,
                                          xref_fam = family_xref,
                                          adopted_by_which_parent = adopting_parent) %>% add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  remove_section(gedcom, 1, "CHAN", xrefs = get_active_record(gedcom))
    event_str <- dplyr::bind_rows(event_str, CHANGE_DATE())
  }
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(event_str, .before = next_row) %>% 
    finalise()
  
}


#' @export
#' @rdname add_individual_event
add_individual_event_birth <- purrr::partial(add_individual_event, event_type = "BIRT", event_descriptor = "")
# formals(add_individual_event_birth) <- purrr::list_modify(formals(add_individual_event), 
#                                                                   event_type = "BIRT", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_christening <- purrr::partial(add_individual_event, event_type = "CHR", event_descriptor = "")
# formals(add_individual_event_christening) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "CHR", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_death <- purrr::partial(add_individual_event, event_type = "DEAT", event_descriptor = "")
# formals(add_individual_event_death) <- purrr::list_modify(formals(add_individual_event),
#                                                                  event_type = "DEAT", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_burial <- purrr::partial(add_individual_event, event_type = "BURI", event_descriptor = "")
# formals(add_individual_event_burial) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "BURI", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_cremation <- purrr::partial(add_individual_event, event_type = "CREM", event_descriptor = "")
# formals(add_individual_event_cremation) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "CREM", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_adoption <- purrr::partial(add_individual_event, event_type = "ADOP", event_descriptor = "")
# formals(add_individual_event_adoption) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "ADOP", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_baptism <- purrr::partial(add_individual_event, event_type = "BAPM", event_descriptor = "")
# formals(add_individual_event_baptism) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "BAPM", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_bar_mitzvah <- purrr::partial(add_individual_event, event_type = "BARM", event_descriptor = "")
# formals(add_individual_event_bar_mitzvah) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "BARM", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_bas_mitzvah <- purrr::partial(add_individual_event, event_type = "BASM", event_descriptor = "")
# formals(add_individual_event_bas_mitzvah) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "BASM", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_adult_christening <- purrr::partial(add_individual_event, event_type = "CHRA", event_descriptor = "")
# formals(add_individual_event_adult_christening) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "CHRA", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_confirmation <- purrr::partial(add_individual_event, event_type = "CONF", event_descriptor = "")
# formals(add_individual_event_confirmation) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "CONF", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_first_communion <- purrr::partial(add_individual_event, event_type = "FCOM", event_descriptor = "")
# formals(add_individual_event_first_communion) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "FCOM", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_naturalization <- purrr::partial(add_individual_event, event_type = "NATU", event_descriptor = "")
# formals(add_individual_event_naturalization) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "NATU", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_emigration <- purrr::partial(add_individual_event, event_type = "EMIG", event_descriptor = "")
# formals(add_individual_event_emigration) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "EMIG", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_immigration <- purrr::partial(add_individual_event, event_type = "IMMI", event_descriptor = "")
# formals(add_individual_event_immigration) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "IMMI", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_census <- purrr::partial(add_individual_event, event_type = "CENS", event_descriptor = "")
# formals(add_individual_event_census) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "CENS", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_probate <- purrr::partial(add_individual_event, event_type = "PROB", event_descriptor = "")
# formals(add_individual_event_probate) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "PROB", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_will <- purrr::partial(add_individual_event, event_type = "WILL", event_descriptor = "")
# formals(add_individual_event_will) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "WILL", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_graduation <- purrr::partial(add_individual_event, event_type = "GRAD", event_descriptor = "")
# formals(add_individual_event_graduation) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "GRAD", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_retirement <- purrr::partial(add_individual_event, event_type = "RETI", event_descriptor = "")
# formals(add_individual_event_retirement) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "RETI", event_descriptor = "")
#' @export
#' @rdname add_individual_event
add_individual_event_other <- purrr::partial(add_individual_event, event_type = "EVEN")
# formals(add_individual_event_other) <- purrr::list_modify(formals(add_individual_event), 
#                                                                  event_type = "EVEN")

