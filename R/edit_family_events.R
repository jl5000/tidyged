
add_family_event <- function(gedcom,
                             family_xref = character(),
                             event_type = character(),
                             event_subtype = character(),
                             event_descriptor = character(),
                             husband_age_at_event = character(),
                             wife_age_at_event = character(),
                             event_notes = character(),
                             event_date = date_value(),
                             event_cause = character(),
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
  
  check_active_record_valid(gedcom, record_string_fam(), is_family)
  
  
}

#' Add a family event to a family record
#' 
#' @param gedcom 
#' @param family_xref 
#' @param event_subtype 
#' @param event_descriptor 
#' @param husband_age_at_event 
#' @param wife_age_at_event 
#' @param event_date 
#' @param event_cause 
#' @param event_place 
#' @param event_address 
#'
#' @export
add_family_annulment <- function(gedcom,
                                 family_xref = character(),
                                 annulment_type = character(),
                                 husband_age_at_event = character(),
                                 wife_age_at_event = character(),
                                 event_notes = character(),
                                 event_date = date_value(),
                                 event_cause = character(),
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
  
  add_family_event(gedcom = gedcom,
                   family_xref = family_xref,
                   event_type = "ANUL",
                   event_subtype = annulment_type,
                   event_descriptor = character(),
                   husband_age_at_event = husband_age_at_event,
                   wife_age_at_event = wife_age_at_event,
                   event_notes = event_notes,
                   event_date = event_date,
                   event_cause = event_cause,
                   place_name = place_name,
                   place_hierarchy = place_hierarchy,
                   place_phonetic_variation = place_phonetic_variation,
                   phonetic_type = phonetic_type,
                   place_romanized_variation = place_romanized_variation,
                   romanized_type = romanized_type,
                   place_latitude = place_latitude,
                   place_longitude = place_longitude,
                   place_notes = place_notes,
                   address_first_line = address_first_line,
                   city = city,
                   state = state,
                   postal_code = postal_code,
                   country = country,
                   phone_number = phone_number,
                   email = email,
                   fax = fax,
                   web_page = web_page,
                   responsible_agency = responsible_agency,
                   religious_affiliation = religious_affiliation,
                   cause_of_event = cause_of_event,
                   restriction_notice = restriction_notice)
  
}


#' @export
#' @rdname add_family_event_annulment
add_family_event_census <- purrr::partial(add_family_event, event_type = "CENS")
#' @export
#' @rdname add_family_event
add_family_event_divorce <- purrr::partial(add_family_event, event_type = "DIV")
#' @export
#' @rdname add_family_event
add_family_event_divorce_filed <- purrr::partial(add_family_event, event_type = "DIVF")
#' @export
#' @rdname add_family_event
add_family_event_engagement <- purrr::partial(add_family_event, event_type = "ENGA")
#' @export
#' @rdname add_family_event
add_family_event_marriage_banns <- purrr::partial(add_family_event, event_type = "MARB")
#' @export
#' @rdname add_family_event
add_family_event_marriage_contract <- purrr::partial(add_family_event, event_type = "MARC")
#' @export
#' @rdname add_family_event
add_family_event_marriage <- purrr::partial(add_family_event, event_type = "MARR")
#' @export
#' @rdname add_family_event
add_family_event_marriage_license <- purrr::partial(add_family_event, event_type = "MARL")
#' @export
#' @rdname add_family_event
add_family_event_marriage_settlement <- purrr::partial(add_family_event, event_type = "MARS")
#' @export
#' @rdname add_family_event
add_family_event_residence <- purrr::partial(add_family_event, event_type = "RESI")
#' @export
#' @rdname add_family_event
add_family_event_other <- purrr::partial(add_family_event, event_type = "EVEN")

