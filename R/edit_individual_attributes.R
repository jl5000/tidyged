

#' Add an attribute associated with an individual
#'
#' @inheritParams add_individual_event
#' 
#' @param attribute_type The type of attribute. This should be automatically populated with the appropriate
#' attribute function.
#' @param attribute_descriptor The value of this attribute.
#' @param fact_classification A descriptive word or phrase used to further classify this 
#' attribute. This should be used whenever the 'other' attribute is used (but can also be used
#' with others).
#'
#' @return An updated tidygedcom object with an expanded Individual record including
#' this attribute.
add_individual_attribute <- function(gedcom,
                                     attribute_type,
                                     attribute_descriptor,
                                     fact_classification = character(),
                                     event_date = date_value(),
                                     event_cause = character(),
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
  
  
  plac_notes <- purrr::map(place_notes, ~ if(grepl(xref_pattern, .x)) {
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
  
  even_notes <- purrr::map(event_notes, ~ if(grepl(xref_pattern, .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  details1 <- EVENT_DETAIL(event_or_fact_classification = fact_classification,
                           date = event_date,
                           place = event_place,
                           address = event_address,
                           responsible_agency = responsible_agency,
                           religious_affiliation = religious_affiliation,
                           cause_of_event = event_cause,
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
rlang::fn_fmls(add_individual_attribute_caste) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "CAST")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_phys_descr <- purrr::partial(add_individual_attribute, attribute_type = "DSCR")
rlang::fn_fmls(add_individual_attribute_phys_descr) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "DSCR")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_education <- purrr::partial(add_individual_attribute, attribute_type = "EDUC")
rlang::fn_fmls(add_individual_attribute_education) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "EDUC")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_national_id <- purrr::partial(add_individual_attribute, attribute_type = "IDNO")
rlang::fn_fmls(add_individual_attribute_national_id) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "IDNO")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_nationality <- purrr::partial(add_individual_attribute, attribute_type = "NATI")
rlang::fn_fmls(add_individual_attribute_nationality) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "NATI")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_num_children <- purrr::partial(add_individual_attribute, attribute_type = "NCHI")
rlang::fn_fmls(add_individual_attribute_num_children) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "NCHI")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_num_marriages <- purrr::partial(add_individual_attribute, attribute_type = "NMR")
rlang::fn_fmls(add_individual_attribute_num_marriages) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "NMR")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_occupation <- purrr::partial(add_individual_attribute, attribute_type = "OCCU")
rlang::fn_fmls(add_individual_attribute_occupation) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "OCCU")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_possessions <- purrr::partial(add_individual_attribute, attribute_type = "PROP")
rlang::fn_fmls(add_individual_attribute_possessions) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "PROP")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_religion <- purrr::partial(add_individual_attribute, attribute_type = "RELI")
rlang::fn_fmls(add_individual_attribute_religion) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "RELI")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_residence <- purrr::partial(add_individual_attribute, attribute_type = "RESI")
rlang::fn_fmls(add_individual_attribute_residence) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "RESI")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_social_sec_num <- purrr::partial(add_individual_attribute, attribute_type = "SSN")
rlang::fn_fmls(add_individual_attribute_social_sec_num) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "SSN")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_nobility_title <- purrr::partial(add_individual_attribute, attribute_type = "TITL")
rlang::fn_fmls(add_individual_attribute_nobility_title) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "TITL")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_other <- purrr::partial(add_individual_attribute, attribute_type = "FACT")
rlang::fn_fmls(add_individual_attribute_other) <- purrr::list_modify(rlang::fn_fmls(add_individual_attribute), 
                                                                     attribute_type = "FACT")

