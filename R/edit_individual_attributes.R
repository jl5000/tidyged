

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
#' @param user_reference_type TODO
#' @param multimedia_links A character vector of multimedia file references accompanying this
#' attribute. These could be xrefs to existing Multimedia records.
#' @param update_date_changed Whether to add/update the change date for the record.
#' @param ... See arguments for main function. The attribute_type/event_type do not need to be populated.
#' @return An updated tidygedcom object with an expanded Individual record including
#' this attribute.
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'                        add_individual(sex = "M") %>% 
#'                        add_individual_attribute(attribute_type = "OCCU",
#'                                                 attribute_descriptor = "Jedi",
#'                                                 local_address_lines = c("line1","line2","line3","line4"),
#'                                                 place_name = "There",
#'                                                 place_notes = "Place note") %>% 
#'                        remove_dates_for_tests(), "json2")
add_individual_attribute <- function(gedcom,
                                     attribute_type,
                                     attribute_descriptor,
                                     fact_classification = character(),
                                     event_date = date_value(),
                                     event_cause = character(),
                                     user_reference_type = character(),
                                     age_at_event = character(),
                                     event_notes = character(),
                                     place_name = character(),
                                     place_phonetic = character(),
                                     phonetisation_method = character(),
                                     place_romanised = character(),
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
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_multimedia(gedcom), tags = "FILE") %>% 
    purrr::map(MULTIMEDIA_LINK)
  
  
  if(length(place_name) == 0) {
    
    event_place <- PLACE_STRUCTURE(character())
    
  } else {
    
    event_place <- PLACE_STRUCTURE(place_name = place_name,
                                   place_phonetic = place_phonetic,
                                   phonetisation_method = phonetisation_method,
                                   place_romanised = place_romanised,
                                   romanisation_method = romanisation_method,
                                   place_latitude = place_latitude,
                                   place_longitude = place_longitude,
                                   notes = plac_notes)
  }
  
  even_notes <- purrr::map(event_notes, ~ if(grepl(xref_pattern(), .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(user_text = .x) }  )
  
  details1 <- EVENT_DETAIL(event_or_fact_classification = fact_classification,
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
  
  attribute_str <- INDIVIDUAL_ATTRIBUTE_STRUCTURE(attribute_type = attribute_type,
                                                  attribute_descriptor = attribute_descriptor,
                                                  individual_event_details = details2,
                                                  user_reference_type = user_reference_type) %>% add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  remove_section(gedcom, 1, "CHAN", "", xrefs = get_active_record(gedcom))
    attribute_str <- dplyr::bind_rows(attribute_str, CHANGE_DATE() %>% add_levels(1))
  }
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(attribute_str, .before = next_row) %>% 
    finalise()
  
  
}


#' @export
#' @rdname add_individual_attribute
add_individual_attribute_caste <- purrr::partial(add_individual_attribute, attribute_type = "CAST")
# formals(add_individual_attribute_caste) <- purrr::list_modify(formals(add_individual_attribute), 
#                                                                      attribute_type = "CAST")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_phys_descr <- purrr::partial(add_individual_attribute, attribute_type = "DSCR")
# formals(add_individual_attribute_phys_descr) <- purrr::list_modify(formals(add_individual_attribute), 
#                                                                      attribute_type = "DSCR")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_education <- purrr::partial(add_individual_attribute, attribute_type = "EDUC")
# formals(add_individual_attribute_education) <- purrr::list_modify(formals(add_individual_attribute), 
#                                                                      attribute_type = "EDUC")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_national_id <- purrr::partial(add_individual_attribute, attribute_type = "IDNO")
# formals(add_individual_attribute_national_id) <- purrr::list_modify(formals(add_individual_attribute), 
#                                                                      attribute_type = "IDNO")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_nationality <- purrr::partial(add_individual_attribute, attribute_type = "NATI")
# formals(add_individual_attribute_nationality) <- purrr::list_modify(formals(add_individual_attribute), 
#                                                                      attribute_type = "NATI")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_num_children <- purrr::partial(add_individual_attribute, attribute_type = "NCHI")
# formals(add_individual_attribute_num_children) <- purrr::list_modify(formals(add_individual_attribute), 
#                                                                      attribute_type = "NCHI")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_num_marriages <- purrr::partial(add_individual_attribute, attribute_type = "NMR")
# formals(add_individual_attribute_num_marriages) <- purrr::list_modify(formals(add_individual_attribute), 
#                                                                      attribute_type = "NMR")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_occupation <- purrr::partial(add_individual_attribute, attribute_type = "OCCU")
# formals(add_individual_attribute_occupation) <- purrr::list_modify(formals(add_individual_attribute), 
#                                                                      attribute_type = "OCCU")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_possessions <- purrr::partial(add_individual_attribute, attribute_type = "PROP")
# formals(add_individual_attribute_possessions) <- purrr::list_modify(formals(add_individual_attribute), 
#                                                                      attribute_type = "PROP")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_religion <- purrr::partial(add_individual_attribute, attribute_type = "RELI")
# formals(add_individual_attribute_religion) <- purrr::list_modify(formals(add_individual_attribute), 
#                                                                      attribute_type = "RELI")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_residence <- purrr::partial(add_individual_attribute, attribute_type = "RESI", attribute_descriptor = "")
# formals(add_individual_attribute_residence) <- purrr::list_modify(formals(add_individual_attribute), 
#                                                                      attribute_type = "RESI", attribute_descriptor = "")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_nobility_title <- purrr::partial(add_individual_attribute, attribute_type = "TITL")
# formals(add_individual_attribute_nobility_title) <- purrr::list_modify(formals(add_individual_attribute),
#                                                                      attribute_type = "TITL")
#' @export
#' @rdname add_individual_attribute
add_individual_attribute_other <- purrr::partial(add_individual_attribute, attribute_type = "FACT")
# formals(add_individual_attribute_other) <- purrr::list_modify(formals(add_individual_attribute), 
#                                                                      attribute_type = "FACT")

