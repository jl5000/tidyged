

#' Add a family event to a Family Group record
#'
#' @inheritParams add_indi_event
#' @param event_descriptor A short description of the event.
#' 
#' @param husband_age_at_event A character string that indicates the age in years, months, and days 
#' that the husband was at the time of the event. Any combination of these is permitted. 
#' Any labels must come after their corresponding number, for example; "4y 8m 10d".
#' @param wife_age_at_event A character string that indicates the age in years, months, and days 
#' that the wife was at the time of the event. Any combination of these is permitted. 
#' Any labels must come after their corresponding number, for example; "4y 8m 10d".
#' @param update_date_changed Whether to add/update the change date for the record.
#' @param ... See arguments for main function. The attribute_type/event_type do not need to be populated.
#' 
#' @return An updated tidyged object with an expanded Family group record including
#' this event.
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_indi("Joe /Bloggs/", sex = "M") %>% 
#'  add_indi("Jess /Bloggs/", sex = "F") %>% 
#'  add_indi("Jessie /Bloggs/", sex = "F") %>% 
#'  add_famg(husband = "Joe", wife = "Jess Bloggs", children = "Jessie") %>% 
#'  add_famg_event(event_type = "MARR", 
#'                   event_date = date_calendar(year = 1969, month = 1, day = 30),
#'                   place_name = "Another place") %>% 
#'  remove_dates_for_tests(), "json2")
add_famg_event <- function(gedcom,
                           event_type = character(),
                           event_descriptor = character(),
                           event_classification = character(),
                           husband_age_at_event = character(),
                           wife_age_at_event = character(),
                           event_notes = character(),
                           event_date = character(),
                           event_cause = character(),
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
                           xref = character(),
                           update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_famg, is_famg)
  
  if(length(local_address_lines) > 3) local_address_lines <- local_address_lines[1:3]
  
  event_address <- tidyged.internals::ADDRESS_STRUCTURE(local_address_lines = local_address_lines,
                                                           address_city = city,
                                                           address_state = state,
                                                           address_postal_code = postal_code,
                                                           address_country = country,
                                                           phone_number = phone_number,
                                                           address_email = email,
                                                           address_fax = fax,
                                                           address_web_page = web_page)
  
  plac_notes <- purrr::map(place_notes, tidyged.internals::NOTE_STRUCTURE)
  
  if(length(place_name) == 0) {
    
    event_place <- tidyged.internals::PLACE_STRUCTURE(character())
    
  } else {
    
    event_place <- tidyged.internals::PLACE_STRUCTURE(place_name = place_name,
                                                         place_phonetic = place_phonetic,
                                                         phonetisation_method = phonetisation_method,
                                                         place_romanised = place_romanised,
                                                         romanisation_method = romanisation_method,
                                                         place_latitude = place_latitude,
                                                         place_longitude = place_longitude,
                                                         notes = plac_notes)
  }
  
  even_notes <- purrr::map(event_notes, tidyged.internals::NOTE_STRUCTURE)
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_multimedia(gedcom), tags = "FILE") %>% 
    purrr::map(tidyged.internals::MULTIMEDIA_LINK)
  
  if(event_type == "MARR" & length(event_classification) == 0)
    event_classification <- "marriage"
  
  details1 <- tidyged.internals::EVENT_DETAIL(event_or_fact_classification = event_classification,
                                                 date = event_date,
                                                 place = event_place,
                                                 address = event_address,
                                                 responsible_agency = responsible_agency,
                                                 religious_affiliation = religious_affiliation,
                                                 cause_of_event = event_cause,
                                                 notes = even_notes,
                                                 multimedia_links = media_links)
  
  details2 <- tidyged.internals::FAMILY_EVENT_DETAIL(husband_age_at_event = husband_age_at_event,
                                                        wife_age_at_event = wife_age_at_event,
                                                        event_details = details1)
  
  event_str <- tidyged.internals::FAMILY_EVENT_STRUCTURE(event_type_family = event_type,
                                                            event_descriptor = event_descriptor,
                                                            family_event_details = details2) %>% 
    tidyged.internals::add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  remove_section(gedcom, 1, "CHAN", "", xrefs = xref)
    event_str <- dplyr::bind_rows(event_str, tidyged.internals::CHANGE_DATE() %>% 
                                    tidyged.internals::add_levels(1))
  }
  
  next_row <- find_insertion_point(gedcom, xref, 0, "FAM")

  gedcom %>%
    tibble::add_row(event_str, .before = next_row) %>% 
    tidyged.internals::finalise() %>% 
    activate_famg(xref)
  
}


#' @rdname add_famg_event
#' @export
add_famg_event_annulment <- purrr::partial(add_famg_event, event_type = "ANUL", event_descriptor = "")
# formals(add_famg_event_annulment) <- purrr::list_modify(formals(add_famg_event), 
#                                                               event_type = "ANUL", event_descriptor = "")
#' @rdname add_famg_event
#' @export
add_famg_event_census <- purrr::partial(add_famg_event, event_type = "CENS", event_descriptor = "")
# formals(add_famg_event_census) <- purrr::list_modify(formals(add_famg_event), 
#                                                               event_type = "CENS", event_descriptor = "")
#' @export
#' @rdname add_famg_event
add_famg_event_divorce <- purrr::partial(add_famg_event, event_type = "DIV", event_descriptor = "")
# formals(add_famg_event_divorce) <- purrr::list_modify(formals(add_famg_event), 
#                                                               event_type = "DIV", event_descriptor = "")
#' @export
#' @rdname add_famg_event
add_famg_event_divorce_filed <- purrr::partial(add_famg_event, event_type = "DIVF", event_descriptor = "")
# formals(add_famg_event_divorce_filed) <- purrr::list_modify(formals(add_famg_event), 
#                                                               event_type = "DIVF", event_descriptor = "")
#' @export
#' @rdname add_famg_event
add_famg_event_engagement <- purrr::partial(add_famg_event, event_type = "ENGA", event_descriptor = "")
# formals(add_famg_event_engagement) <- purrr::list_modify(formals(add_famg_event), 
#                                                               event_type = "ENGA", event_descriptor = "")
#' @export
#' @rdname add_famg_event
add_famg_event_marriage_banns <- purrr::partial(add_famg_event, event_type = "MARB", event_descriptor = "")
# formals(add_famg_event_marriage_banns) <- purrr::list_modify(formals(add_famg_event), 
#                                                               event_type = "MARB", event_descriptor = "")
#' @export
#' @rdname add_famg_event
add_famg_event_marriage_contract <- purrr::partial(add_famg_event, event_type = "MARC", event_descriptor = "")
# formals(add_famg_event_marriage_contract) <- purrr::list_modify(formals(add_famg_event), 
#                                                               event_type = "MARC", event_descriptor = "")
#' @export
#' @rdname add_famg_event
add_famg_event_relationship <- purrr::partial(add_famg_event, event_type = "MARR", event_descriptor = "")
# formals(add_famg_event_relationship) <- purrr::list_modify(formals(add_famg_event), 
#                                                               event_type = "MARR", event_descriptor = "")
#' @export
#' @rdname add_famg_event
add_famg_event_marriage_license <- purrr::partial(add_famg_event, event_type = "MARL", event_descriptor = "")
# formals(add_famg_event_marriage_license) <- purrr::list_modify(formals(add_famg_event), 
#                                                               event_type = "MARL", event_descriptor = "")
#' @export
#' @rdname add_famg_event
add_famg_event_marriage_settlement <- purrr::partial(add_famg_event, event_type = "MARS", event_descriptor = "")
# formals(add_famg_event_marriage_settlement) <- purrr::list_modify(formals(add_famg_event), 
#                                                               event_type = "MARS", event_descriptor = "")
#' @export
#' @rdname add_famg_event
add_famg_event_residence <- purrr::partial(add_famg_event, event_type = "RESI", event_descriptor = "")
# formals(add_famg_event_residence) <- purrr::list_modify(formals(add_famg_event), 
#                                                               event_type = "RESI", event_descriptor = "")
#' @export
#' @rdname add_famg_event
add_famg_event_other <- purrr::partial(add_famg_event, event_type = "EVEN")
# formals(add_famg_event_other) <- purrr::list_modify(formals(add_famg_event), 
#                                                               event_type = "EVEN")

