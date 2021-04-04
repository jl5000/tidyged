

#' Add a family event to a Family Group record
#'
#' The three-letter codes used for the type parameter are:
#' 
#' ann(ulment), cen(sus), div(orce), dif (divorce filed), eng(agement), mab (marriage banns),
#' mac (marriage contract), mal (marriage license), rel(ationship), mas (marriage settlement),
#' res(idence).
#' 
#' Alternatively eve (for any other event).
#' 
#' Example classifications of non-marriage relationships are:
#' 
#' not married, civil, living together, living apart together. See page 59 of the GEDCOM 
#' Specification for more examples.
#' 
#' @param gedcom A tidyged object.
#' @param type A (case-insensitive) three-letter code giving the type of event. See Details.
#' @param descriptor A short description of an 'other' event.
#' @param classification A descriptive word or phrase used to further classify this 
#' event. This should be used whenever the 'other' event is used (but can also be used
#' with others). Recommended values for non-marriage relationships can be found in Details.
#' @param date A date_calendar(), date_approximated(), date_period(), or date_range() 
#' object giving the timing of the event..
#' @param husband_age A character string that indicates the age in years, months, and days 
#' that the husband was at the time of the event. Any combination of these is permitted. 
#' Any labels must come after their corresponding number, for example; "4y 8m 10d".
#' @param wife_age A character string that indicates the age in years, months, and days 
#' that the wife was at the time of the event. Any combination of these is permitted. 
#' Any labels must come after their corresponding number, for example; "4y 8m 10d".
#' @param cause Used in special cases to record the reasons which precipitated an event. 
#' @param event_place A place() object giving the place associated with this event.
#' @param event_address An address() object giving the address associated with this event.
#' @param notes A character vector of notes accompanying the event. These could be xrefs to 
#' existing Note records.
#' @param responsible_agency The organisation, institution, corporation, person, or other 
#' entity that has responsibility for the event.
#' @param religious_affiliation A name of the religion with which this event was affiliated.
#' @param multimedia_links A character vector of multimedia file references accompanying this
#' event. These could be xrefs to existing Multimedia records.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param update_date_changed Whether to add/update the change date for the record.
#' 
#' @return An updated tidyged object with an expanded Family group record including
#' this event.
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_indi(qn = "Joe Bloggs", sex = "M") %>% 
#'  add_indi(qn = "Jess Bloggs", sex = "F") %>% 
#'  add_indi(qn = "Jessie Bloggs", sex = "F") %>% 
#'  add_famg(husband = "Joe", wife = "@I2@", children = "Jessie") %>% 
#'  add_famg_event(type = "rel", 
#'                 date = date_calendar(year = 1969, month = 1, day = 30),
#'                 event_place = place(name = "Another place")) %>% 
#'  tidyged.internals::remove_dates_for_tests(), "json2")
add_famg_event <- function(gedcom,
                           type,
                           descriptor = "",
                           classification = character(),
                           date = character(),
                           husband_age = character(),
                           wife_age = character(),
                           cause = character(),
                           event_place = place(),
                           event_address = address(),
                           notes = character(),
                           responsible_agency = character(),
                           religious_affiliation = character(),
                           multimedia_links = character(),
                           xref = character(),
                           update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_famg, is_famg)
  
  type <- tolower(stringr::str_sub(type, 1, 3))
  type <- dplyr::case_when(type == "ann" ~ "ANUL",
                           type == "cen" ~ "CENS",
                           type == "div" ~ "DIV",
                           type == "dif" ~ "DIVF",
                           type == "eng" ~ "ENGA",
                           type == "mab" ~ "MARB",
                           type == "mac" ~ "MARC",
                           type == "mal" ~ "MARL",
                           type == "rel" ~ "MARR",
                           type == "mas" ~ "MARS",
                           type == "res" ~ "RESI",
                           type == "eve" ~ "EVEN",
                           TRUE ~ "error")
  
  if(type == "error") stop("type not recognised")
  
  if(type == "EVEN") {
    if(descriptor == "") stop("A descriptor must be given with this type")
  } else {
    descriptor == ""
  }
  
  even_notes <- purrr::map(notes, tidyged.internals::NOTE_STRUCTURE)
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_media(gedcom), tags = "FILE") %>% 
    purrr::map(tidyged.internals::MULTIMEDIA_LINK)
  
  if(type == "MARR" & length(classification) == 0)
    classification <- "marriage"
  
  details1 <- tidyged.internals::EVENT_DETAIL(event_or_fact_classification = classification,
                                              date = date,
                                              place = event_place,
                                              address = event_address,
                                              responsible_agency = responsible_agency,
                                              religious_affiliation = religious_affiliation,
                                              cause_of_event = cause,
                                              notes = even_notes,
                                              multimedia_links = media_links)
  
  details2 <- tidyged.internals::FAMILY_EVENT_DETAIL(husband_age_at_event = husband_age,
                                                     wife_age_at_event = wife_age,
                                                     event_details = details1)
  
  event_str <- tidyged.internals::FAMILY_EVENT_STRUCTURE(event_type_family = type,
                                                         event_descriptor = descriptor,
                                                         family_event_details = details2) %>% 
    tidyged.internals::add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  tidyged.internals::remove_section(gedcom, 1, "CHAN", "", xrefs = xref)
    event_str <- dplyr::bind_rows(event_str, tidyged.internals::CHANGE_DATE() %>% 
                                    tidyged.internals::add_levels(1))
  }
  
  next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, "FAM")

  gedcom %>%
    tibble::add_row(event_str, .before = next_row) %>% 
    tidyged.internals::finalise() %>% 
    activate_famg(xref)
  
}

