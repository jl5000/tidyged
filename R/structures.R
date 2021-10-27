
#' Define a place associated with a fact
#'
#' @param name The jurisdictional name of the place. 
#' Jurisdictions are separated by commas, for example, "Cove, Cache, Utah, USA."
#' @param phonetic_var A named character vector of phonetic variations of the place name. Element names
#' must give the phonetisation method used in transforming the text to the corresponding phonetic variation.
#' i.e. c(method1 = "var1", method2 = "var2")
#' @param romanised_var A named character vector of romanised variations of the place name. Element names
#' must give the romanisation method used in transforming the text to the corresponding romanised variation.
#' i.e. c(method1 = "var1", method2 = "var2")
#' @param latitude The value specifying the latitudinal coordinate of the place. 
#' The latitude coordinate is the direction North or South from the equator in degrees and 
#' fraction of degrees carried out to give the desired accuracy. 
#' For example: 18 degrees, 9 minutes, and 3.4 seconds North would be formatted as "N18.150944"
#' @param longitude The value specifying the longitudinal coordinate of the place. 
#' The longitude coordinate is Degrees and fraction of degrees east or west of the zero or 
#' base meridian coordinate. For example:
#' 168 degrees, 9 minutes, and 3.4 seconds East would be formatted as "E168.150944". 
#' @param notes A character vector of notes accompanying the place.
#' These could be xrefs to existing Note records.
#'
#' @return A tibble describing a place.
#' @export
#' @tests
#' expect_equal(place(), tibble::tibble())
#' expect_snapshot_value(place("A place"), "json2")
place <- function(name = character(),
                  phonetic_var = character(),
                  romanised_var = character(),
                  latitude = character(),
                  longitude = character(),
                  notes = character()) {
  
  plac_notes <- purrr::map(notes, tidyged.internals::NOTE_STRUCTURE)
  
  if(length(name) == 0) {
    
    tidyged.internals::PLACE_STRUCTURE(character())
    
  } else {
    
    if(length(phonetic_var) == 0) {
      phonetisation_method <- character()
    } else {
      if(is.null(names(phonetic_var))) stop("Elements in the phonetic_var vector must be named")
      if(any(names(phonetic_var)=="")) stop("Every element in the phonetic_var vector must be named")
      phonetisation_method <- names(phonetic_var)
    }
    
    if(length(romanised_var) == 0) {
      romanisation_method <- character()
    } else {
      if(is.null(names(romanised_var))) stop("Elements in the romanised_var vector must be named")
      if(any(names(romanised_var)=="")) stop("Every element in the romanised_var vector must be named")
      romanisation_method <- names(romanised_var)
    }
    
    tidyged.internals::PLACE_STRUCTURE(place_name = name,
                                       place_phonetic = phonetic_var,
                                       phonetisation_method = phonetisation_method,
                                       place_romanised = romanised_var,
                                       romanisation_method = romanisation_method,
                                       place_latitude = latitude,
                                       place_longitude = longitude,
                                       notes = plac_notes)
  }
  
  
}


#' Define an address
#'
#' @param local_address_lines A character vector containing up to three local address lines.
#' @param city The city of the address.
#' @param state The state/county of the address.
#' @param postal_code The postal code of the address.
#' @param country The country of the address.
#' @param phone_number A character vector containing up to three phone numbers.
#' @param email A character vector containing up to three email addresses.
#' @param fax A character vector containing up to three fax numbers.
#' @param web_page A character vector containing up to three web pages.
#'
#' @return A tibble describing an address.
#' @export
#' @tests
#' expect_equal(address(), tibble::tibble())
#' expect_snapshot_value(address("A place"), "json2")
address <- function(local_address_lines = character(),
                    city = character(),
                    state = character(),
                    postal_code = character(),
                    country = character(),
                    phone_number = character(),
                    email = character(),
                    fax = character(),
                    web_page = character()) {
  
  if(length(local_address_lines) > 3) local_address_lines <- local_address_lines[1:3]
  if(length(phone_number) > 3) phone_number <- phone_number[1:3]
  if(length(email) > 3) email <- email[1:3]
  if(length(fax) > 3) fax <- fax[1:3]
  if(length(web_page) > 3) web_page <- web_page[1:3]
  
  tidyged.internals::ADDRESS_STRUCTURE(local_address_lines = local_address_lines,
                                       address_city = city,
                                       address_state = state,
                                       address_postal_code = postal_code,
                                       address_country = country,
                                       phone_number = phone_number,
                                       address_email = email,
                                       address_fax = fax,
                                       address_web_page = web_page)
  
  
}

#' Define a personal name's components
#' 
#' @param prefix The name prefix, e.g. Cmdr.
#' @param given The given name or earned name. Different given names are separated 
#' by a comma.
#' @param nickname A descriptive or familiar name used in connection with one's 
#' proper name.
#' @param surname_prefix Surname prefix or article used in a family name. 
#' For example in the name "de la Cruz", this value would be "de la".
#' @param surname Surname or family name. Different surnames are separated by a comma.
#' @param suffix Non-indexing name piece that appears after the given name and surname 
#' parts, e.g. Jr. Different name suffix parts are separated by a comma.
#' @param notes A character vector of notes accompanying this name.
#' These could be xrefs to existing Note records.
#'
#' @return A tibble describing a personal name's components.
#' @export
name_pieces <- function(prefix = character(),
                        given = character(), 
                        nickname = character(), 
                        surname_prefix = character(),
                        surname = character(),
                        suffix = character(),
                        notes = character()) {
  
  nam_notes <- purrr::map(notes, tidyged.internals::NOTE_STRUCTURE)
  
  names <- tidyged.internals::PERSONAL_NAME_PIECES(name_piece_prefix = prefix,
                                                   name_piece_given = given, 
                                                   name_piece_nickname = nickname, 
                                                   name_piece_surname_prefix = surname_prefix,
                                                   name_piece_surname = surname,
                                                   name_piece_suffix = suffix,
                                                   notes = nam_notes)
  
  if(nrow(names) > 0 & #We need this check so an empty full name is not constructed
     length(given) + length(surname_prefix) + length(surname) + length(suffix) == 0) 
    stop("Try to define a given name or surname")
  
  names
}


#' Create a citation of a Source record
#'
#' @param gedcom A tidyged object.
#' @param source A character string identifying the source. This can either 
#' be an xref or term(s) to match to a source title.
#' @param where Specific location within the information referenced. For a published work, this could include
#' the volume of a multi-volume work and the page number(s). For a newspaper, it could include a column
#' number and page number. A census record might have an enumerating district, page number, line number, 
#' dwelling number, and family number. 
#' The data in this field should be in the form of a label and value pair, such as Label1: value,
#' Label2: value, with each pair being separated by a comma. For example, Film: 1234567,
#' Frame: 344, Line: 28.
#' @param event A code that indicates the type of event which was responsible for the source entry being recorded. 
#' For example, if the entry was created to record a birth of a child, then the type would be BIRT regardless of 
#' the assertions made from that record, such as the mother's name or mother's birth date. This will allow a 
#' prioritised best view choice and a determination of the certainty associated with the source used in asserting 
#' the cited fact.
#' @param role Indicates what role this person played in the event that is being cited in this context.
#' @param entry_date A date_calendar(), date_period(), date_range(), or date_approximated() 
#' value giving the date that this data was entered into the original source document.
#' @param source_text A verbatim copy of any description contained within the source. 
#' This indicates notes or text that are actually contained in the source document, 
#' not the submitter's opinion about the source.
#' @param certainty An evaluation of the credibility of a piece of information, based upon 
#' its supporting evidence. Some systems use this feature to rank multiple conflicting opinions 
#' for display of most likely information first. It is not intended to eliminate the receiver's 
#' need to evaluate the evidence for themselves. Values allowed:
#' "unreliable", "subjective", "secondary", "primary".
#' @param notes A character vector of notes accompanying the citation. These could be xrefs to 
#' existing Note records.
#' @param multimedia_links A character vector of Multimedia record xrefs accompanying this 
#' record.
#'
#' @return A tibble describing a source citation.
#' @export
source_citation <- function(gedcom,
                            source,
                            where = character(),
                            event = character(),
                            role = character(),
                            entry_date = character(),
                            source_text = character(),
                            certainty = character(),
                            notes = character(),
                            multimedia_links = character()) {
  
  sour <- get_valid_xref(gedcom, source, .pkgenv$record_string_sour, is_sour)

  cit_notes <- create_note_structures(gedcom, notes)
  media_links <- create_multimedia_links(gedcom, multimedia_links)
  
  if(length(certainty) > 0) {
    certainty <- dplyr::case_when(certainty == "unreliable" ~ "0",
                                  certainty == "subjective" ~ "1",
                                  certainty == "secondary" ~ "2",
                                  certainty == "primary" ~ "3",
                                  TRUE ~ "error")
    
    if(certainty == "error") stop("Invalid certainty value given")
  }
  
  tidyged.internals::SOURCE_CITATION(xref_sour = sour,
                                     where_within_source = where,
                                     event_type_cited_from = event,
                                     role_in_event = role,
                                     entry_recording_date = entry_date,
                                     text_from_source = source_text,
                                     certainty_assessment = certainty,
                                     multimedia_links = media_links,
                                     notes = cit_notes)
  
  
}