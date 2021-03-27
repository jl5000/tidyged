
#' Define a place associated with a fact
#'
#' @param name The jurisdictional name of the place. 
#' Jurisdictions are separated by commas, for example, "Cove, Cache, Utah, USA."
#' @param phonetic_var A character vector of phonetic variations of the place name.
#' @param phonetisation_method A character vector giving the method used in transforming the text to 
#' the corresponding phonetic variation. If this argument is used, it must be the same size
#' as the phonetic_var argument.
#' @param romanised_var A character vector of romanised variations of the place name. 
#' @param romanisation_method A character vector giving the method used in transforming the text to 
#' the corresponding romanised variation. If this argument is used, it must be the same size
#' as the romanised_var argument.
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
                  phonetisation_method = character(),
                  romanised_var = character(),
                  romanisation_method = character(),
                  latitude = character(),
                  longitude = character(),
                  notes = character()) {
  
  plac_notes <- purrr::map(notes, tidyged.internals::NOTE_STRUCTURE)
  
  if(length(name) == 0) {
    
    tidyged.internals::PLACE_STRUCTURE(character())
    
  } else {
    
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
