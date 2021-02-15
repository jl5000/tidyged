
#' Create a base tidyged object
#' 
#' This function creates a minimal tidyged object with header and footer sections and a single submitter record.
#'  
#' @param submitter_details Details of the submitter of the file (you?) using the subm() function. If no submitter
#' name is provided, the username is used.
#' @param gedcom_description A note to describe the contents of the file in terms of "ancestors or descendants of" 
#' so that the person receiving the data knows what genealogical information the file contains.
#' @param gedcom_copyright A copyright statement needed to protect the copyrights of the submitter of this GEDCOM file.
#' @param source_data_name The name of the electronic data source that was used to obtain the data in this file. 
#' @param source_data_date The date this source was created or published. Ensure you create this date with the 
#' date_exact() function.
#' @param source_data_copyright A copyright statement required by the owner of data from which this information was 
#' obtained.  
#' @param receiving_system The name of the system expected to process the GEDCOM-compatible file. 
#' @param language The human language in which the data in the file is normally read or written.
#'
#' @return A minimal tidyged object. 
#' @export
gedcom <- function(submitter_details = subm(),
                   gedcom_description = character(),
                   gedcom_copyright = character(),
                   source_data_name = character(),
                   source_data_date = date_exact(),
                   source_data_copyright = character(),
                   receiving_system = "tidyged",
                   language = "English") {
  
  tidyged.internals::GEDCOM_HEADER(
    header_extension = tidyged.internals::LINEAGE_LINKED_HEADER_EXTENSION(system_id = "tidyged",
                                                                          product_version_number = utils::packageVersion("tidyged"),
                                                                          name_of_product = "The 'tidyged' package for the R language",
                                                                          name_of_business = "Jamie Lendrum",
                                                                          business_address = tidyged.internals::ADDRESS_STRUCTURE(address_email = "jalendrum@gmail.com",
                                                                                                               address_web_page = "https://jl5000.github.io/tidyged/"),
                                                                          name_of_source_data = source_data_name,
                                                                          publication_date = source_data_date,
                                                                          copyright_source_data = source_data_copyright,
                                                                          receiving_system_name = receiving_system,
                                                                          file_creation_date = date_current(),
                                                                          language_of_text = language,
                                                                          xref_subm = assign_xref(.pkgenv$xref_prefix_subm, 1),
                                                                          copyright_gedcom_file = gedcom_copyright,
                                                                          gedcom_content_description = gedcom_description)) %>% 
    dplyr::bind_rows(submitter_details, 
                     tidyged.internals::FOOTER_SECTION()) %>%
    tidyged.internals::set_class_to_tidyged()
  
}



#' Define a Submitter record for a new tidyged object
#'
#' @details 
#' This function is supposed to be used in the gedcom() function to define a
#' new tidyged object.
#' 
#' This submitter record identifies the individual or organization that contributed 
#' information contained in the GEDCOM file.
#' 
#' @param name The name of the submitter.
#' @param local_address_lines The first lines of the submitter address.
#' @param city The city of the submitter.
#' @param state The state/county of the submitter.
#' @param postal_code The postal code of the submitter.
#' @param country The country of the submitter.
#' @param phone_number A character vector containing up to three phone numbers of the submitter.
#' @param email A character vector containing up to three email addresses of the submitter.
#' @param fax A character vector containing up to three fax numbers of the submitter.
#' @param web_page A character vector containing up to three web pages of the submitter.
#' @param subm_notes A character vector of notes accompanying this Submitter record.
#' These could be xrefs to existing Note records.
#' @param multimedia_links TODO
#'
#' @return A Submitter record to be incorporated into a new tidyged object.
#' @export
subm <- function(name = unname(Sys.info()["user"]),
                 local_address_lines = character(),
                 city = character(),
                 state = character(),
                 postal_code = character(),
                 country = character(),
                 phone_number = character(),
                 email = character(),
                 fax = character(),
                 web_page = character(),
                 subm_notes = character(),
                 multimedia_links = character()) {
  
  if(length(local_address_lines) > 3) local_address_lines <- local_address_lines[1:3]
  
  address <- tidyged.internals::ADDRESS_STRUCTURE(local_address_lines = local_address_lines,
                                                     address_city = city,
                                                     address_state = state,
                                                     address_postal_code = postal_code,
                                                     address_country = country,
                                                     phone_number = phone_number,
                                                     address_email = email,
                                                     address_fax = fax,
                                                     address_web_page = web_page)
  
  sub_notes <- purrr::map(subm_notes, tidyged.internals::NOTE_STRUCTURE)
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_media(gedcom), tags = "FILE") %>% 
    purrr::map(tidyged.internals::MULTIMEDIA_LINK)
  
  tidyged.internals::SUBMITTER_RECORD(xref_subm = assign_xref(.pkgenv$xref_prefix_subm, 1),
                                         submitter_name = name,
                                         address = address,
                                         notes = sub_notes,
                                         multimedia_links = media_links)
  
}



