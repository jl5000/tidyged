
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
                   receiving_system = "gedcompendium",
                   language = "English") {
  
  tidyged.internals::GEDCOM_HEADER(
    header_extension = tidyged.internals::LINEAGE_LINKED_HEADER_EXTENSION(system_id = "gedcompendium",
                                                                          name_of_product = "The 'gedcompendium' ecosystem of packages for the R language",
                                                                          name_of_business = "Jamie Lendrum",
                                                                          business_address = address(email = "jalendrum@gmail.com",
                                                                                                     web_page = "https://jl5000.github.io/tidyged/"),
                                                                          name_of_source_data = source_data_name,
                                                                          publication_date = source_data_date,
                                                                          copyright_source_data = source_data_copyright,
                                                                          receiving_system_name = receiving_system,
                                                                          file_creation_date = date_current(),
                                                                          language_of_text = language,
                                                                          xref_subm = tidyged.internals::assign_xref_subm(ref = 1),
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
#' @param subm_address An address() object containing the submitter address.
#' @param subm_notes A character vector of notes accompanying this Submitter record.
#' These could be xrefs to existing Note records.
#' @param multimedia_links A character vector of multimedia file references accompanying this 
#' Submitter record. These could be xrefs to existing Multimedia records.
#'
#' @return A Submitter record to be incorporated into a new tidyged object.
#' @export
subm <- function(name = unname(Sys.info()["user"]),
                 subm_address = address(),
                 subm_notes = character(),
                 multimedia_links = character()) {
  
  sub_notes <- purrr::map(subm_notes, tidyged.internals::NOTE_STRUCTURE)
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_media(gedcom), tags = "FILE") %>% 
    purrr::map(tidyged.internals::MULTIMEDIA_LINK)
  
  tidyged.internals::SUBMITTER_RECORD(xref_subm = tidyged.internals::assign_xref_subm(ref = 1),
                                         submitter_name = name,
                                         address = subm_address,
                                         notes = sub_notes,
                                         multimedia_links = media_links)
  
}



