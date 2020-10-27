

#' Import a GEDCOM file
#'
#' Imports a *.ged file and creates a tidygedcom object.
#'
#' @param filepath The full filepath of the GEDCOM file
#'
#' @return A tidygedcom object
#' @export
#'
#' @examples
#' \dontrun{
#' import_gedcom("C:/my_family.ged")
#' }
import_gedcom <- function(filepath) {
  
  if(tolower(stringr::str_sub(filepath, -4, -1)) != ".ged")
    warning("GEDCOM files usually have a .ged extension. Continuing anyway.")
  
  ged <- readr::read_lines(filepath) %>% 
    stringr::str_trim() %>% 
    tibble::tibble(value = .) %>%
    tidyr::extract(value, into = c("level", "record", "tag", "value"), 
                   regex = "^(.) (@.+@)? ?(\\w{3,5}) ?(.*)$") %>%
    dplyr::mutate(record = dplyr::if_else(tag == "HEAD", "HD", record),
                  record = dplyr::if_else(tag == "TRLR", "TR", record)) %>%
    tidyr::fill(record) %>% 
    dplyr::mutate(level = as.numeric(level),
                  value = stringr::str_replace_all(value, "@@", "@")) %>% 
    set_class_to_tidygedcom()

  validate_gedcom(ged)
  ged
  
}



#' Save a tidygedcom object to disk as a GEDCOM file
#'
#' @param gedcom A tidygedcom object.
#' @param filepath The full filepath to write to.
#'
#' @return Nothing
#' @export
export_gedcom <- function(gedcom, filepath) {
  
  if(tolower(stringr::str_sub(filepath, -4, -1)) != ".ged")
    warning("Output is not being saved as a GEDCOM file (*.ged)")
  
  gedcom %>%
    #update_header(file_name = basename(filepath)) %>% 
    purrr::when(
      nrow(dplyr::filter(., record == "HD", tag == "FILE")) == 0 ~
                     tibble::add_row(., tibble::tibble(level = 1, record = "HD", 
                                                       tag = "FILE", value = basename(filepath)),
                                     .before = find_insertion_point(., "HD", 0, "HEAD")),
      nrow(dplyr::filter(., record == "HD", tag == "FILE")) == 1 ~
        dplyr::mutate(., value = dplyr::if_else(record == "HD" & tag == "FILE", basename(filepath), value)),
      ~ .
    ) %>% 
    dplyr::mutate(value = str_replace_all(value, "@", "@@")) %>% 
    dplyr::mutate(record = dplyr::if_else(dplyr::lag(record) == record, "", record)) %>% 
    dplyr::mutate(record = dplyr::if_else(record == "TR", "", record)) %>% 
    tidyr::replace_na(list(record = "")) %>% 
    dplyr::transmute(value = paste(level, record, tag, value)) %>% 
    dplyr::mutate(value = stringr::str_replace_all(value, "  ", " ")) %>%
    utils::write.table(filepath, na = "", col.names = FALSE, quote = FALSE, row.names = FALSE)
  
}



#' Create a base tidygedcom object
#' 
#' This function creates a minimal tidygedcom object with header and footer sections and a single submitter record.
#'  
#' @param submitter_details Details of the submitter of the file (you?) using the subm() function. If no submitter
#' name is provided, the username is used.
#' @param gedcom_description A note to describe the contents of the file in terms of "ancestors or descendants of" 
#' so that the person receiving the data knows what genealogical information the transmission contains.
#' @param gedcom_copyright A copyright statement needed to protect the copyrights of the submitter of this GEDCOM file.
#' @param source_data_name The name of the electronic data source that was used to obtain the data in this transmission. 
#' @param source_data_date The date this source was created or published. Ensure you create this date with the 
#' date_exact() function.
#' @param source_data_copyright A copyright statement required by the owner of data from which this information was 
#' obtained.  
#' @param receiving_system The name of the system expected to process the GEDCOM-compatible transmission. 
#' @param language The human language in which the data in the transmission is normally read or written.
#'
#' @return A minimal tidygedcom object 
#' @export
gedcom <- function(submitter_details = subm(),
                   gedcom_description = character(),
                   gedcom_copyright = character(),
                   source_data_name = character(),
                   source_data_date = date_exact(),
                   source_data_copyright = character(),
                   receiving_system = character(),
                   language = character()) {
  
  
  GEDCOM_HEADER(header_extension = 
    LINEAGE_LINKED_HEADER_EXTENSION(name_of_source_data = source_data_name,
                                    publication_date_source_data = source_data_date,
                                    copyright_source_data = source_data_copyright,
                                    receiving_system_name = receiving_system,
                                    file_creation_date = current_date(),
                                    language_of_text = language,
                                    xref_subm = assign_xref(xref_prefix_subm(), 1),
                                    copyright_gedcom_file = gedcom_copyright,
                                    gedcom_content_description = gedcom_description)) %>% 
    dplyr::bind_rows(submitter_details, 
                     FOOTER_SECTION()) %>%
    set_class_to_tidygedcom()
  
}


