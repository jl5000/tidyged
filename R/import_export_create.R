

import_gedcom <- function(filepath) {
  
  ged <- readr::read_lines(filepath) %>% 
    stringr::str_trim() %>% 
    tibble::tibble(value = .) %>%
    tidyr::extract(value, into = c("level", "id", "tag", "value"), 
                   regex = "^(.) (@.+@)? ?(\\w{3,5}) ?(.*)$") %>%
    dplyr::mutate(id = dplyr::if_else(tag == "HEAD", "HD", id),
                  id = dplyr::if_else(tag == "TRLR", "TR", id)) %>%
    tidyr::fill(id) %>% 
    dplyr::mutate(level = as.numeric(level)) %>% 
    set_class_to_tidygedcom()

  validate_gedcom(ged)
  ged
  
}



export_gedcom <- function(gedcom_df, filepath) {
  
  gedcom_df %>%
    update_header(file_name = basename(filepath)) %>% 
    dplyr::mutate(id = dplyr::if_else(dplyr::lag(id) == id, "", id)) %>% 
    tidyr::replace_na(list(id = "")) %>% 
    dplyr::transmute(value = paste(level, id, tag, value)) %>% 
    dplyr::mutate(value = stringr::str_replace_all(value, "  ", " ")) %>%
    utils::write.table(filepath, na = "", col.names = FALSE, quote = FALSE, row.names = FALSE)
  
}



#' @export
gedcom <- function(submitter_details = subm(),
                   gedcom_description = character(),
                   gedcom_copyright = character(),
                   source_data_name = character(),
                   source_data_date = date_exact(),
                   source_data_copyright = character(),
                   receiving_system = character(),
                   language = character(),
                   char_set = "UTF-8",
                   char_set_version = character()) {
  
  
  HEADER_SECTION(xref_subm = ref_to_xref(1, "U"),
                 approved_system_id = "tidygedcom",
                 character_set = char_set,
                 system_version_number = packageVersion("tidygedcom"),
                 name_of_source_data = source_data_name,
                 publication_date_source_data = source_data_date,
                 copyright_source_data = source_data_copyright,
                 receiving_system_name = receiving_system,
                 transmission_date = current_date(),
                 copyright_gedcom_file = gedcom_copyright,
                 character_set_version_number = char_set_version,
                 language_of_text = language,
                 gedcom_content_description = gedcom_description) %>% 
    dplyr::bind_rows(submitter_details, 
                     FOOTER_SECTION()) %>%
    set_class_to_tidygedcom()
  
  
}


