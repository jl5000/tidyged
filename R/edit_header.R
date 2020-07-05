
gedcom <- function(submitter = subm(),
                   gedcom_description = character(),
                   gedcom_copyright = character(),
                   source_data_name = character(),
                   source_data_date = date_exact(),
                   source_data_copyright = character(),
                   receiving_system = character(),
                   language = character(),
                   char_set = "UTF-8",
                   char_set_version = character()) {
  
  submitter_name <- Sys.info()["user"]
  
  HEADER_SECTION(xref_subm = submitter_name,
                 approved_system_id = "tidygedcom",
                 character_set = char_set,
                 system_version_number = packageVersion("tidygedcom"),
                 name_of_source_data = source_data_name,
                 publication_date_source_data = source_data_date,
                 copyright_source_data = source_data_copyright,
                 receiving_system_name = receiving_system,
                 transmission_date = toupper(format(Sys.Date(), "%d %b %Y")),
                 copyright_gedcom_file = gedcom_copyright,
                 character_set_version_number = char_set_version,
                 language_of_text = language,
                 gedcom_content_description = gedcom_description) %>% 
    dplyr::bind_rows(FOOTER_SECTION()) %>% 
    set_class_to_tidygedcom()
  
  
}


update_header <- function(gedcom) {
  
  tibble::add_row(gedcom,
                  )
  
}


