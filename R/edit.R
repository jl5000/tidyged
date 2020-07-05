
gedcom <- function(submitter_name,
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
                 xref_subn = character(),
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
    dplyr::bind_rows(FOOTER_SECTION())
  
  
}


update_header <- function(gedcom) {
  
  tibble::add_row(gedcom,
                  )
  
}

add_individual <- function(gedcom) {
  
  
}


remove_individual <- function(gedcom) {
  
  
  
  
}


update_individual <- function(gedcom) {
  
  
  
  
}


add_family <- function(gedcom) {
  
  
}


remove_family <- function(gedcom) {
  
  
  
}


update_family <- function(gedcom) {
  
  
  
  
}