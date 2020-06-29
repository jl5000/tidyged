
gedcom <- function(submitter_name,
                   gedcom_description = character()) {
  
  dplyr::bind_rows(
    HEADER_SECTION(submitter_ref,
                   approved_system_id = "tidygedcom",
                   submission_ref = character(),
                   system_version_number = character(),
                   name_of_product = character(),
                   name_of_business = character(),
                   business_address = ADDRESS_STRUCTURE(character()),
                   source_data_name = character(),
                   source_data_publication_date = character(),
                   source_data_copyright = character(),
                   receiving_system = character(),
                   transmission_date = character(),
                   transmission_time = character(),
                   file_name = character(),
                   gedcom_copyright = character(),
                   character_set = "UTF-8",
                   character_set_version_number = character(),
                   language = "English",
                   place = character(),
                   gedcom_description = character()),
    FOOTER_SECTION()
  )
  
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