
gedcom <- function(submitter_name,
                   gedcom_description = character()) {
  
  header_section(submitter_ref,
                 approved_system_id = "tidygedcom",
                 submission_ref = character(),
                 system_version_number = character(),
                 name_of_product = character(),
                 name_of_business = character(),
                 business_address = address_structure(character()),
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
                 gedcom_description = character())
  
}


update_header <- function() {
  
  
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