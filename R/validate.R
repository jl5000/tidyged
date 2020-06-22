

check_gedcom_file <- function(gedcom) {
  
  
  
}


validate_input_size <- function(input, max_dim, min_char = NULL, max_char = NULL) {
  if (length(input) > max_dim)
    stop("Input ", input, " has too many dimensions. The limit is ", max_dim)
  
  if (length(input) > 0 && !is.null(max_char) && max(nchar(input)) > max_char)
    stop("Input ", input, " has too many characters. The limit is ", max_char)
  
  if (length(input) > 0 && !is.null(min_char) && min(nchar(input)) < min_char)
    stop("Input ", input, " has too few characters. The minimum is ", min_char)
}

validate_input_pattern <- function(input, pattern) {
  for (i in input) {
    if (!grepl(pattern, i))
      stop("Input ", i, " is in an unexpected format")
  }
}

validate_input_choice <- function(input, choices) {
  if (length(input) == 1 && input %nin% choices) 
    stop("Invalid argument value: ", input, ".\n  The valid values are: ", 
         paste(choices, collapse = ", "))
}

validate_address_city <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_address_country <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_address_email <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 120)
}
validate_address_fax <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 60)
}
validate_address_lines <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_address_postal_code <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 10)
}
validate_address_state <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_address_web_page <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 120)
}
validate_address_country <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_adopted_by_which_parent <- function(input, max_dim) {
  choices <- c("HUSB", "WIFE", "BOTH")
  validate_input(input, choices)
  validate_input_size(input, max_dim, 1, 4)
}
validate_age_at_event <- function(input, max_dim) {
  validate_input_size(input, max_dim, 2, 15) # 5.5.1L
  validate_input_pattern(input, paste0("[<>]?", #TODO: handle the extra space
                                       "\\d{1,3}y \\d{1,2}m \\d{1,3}d$|",
                                       "\\d{1,3}y \\d{1,2}m$|",
                                       "\\d{1,3}y \\d{1,3}d$|",
                                       "\\d{1,2}m \\d{1,3}d$|",
                                       "\\d{1,3}y$|",
                                       "\\d{1,2}m$|",
                                       "\\d{1,3}d$|",
                                       "^CHILD$|",
                                       "^INFANT$|",
                                       "^STILLBORN$"))
}
validate_ancestral_file_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 12)
}
validate_approved_system_id <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 20)
}
validate_attribute_descriptor <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_attribute_type <- function(input, max_dim) {
  #TODO: Why is it not the full list? DSCR IDNO NCHI NMR SSN
  choices <- c("CAST", "DSCR", "EDUC", "IDNO",
               "NATI", "NCHI", "NMR", "OCCU",
               "PROP", "RELI", "RESI", "SSN",
               "TITL", "FACT")
  validate_input_choice(input, choices)
  validate_input_size(input, max_dim, 1, 4)
}
validate_automated_record_id <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 12)
}
validate_caste_name <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_cause_of_event <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_certainty_assessment <- function(input, max_dim) {
  choices <- as.character(0:3)
  validate_input(input, choices)
  validate_input_size(input, max_dim, 1, 1)
}
validate_change_date <- function(input, max_dim) {
  validate_input_size(input, max_dim, 10, 14)
  validate_input_pattern(input, date_exact_pattern())  
}
validate_character_set <- function(input, max_dim) {
  choices <- c("ANSEL", "UTF-8", "UNICODE", "ASCII")
  validate_input_choice(input, choices)
  validate_input_size(input, max_dim, 1, 8)
}
validate_child_linkage_status <- function(input, max_dim) {
  choices <- c("challenged", "disproven", "proven")
  validate_input_choice(input, choices)
  validate_input_size(input, max_dim, 1, 15)
}
validate_copyright_gedcom_file <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_copyright_source_data <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_count_of_children <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 3)
}
validate_count_of_marriages <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 3)
}
validate_date <- function(input, max_dim) {
  #TODO
  validate_input_size(input, max_dim, 4, 35)
}
validate_date_approximated <- function(input, max_dim) {
  #TODO
  validate_input_size(input, max_dim, 4, 35)
}
validate_date_calendar <- function(input, max_dim) {
  #TODO
  validate_input_size(input, max_dim, 4, 35)
}
validate_date_value <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 35)
  validate_input_pattern(input, date_value_pattern())
  #TODO: ensure year 1 is before year 2 etc.
}
validate_descriptive_title <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_event_attribute_type <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 15)
}
validate_event_descriptor <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_event_or_fact_classification <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_event_type_cited_from <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 15)
}
validate_event_type_family <- function(input, max_dim) {
  choices <- c("ANUL", "CENS", "DIV", "DIVF",
               "ENGA", "MARB", "MARC", "MARR",
               "MARL", "MARS", "RESI", "EVEN")
  validate_input_choice(input, choices)
  validate_input_size(input, max_dim, 3, 4)
}
validate_event_type_individual <- function(input, max_dim) {
  choices <- c("BIRT", "CHR", "DEAT", "BURI", "CREM",
               "ADOP", "BAPM", "BARM", "BASM", "BLES",
               "CHRA", "CONF", "FCOM", "ORDN", "NATU",
               "EMIG", "IMMI", "CENS", "PROB", "WILL",
               "GRAD", "RETI", "EVEN")
  validate_input_choice(input, choices)
  validate_input_size(input, max_dim, 3, 4)
}
validate_events_recorded <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_file_name <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_gedcom_content_description <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_generations_of_ancestors <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 4)
}
validate_generations_of_descendents <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 4)
}
validate_language_of_text <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 15)
}
validate_language_preference <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
#TODO Month stuff
validate_multimedia_file_reference <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 30)
}
validate_multimedia_format <- function(input, max_dim) {
  choices <- c( "bmp", "gif", "jpg", "ole", "pcx", "tif", "wav")
  validate_input_choice(input, choices)
  validate_input_size(input, max_dim, 3, 4)
}
validate_name_of_business <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_name_of_family_file <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_name_of_product <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_name_of_repository <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_name_of_source_data <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_name_personal <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_name_phonetic_variation <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_name_piece <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_phone_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 25)
}
validate_name_piece_given <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_name_piece_nickname <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 30)
}
validate_name_piece_prefix <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 30)
}
validate_name_piece_suffix <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 30)
}
validate_name_piece_surname <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_name_piece_surname_prefix <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 30)
}
validate_name_romanized_variation <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_name_text <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_name_type <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 30)
}
validate_national_id_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 30)
}
validate_national_or_tribal_origin <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_nobility_type_title <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_occupation <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_ordinance_process_flag <- function(input, max_dim) {
  choices <- c("yes", "no")
  validate_input_choice(input, choices)
  validate_input_size(input, max_dim, 2, 3)
}
validate_pedigree_linkage_type <- function(input, max_dim) {
  choices <- c("adopted", "birth", "foster", "sealing")
  validate_input_choice(input, choices)
  validate_input_size(input, max_dim, 5, 7)
}
validate_permanent_record_file_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_phone_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 25)
}
validate_phonetic_type <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 30)
}
validate_physical_description <- function(input, max_dim) {
  # character limits not checked as it is split over several lines if too long
  validate_input_size(input, max_dim)
}
validate_place_hierarchy <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_place_latitude <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 10) # 5.5.1L
  validate_input_pattern(input, "^[NS]\\d{1,2}\\.\\d{2,6}$")
}
validate_place_living_ordinance <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_place_longitude <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 11) # 5.5.1L
  validate_input_pattern(input, "^[EW]\\d{1,3}\\.\\d{2,6}$")
}
validate_place_name <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_place_phonetic_variation <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_place_romanized_variation <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_place_text <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_possessions <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_publication_date <- function(input, max_dim) {
  validate_input_size(input, max_dim, 10, 11)
  validate_input_pattern(input, "\\d{1,2} \\w{3} \\d{4}")
}
validate_receiving_system_name <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 20)
}
validate_record_identifier <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 18)
}
validate_registered_resource_identifier <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 25)
}
validate_relation_is_descriptor <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 25)
}
validate_religious_affiliation <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_responsible_agency <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_restriction_notice <- function(input, max_dim) {
  choices <- c("confidential", "locked", "privacy")
  validate_input_choice(input, choices)
  validate_input_size(input, max_dim, 6, 7)
}
validate_role_descriptor <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 25)
}
validate_role_in_event <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 15)
}
validate_romanized_type <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 30)
}
validate_scholastic_achievement <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_sex_value <- function(input, max_dim) {
  choices <- c("M", "F", "U")
  validate_input_choice(input, choices)
  validate_input_size(input, max_dim, 1, 248)
}
validate_social_security_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 9, 11)
}
validate_source_call_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_source_description <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_source_descriptive_title <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_source_filed_by_entry <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_source_jurisdiction_place <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_source_media_type <- function(input, max_dim) {
  choices <- c("audio", "book", "card", "electronic", "fiche", 
               "film", "magazine", "manuscript", "map", 
               "newspaper", "photo", "tombstone", "video")
  validate_input_choice(input, choices)
  validate_input_size(input, max_dim, 1, 15)
}
validate_source_originator <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_source_publication_facts <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_submitter_name <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_submitter_registered_rfn <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 30)
}
validate_submitter_text <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_temple_code <- function(input, max_dim) {
  validate_input_size(input, max_dim, 4, 5)
}
validate_text_from_source <- function(input, max_dim) {
  validate_text(input, max_dim, 1, 248)
}
validate_time_value <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 12)
  validate_input_pattern(input, paste0("^\\d\\d:\\d\\d:\\d\\d.\\d{1,3}$|",
                                       "^\\d\\d:\\d\\d:\\d\\d$|",
                                       "^\\d\\d:\\d\\d$"))
}
validate_transmission_date <- function(input, max_dim) {
  validate_date_exact(input, max_dim)
  validate_input_pattern(input, "\\d{1,2} \\w{3} \\d{4}")
}
validate_user_reference_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 20)
}
validate_user_reference_type <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 40)
}
validate_version_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 15)
}
validate_where_within_source <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_xref <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 22)
  validate_input_pattern(input, "@.{1,20}@")
}
validate_year <- function(input, max_dim) {
  validate_input_size(input, max_dim, 3, 4)
}
validate_year_greg <- function(input, max_dim) {
  #TODO
  validate_input_size(input, max_dim, 3, 7)
}

