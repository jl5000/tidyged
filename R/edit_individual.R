


add_individual <- function(gedcom,
                           sex = character(),
                           restriction_notice = character(),
                           submitters = character(),
                           aliases = character(),
                           submitters_interested_in_ancestors = character(),
                           submitters_interested_in_descendants = character(),
                           permanent_record_file_number = character(),
                           ancestral_file_number = character(),
                           user_reference_number = character(),
                           user_reference_type = character(),
                           automated_record_id = character()) {
  
  xref <- assign_xref(xref_prefix_indi(), gedcom = gedcom)
  
  xrefs_subm <- purrr::map_chr(submitters, find_xref, 
                               gedcom = gedcom, record_xrefs = xrefs_submitters(gedcom), tags = "NAME")
  
  xrefs_alia <- purrr::map_chr(aliases, find_xref, 
                               gedcom = gedcom, record_xrefs = xrefs_individuals(gedcom), 
                               tags = c("NAME", "ROMN", "FONE"))
  
  xrefs_subm_interested_in_ancestors <- 
    purrr::map_chr(submitters_interested_in_ancestors, find_xref, 
                   gedcom = gedcom, record_xrefs = xrefs_submitters(gedcom), tags = "NAME")
  
  xrefs_subm_interested_in_descendants <- 
    purrr::map_chr(submitters_interested_in_descendants, find_xref, 
                   gedcom = gedcom, record_xrefs = xrefs_submitters(gedcom), tags = "NAME")
  
  ind_record <- INDIVIDUAL_RECORD(xref_indi = xref,
                                  restriction_notice = restriction_notice,
                                  sex_value = sex,
                                  xrefs_subm = xrefs_subm,
                                  xrefs_alia = xrefs_alia,
                                  xrefs_subm_interested_in_ancestors = xrefs_subm_interested_in_ancestors,
                                  xrefs_subm_interested_in_descendants = xrefs_subm_interested_in_descendants,
                                  permanent_record_file_number = permanent_record_file_number,
                                  ancestral_file_number = ancestral_file_number,
                                  user_reference_number = user_reference_number,
                                  user_reference_type = user_reference_type,
                                  automated_record_id = automated_record_id) 
  
  gedcom %>%
    tibble::add_row(ind_record, .before = nrow(.)) %>% 
    set_active_record(xref)
}

add_individual_names <- function(gedcom,
                                 name,
                                 type = character(),
                                 prefix = character(),
                                 given = character(),
                                 nickname = character(),
                                 surname_prefix = character(),
                                 surname = character(),
                                 suffix = character()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  name_pieces <- PERSONAL_NAME_PIECES(name_piece_prefix = prefix,
                                      name_piece_given = given, 
                                      name_piece_nickname = nickname, 
                                      name_piece_surname_prefix = surname_prefix,
                                      name_piece_surname = surname,
                                      name_piece_suffix = suffix)
  
  name_str <- PERSONAL_NAME_STRUCTURE(name_personal = name,
                                       name_type = type,
                                       name_pieces = name_pieces)
  
  next_row = 5 #TODO: Function to find insertion point
  
  gedcom %>%
    tibble::add_row(name_str, .before = next_row)
}

add_individual_names_var <- function(gedcom,
                                     primary_name,
                                     variation_name,
                                     type,
                                     phonetic_variation = TRUE,
                                     prefix = character(),
                                     given = character(),
                                     nickname = character(),
                                     surname_prefix = character(),
                                     surname = character(),
                                     suffix = character()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  if(phonetic_variation) {
    
    name_phonetic_var <- variation_name
    phonetic_type <- type
    phon_name_pieces <- PERSONAL_NAME_PIECES(name_piece_prefix = prefix,
                                             name_piece_given = given, 
                                             name_piece_nickname = nickname, 
                                             name_piece_surname_prefix = surname_prefix,
                                             name_piece_surname = surname,
                                             name_piece_suffix = suffix)
    name_romanized_var <- character()
    romanized_type <- character()
    rom_name_pieces <- list()
    
  } else {
    
    name_romanized_var <- variation_name
    romanized_type <- type
    rom_name_pieces <- PERSONAL_NAME_PIECES(name_piece_prefix = prefix,
                                            name_piece_given = given, 
                                            name_piece_nickname = nickname, 
                                            name_piece_surname_prefix = surname_prefix,
                                            name_piece_surname = surname,
                                            name_piece_suffix = suffix)
    name_phonetic_var <- character()
    phonetic_type <- character()
    phon_name_pieces <- list()
    
  }
  
  name_str <- PERSONAL_NAME_STRUCTURE(name_personal = "what?",
                                      name_type = character(),
                                      name_pieces = PERSONAL_NAME_PIECES(), 
                                      name_phonetic_variation = name_phonetic_var,
                                      phonetic_type = phonetic_type,
                                      phonetic_name_pieces = phon_name_pieces,
                                      name_romanized_variation = name_romanized_var,
                                      romanized_type = romanized_type,
                                      romanized_name_pieces = rom_name_pieces)
  
  next_row = 5 #TODO: Function to find insertion point using primary_name
  
  gedcom %>%
    tibble::add_row(name_str, .before = next_row)
  
}

add_individual_event <- function(gedcom,
                                 event_type,
                                 age_at_event = character(),
                                 event_or_fact_classification = character(),
                                 date = date_value(),
                                 place_name = character(),
                                 place_hierarchy = character(),
                                 place_phonetic_variation = character(),
                                 phonetic_type = character(),
                                 place_romanized_variation = character(),
                                 romanized_type = character(),
                                 place_latitude = character(),
                                 place_longitude = character(),
                                 all_address_lines = character(),
                                 address_city = character(),
                                 address_state = character(),
                                 address_postal_code = character(),
                                 address_country = character(),
                                 phone_number = character(),
                                 address_email = character(),
                                 address_fax = character(),
                                 address_web_page = character(),
                                 responsible_agency = character(),
                                 religious_affiliation = character(),
                                 cause_of_event = character(),
                                 restriction_notice = character(),
                                 family_xref = character(),
                                 adopting_parent = character()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
}


add_individual_attribute <- function(gedcom,
                                     attribute_type,
                                     attribute_descriptor,
                                     age_at_event = character(),
                                     event_or_fact_classification = character(),
                                     date = date_value(),
                                     place_name = character(),
                                     place_hierarchy = character(),
                                     place_phonetic_variation = character(),
                                     phonetic_type = character(),
                                     place_romanized_variation = character(),
                                     romanized_type = character(),
                                     place_latitude = character(),
                                     place_longitude = character(),
                                     all_address_lines = character(),
                                     address_city = character(),
                                     address_state = character(),
                                     address_postal_code = character(),
                                     address_country = character(),
                                     phone_number = character(),
                                     address_email = character(),
                                     address_fax = character(),
                                     address_web_page = character(),
                                     responsible_agency = character(),
                                     religious_affiliation = character(),
                                     cause_of_event = character(),
                                     restriction_notice = character()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  
  
}

add_individual_association <- function(gedcom,
                                       associated_with,
                                       association) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  indi_xref <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), associated_with)
  
  asso_str <- ASSOCIATION_STRUCTURE(xref_indi = indi_xref,
                                    relation_is_descriptor = association)
  
  next_row = 5 #TODO: Function to find insertion point
  
  gedcom %>%
    tibble::add_row(asso_str, .before = next_row)
  
}


add_individual_family_link_as_spouse <- function(gedcom, family_xref) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  link <- SPOUSE_TO_FAMILY_LINK(xref_fam = family_xref)
  
  next_row = 5
  
  gedcom %>%
    tibble::add_row(link, .before = next_row)
}

add_individual_family_link_as_child <- function(gedcom, 
                                                family_xref,
                                                linkage_type = character(),
                                                linkage_status = character()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  link <- CHILD_TO_FAMILY_LINK(xref_fam = family_xref,
                               pedigree_linkage_type = linkage_type,
                               child_linkage_status = linkage_status)
  
  next_row = 5
  
  gedcom %>%
    tibble::add_row(link, .before = next_row)
  
}

remove_individual <- function(gedcom, remove_aliases = FALSE, remove_associatons = TRUE) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  #TODO: Need to remove associations and aliases
  gedcom %>% 
    dplyr::filter(record != get_active_record(.), value != get_active_record(.)) %>% 
    null_active_record()
}


update_individual <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  

    
  
}
