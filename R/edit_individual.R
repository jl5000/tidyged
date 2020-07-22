


add_individual <- function(gedcom,
                           sex = character(),
                           restriction_notice = character(),
                           submitters = character(),
                           aliases = character(),
                           submitters_interested_in_ancestors = character(),
                           submitters_interested_in_descendents = character(),
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
  
  xrefs_subm_interested_in_descendents <- 
    purrr::map_chr(submitters_interested_in_descendents, find_xref, 
                   gedcom = gedcom, record_xrefs = xrefs_submitters(gedcom), tags = "NAME")
  
  ind_record <- INDIVIDUAL_RECORD(xref_indi = xref,
                                  restriction_notice = restriction_notice,
                                  sex_value = sex,
                                  xrefs_subm = xrefs_subm,
                                  xrefs_alia = xrefs_alia,
                                  xrefs_subm_interested_in_ancestors = xrefs_subm_interested_in_ancestors,
                                  xrefs_subm_interested_in_descendents = xrefs_subm_interested_in_descendents,
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
                                 name_personal,
                                 name_type = character(),
                                 name_pieces = PERSONAL_NAME_PIECES(), 
                                 name_phonetic_variation = character(),
                                 phonetic_type = character(),
                                 phonetic_name_pieces = list(),
                                 name_romanized_variation = character(),
                                 romanized_type = character(),
                                 romanized_name_pieces = list()) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  
  
}

add_individual_event <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
}


add_individual_attribute <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  
}

add_individual_association <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  
  
}


add_individual_family_link_as_spouse <- function(gedcom, family_xref) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  message("spouse", family_xref)
  gedcom
}

add_individual_family_link_as_child <- function(gedcom, family_xref) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  message("child", family_xref)
  gedcom
  
}

remove_individual <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  
  
  
  null_active_record(gedcom)
}


update_individual <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  

    
  
}
