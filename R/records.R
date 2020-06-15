

header_section <- function(submitter_ref,
                           approved_system_id,
                           character_set,
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
                           character_set_version_number = character(),
                           language = character(),
                           place = character(),
                           gedcom_description = character()) {
  
  system_version_number <- as.character(system_version_number)
  character_set_version_number <- as.character(character_set_version_number)
  check_character_set(character_set)
  
  temp <- bind_rows(
    tibble(level = 0, id = "HD", tag = "HEAD", value = ""),
    tibble(level = 1, tag = "SOUR", value = approved_system_id),
    tibble(level = 2, tag = "VERS", value = system_version_number),
    tibble(level = 2, tag = "NAME", value = name_of_product),
    tibble(level = 2, tag = "CORP", value = name_of_business),
    business_address %>% add_levels(3),
    tibble(level = 2, tag = "DATA", value = source_data_name),
    tibble(level = 3, tag = "DATE", value = source_data_publication_date),
    tibble(level = 3, tag = "COPR", value = source_data_copyright),
    tibble(level = 1, tag = "DEST", value = receiving_system),
    tibble(level = 1, tag = "DATE", value = transmission_date),
    tibble(level = 2, tag = "TIME", value = transmission_time),
    tibble(level = 1, tag = "SUBM", value = ref_to_xref(submitter_ref, "U")),
    tibble(level = 1, tag = "SUBN", value = ref_to_xref(submission_ref, "SU")),
    tibble(level = 1, tag = "FILE", value = file_name),
    split_text(start_level = 1, top_tag = "COPR", text = gedcom_copyright, char_limit = 90),
    tibble(level = 1, tag = "GEDC", value = ""),
    tibble(level = 2, tag = "VERS", value = "5.5.1"),
    tibble(level = 2, tag = "FORM", value = "Lineage-Linked"),
    tibble(level = 1, tag = "CHAR", value = character_set),
    tibble(level = 2, tag = "VERS", value = character_set_version_number),
    tibble(level = 1, tag = "LANG", value = language),
    tibble(level = 1, tag = "PLAC", value = ""),
    tibble(level = 2, tag = "FORM", value = place),
    split_text(start_level = 1, top_tag = "NOTE", text = gedcom_description)
  ) %>% 
    finalise()
  
  # There should only be one FORM (Lineage linked) if no place defined
  if (sum(temp$tag == "FORM") == 1) {
    filter(temp, tag != "PLAC")
  } else {
    temp
  }
  
  
}



family_record <- function(family_ref,
                          restriction_notice = character(),
                          events = list(),
                          husband_ref = character(),
                          wife_ref = character(),
                          children_ref = character(),
                          children_count = character(),
                          submitter_refs = character(),
                          lds_sealings = list(),
                          user_reference_number = character(),
                          user_reference_type = character(),
                          automated_record_id = character(),
                          change_date = change_date(character()),
                          notes = list(),
                          source_citations = list(),
                          multimedia_links = list()){
  
  children_count <- as.character(children_count)
  user_reference_number <- as.character(user_reference_number)
  check_restriction_notice(restriction_notice)
  
  bind_rows(
    tibble(level = 0, id = ref_to_xref(family_ref, "F"), tag = "FAM"),
    tibble(level = 1, tag = "RESN", value = restriction_notice),
    events %>% bind_rows() %>% add_levels(1),
    tibble(level = 1, tag = "HUSB", value = ref_to_xref(husband_ref, "I")),
    tibble(level = 1, tag = "WIFE", value = ref_to_xref(wife_ref, "I")),
    tibble(level = 1, tag = "CHIL", value = ref_to_xref(children_ref, "I")),
    tibble(level = 1, tag = "NCHI", value = children_count),
    tibble(level = 1, tag = "SUBM", value = ref_to_xref(submitter_refs, "U")),
    lds_sealings %>% bind_rows() %>% add_levels(1),
    tibble(level = 1, tag = "REFN", value = user_reference_number),
    tibble(level = 2, tag = "TYPE", value = user_reference_type),
    tibble(level = 1, tag = "RIN", value = automated_record_id),
    change_date %>% add_levels(1),
    notes %>% bind_rows() %>% add_levels(1),
    source_citations %>% bind_rows() %>% add_levels(1),
    multimedia_links %>% bind_rows() %>% add_levels(1)
  ) %>% 
    finalise()
  
  
}

individual_record <- function(individual_ref,
                              restriction_notice = character(),
                              names = list(),
                              sex = character(),
                              events = list(),
                              attributes = list(),
                              ordinance = list(),
                              child_to_family_links = list(),
                              spouse_to_family_links = list(),
                              submitter_refs = character(),
                              associations = list(),
                              alias_refs = character(),
                              submitters_interested_in_ancestors = character(),
                              submitters_interested_in_descendents = character(),
                              record_file_number = character(),
                              ancestral_file_number = character(),
                              user_reference_number = character(),
                              user_reference_type = character(),
                              automated_record_id = character(),
                              change_date = change_date(character()),
                              notes = list(),
                              source_citations = list(),
                              multimedia_links = list()) {
  
  record_file_number <- as.character(record_file_number)
  ancestral_file_number <- as.character(ancestral_file_number)
  user_reference_number <- as.character(user_reference_number)
  check_restriction_notice(restriction_notice)
  check_sex(sex)
  
  bind_rows(
    tibble(level = 0, id = ref_to_xref(individual_ref, "I"), tag = "INDI"),
    tibble(level = 1, tag = "RESN", value = restriction_notice),
    names %>% bind_rows() %>% add_levels(1),
    tibble(level = 1, tag = "SEX", value = sex),
    events %>% bind_rows() %>% add_levels(1),
    attributes %>% bind_rows() %>% add_levels(1),
    ordinance %>% bind_rows() %>% add_levels(1),
    child_to_family_links %>% bind_rows() %>% add_levels(1),
    spouse_to_family_links %>% bind_rows() %>% add_levels(1),
    tibble(level = 1, tag = "SUBM", value = ref_to_xref(submitter_refs, "U")),
    associations %>% bind_rows() %>%  add_levels(1),
    tibble(level = 1, tag = "ALIA", value = ref_to_xref(alias_refs, "I")),
    tibble(level = 1, tag = "ANCI", value = ref_to_xref(submitters_interested_in_ancestors, "U")),
    tibble(level = 1, tag = "DESI", value = ref_to_xref(submitters_interested_in_descendents, "U")),
    tibble(level = 1, tag = "RFN", value = record_file_number),
    tibble(level = 1, tag = "AFN", value = ancestral_file_number),
    tibble(level = 1, tag = "REFN", value = user_reference_number),
    tibble(level = 2, tag = "TYPE", value = user_reference_type),
    tibble(level = 1, tag = "RIN", value = automated_record_id),
    change_date %>% add_levels(1),
    notes %>% bind_rows() %>% add_levels(1),
    source_citations %>% bind_rows() %>% add_levels(1),
    multimedia_links %>% bind_rows() %>% add_levels(1)
  ) %>% 
    finalise()
  
}



multimedia_record <- function(object_ref,
                              file_reference,
                              media_format,
                              media_type = character(),
                              title = character(),
                              user_ref_number = character(),
                              user_ref_type = character(),
                              automated_record_id = character(),
                              notes = list(),
                              source_citations = list(),
                              change_date = change_date(character())){
  
  file_reference <- as.character(file_reference)
  user_ref_number <- as.character(user_ref_number)
  
  bind_rows(
    tibble(level = 0, id = ref_to_xref(object_ref, "M"), tag = "OBJE"),
    tibble(level = 1, tag = "FILE", value = file_reference),
    tibble(level = 2, tag = "FORM", value = media_format),
    tibble(level = 3, tag = "TYPE", value = media_type),
    tibble(level = 2, tag = "TITL", value = title),
    tibble(level = 1, tag = "REFN", value = user_ref_number),
    tibble(level = 2, tag = "TYPE", value = user_ref_type),
    tibble(level = 1, tag = "RIN", value = automated_record_id),
    notes %>% bind_rows() %>% add_levels(1),
    source_citations %>% bind_rows() %>% add_levels(1),
    change_date %>% add_levels(1)
  ) %>% 
    finalise()
  
  
}



note_record <- function(note_ref,
                        submitter_text,
                        user_ref_number = character(),
                        user_ref_type = character(),
                        automated_record_id = character(),
                        source_citations = list(),
                        change_date = change_date(character())){
  
  user_ref_number <- as.character(user_ref_number)
  
  bind_rows(
    split_text(start_level = 0, top_tag = "NOTE", text = submitter_text),
    tibble(level = 1, tag = "REFN", value = user_ref_number),
    tibble(level = 2, tag = "TYPE", value = user_ref_type),
    tibble(level = 1, tag = "RIN", value = automated_record_id),
    source_citations %>% bind_rows() %>% add_levels(1),
    change_date %>% add_levels(1)
  ) %>% 
    mutate(id = if_else(tag == "NOTE", ref_to_xref(note_ref, "T"), NA_character_)) %>% 
    finalise()
  
}



repository_record <- function(repo_ref,
                              repo_name,
                              repo_address = address_structure(character()),
                              repo_notes = list(),
                              user_ref_number = character(),
                              user_ref_type = character(),
                              automated_record_id = character(),
                              change_date = change_date(character())){

  user_ref_number <- as.character(user_ref_number)
  
  bind_rows(
    tibble(level = 0, id = ref_to_xref(repo_ref, "R"), tag = "REPO"),
    tibble(level = 1, tag = "NAME", value = repo_name),
    repo_address %>% add_levels(1),
    repo_notes %>% bind_rows() %>% add_levels(1),
    tibble(level = 1, tag = "REFN", value = user_ref_number),
    tibble(level = 2, tag = "TYPE", value = user_ref_type),
    tibble(level = 1, tag = "RIN", value = automated_record_id),
    change_date %>% add_levels(1)
  ) %>% 
    finalise()
  
    
}



source_record <- function(source_ref,
                          events_recorded = character(),
                          date_period = character(),
                          source_jurisdiction_place = character(),
                          responsible_agency = character(),
                          data_notes = list(),
                          source_originator = character(),
                          source_title = character(),
                          source_filed_by = character(),
                          source_publ_facts = character(),
                          source_text = character(),
                          source_repo_citations = list(),
                          user_ref_number = character(),
                          user_ref_type = character(),
                          automated_record_id = character(),
                          change_date = change_date(),
                          notes = list(),
                          multimedia_links = list()){
  
  user_ref_number <- as.character(user_ref_number)
  
  bind_rows(
    tibble(level = 0, id = ref_to_xref(source_ref, "S"), tag = "SOUR"),
    tibble(level = 1, tag = "DATA", value = ""),
    tibble(level = 2, tag = "EVEN", value = events_recorded),
    tibble(level = 3, tag = "DATE", value = date_period),
    tibble(level = 3, tag = "PLAC", value = source_jurisdiction_place),
    tibble(level = 2, tag = "AGNC", value = responsible_agency),
    data_notes %>% bind_rows() %>% add_levels(2),
    split_text(start_level = 1, top_tag = "AUTH", text = source_originator),
    split_text(start_level = 1, top_tag = "TITL", text = source_title),
    tibble(level = 1, tag = "ABBR", value = source_filed_by),
    split_text(start_level = 1, top_tag = "PUBL", text = source_publ_facts),
    split_text(start_level = 1, top_tag = "TEXT", text = source_text),
    source_repo_citations %>% bind_rows() %>% add_levels(1),
    tibble(level = 1, tag = "REFN", value = user_ref_number),
    tibble(level = 2, tag = "TYPE", value = user_ref_type),
    tibble(level = 1, tag = "RIN", value = automated_record_id),
    change_date %>% add_levels(1),
    notes %>% bind_rows() %>% add_levels(1),
    multimedia_links %>% bind_rows() %>% add_levels(1)
  ) %>% 
    finalise()
  
}


# Max = 1
submission_record <- function(submission_ref,
                              submitter_ref = character(),
                              name_of_family_file = character(),
                              temple_code = character(),
                              num_anc_generations = character(),
                              num_des_generations = character(),
                              ordinance_flag = character(),
                              automated_record_id = character(),
                              notes = list(),
                              change_date = change_date(character())){
 
  num_anc_generations <- as.character(num_anc_generations)
  num_des_generations <- as.character(num_des_generations)
  check_ordinance_flag(ordinance_flag)
  
  bind_rows(
    tibble(level = 0, id = ref_to_xref(submission_ref, "SN"), tag = "SUBN"),
    tibble(level = 1, tag = "SUBM", value = ref_to_xref(submitter_ref, "U")),
    tibble(level = 1, tag = "FAMF", value = name_of_family_file),
    tibble(level = 1, tag = "TEMP", value = temple_code),
    tibble(level = 1, tag = "ANCE", value = num_anc_generations),
    tibble(level = 1, tag = "DESC", value = num_des_generations),
    tibble(level = 1, tag = "ORDI", value = ordinance_flag),
    tibble(level = 1, tag = "RIN", value = automated_record_id),
    notes %>% bind_rows() %>% add_levels(1),
    change_date %>% add_levels(1)
  ) %>% 
    finalise()
  
   
}


submitter_record <- function(submitter_ref,
                             submitter_name = character(),
                             submitter_address = address_structure(character()),
                             multimedia_links = list(),
                             language_preference = character(),
                             submitter_registered_rfn = character(),
                             automated_record_id = character(),
                             notes = list(),
                             change_date = change_date(character())){
  
  submitter_registered_rfn <- as.character(submitter_registered_rfn)
  
  bind_rows(
    tibble(level = 0, id = ref_to_xref(submitter_ref, "U"), tag = "SUBM"),
    tibble(level = 1, tag = "NAME", value = submitter_name),
    submitter_address %>% add_levels(1),
    multimedia_links %>% bind_rows() %>% add_levels(1),
    tibble(level = 1, tag = "LANG", value = language_preference),
    tibble(level = 1, tag = "RFN", value = submitter_registered_rfn),
    tibble(level = 1, tag = "RIN", value = automated_record_id),
    notes %>% bind_rows() %>% add_levels(1),
    change_date %>% add_levels(1)
  ) %>% 
    finalise()
  
  
}



footer_section <- function(){
  tibble(level = 0, id = "TR", tag = "TRLR", value = "") %>% 
    finalise()
}
