

header_section <- function(approved_system_id,
                           submitter_ref,
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
                           gedcom_description = character()) {
  
  
  temp <- bind_rows(
    tibble(offset = 0, id = "HD", tag = "HEAD", value = ""),
    tibble(offset = 1, tag = "SOUR", value = approved_system_id),
    tibble(offset = 2, tag = "VERS", value = system_version_number),
    tibble(offset = 2, tag = "NAME", value = name_of_product),
    tibble(offset = 2, tag = "CORP", value = name_of_business),
    business_address %>% add_offsets(3),
    tibble(offset = 2, tag = "DATA", value = source_data_name),
    tibble(offset = 3, tag = "DATE", value = source_data_publication_date),
    tibble(offset = 3, tag = "COPR", value = source_data_copyright),
    tibble(offset = 1, tag = "DEST", value = receiving_system),
    tibble(offset = 1, tag = "DATE", value = transmission_date),
    tibble(offset = 2, tag = "TIME", value = transmission_time),
    tibble(offset = 1, tag = "SUBM", value = ref_to_xref(submitter_ref, "U")),
    tibble(offset = 1, tag = "SUBN", value = ref_to_xref(submission_ref, "SU")),
    tibble(offset = 1, tag = "FILE", value = file_name),
    tibble(offset = 1, tag = "COPR", value = gedcom_copyright),
    tibble(offset = 1, tag = "GEDC", value = ""),
    tibble(offset = 2, tag = "VERS", value = "5.5.1"),
    tibble(offset = 2, tag = "FORM", value = "Lineage-Linked"),
    tibble(offset = 1, tag = "CHAR", value = character_set),
    tibble(offset = 2, tag = "VERS", value = character_set_version_number),
    tibble(offset = 1, tag = "LANG", value = language),
    tibble(offset = 1, tag = "PLAC", value = ""),
    tibble(offset = 2, tag = "FORM", value = place),
    tibble(offset = 1, tag = "NOTE", value = gedcom_description)
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
                          events = family_event_structure(),
                          husband_ref = character(),
                          wife_ref = character(),
                          children_ref = character(),
                          children_count = character(),
                          submitter_refs = character(),
                          lds_sealings = lds_spouse_sealing(),
                          user_reference_number = character(),
                          user_reference_type = character(),
                          automated_record_id = character(),
                          change_date = change_date(character()),
                          notes = note_structure(),
                          source_citations = source_citation(),
                          multimedia_links = multimedia_link()){
  
  
  bind_rows(
    tibble(offset = 0, id = ref_to_xref(family_ref, "F"), tag = "FAM"),
    tibble(offset = 1, tag = "RESN", value = restriction_notice),
    events %>% add_offsets(1),
    tibble(offset = 1, tag = "HUSB", value = ref_to_xref(husband_ref, "I")),
    tibble(offset = 1, tag = "WIFE", value = ref_to_xref(wife_ref, "I")),
    tibble(offset = 1, tag = "CHIL", value = ref_to_xref(children_ref, "I")),
    tibble(offset = 1, tag = "NCHI", value = children_count),
    tibble(offset = 1, tag = "SUBM", value = ref_to_xref(submitter_refs, "U")),
    lds_sealings %>% add_offsets(1),
    tibble(offset = 1, tag = "REFN", value = user_reference_number),
    tibble(offset = 2, tag = "TYPE", value = user_reference_type),
    tibble(offset = 1, tag = "RIN", value = automated_record_id),
    change_date %>% add_offsets(1),
    notes %>% add_offsets(1),
    source_citations %>% add_offsets(1),
    multimedia_links %>% add_offsets(1)
  ) %>% 
    finalise()
  
  
}

individual_record <- function(individual_ref,
                              restriction_notice = character(),
                              names = personal_name_structure(),
                              sex = character(),
                              events = individual_event_structure(),
                              attributes = individual_attribute_structure(),
                              ordinance = lds_individual_ordinance(),
                              child_to_family_links = child_to_family_link(),
                              spouse_to_family_links = spouse_to_family_link(),
                              submitter_refs = character(),
                              associations = association_structure(),
                              alias_refs = character(),
                              submitters_interested_in_ancestors = character(),
                              submitters_interested_in_descendents = character(),
                              record_file_number = character(),
                              ancestral_file_number = character(),
                              user_reference_number = character(),
                              user_reference_type = character(),
                              automated_record_id = character(),
                              change_date = change_date(character()),
                              notes = note_structure(),
                              source_citations = source_citation(),
                              multimedia_links = multimedia_link()) {
  
  
  bind_rows(
    tibble(offset = 0, id = ref_to_xref(individual_ref, "I"), tag = "INDI"),
    tibble(offset = 1, tag = "RESN", value = restriction_notice),
    names %>% add_offsets(1),
    tibble(offset = 1, tag = "SEX", value = sex),
    events %>% add_offsets(1),
    attributes %>% add_offsets(1),
    ordinance %>% add_offsets(1),
    child_to_family_links %>% add_offsets(1),
    spouse_to_family_links %>% add_offsets(1),
    tibble(offset = 1, tag = "SUBM", value = ref_to_xref(submitter_refs, "U")),
    associations %>% add_offsets(1),
    tibble(offset = 1, tag = "ALIA", value = ref_to_xref(alias_refs, "I")),
    tibble(offset = 1, tag = "ANCI", value = ref_to_xref(submitters_interested_in_ancestors, "U")),
    tibble(offset = 1, tag = "DESI", value = ref_to_xref(submitters_interested_in_descendents, "U")),
    tibble(offset = 1, tag = "RFN", value = record_file_number),
    tibble(offset = 1, tag = "AFN", value = ancestral_file_number),
    tibble(offset = 1, tag = "REFN", value = user_reference_number),
    tibble(offset = 2, tag = "TYPE", value = user_reference_type),
    tibble(offset = 1, tag = "RIN", value = automated_record_id),
    change_date %>% add_offsets(1),
    notes %>% add_offsets(1),
    source_citations %>% add_offsets(1),
    multimedia_links %>% add_offsets(1)
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
                              notes = note_structure(),
                              source_citations = source_citation(),
                              change_date = change_date(character())){
  
  bind_rows(
    tibble(offset = 0, id = ref_to_xref(object_ref, "M"), tag = "OBJE"),
    tibble(offset = 1, tag = "FILE", value = file_reference),
    tibble(offset = 2, tag = "FORM", value = media_format),
    tibble(offset = 3, tag = "TYPE", value = media_type),
    tibble(offset = 2, tag = "TITL", value = title),
    tibble(offset = 1, tag = "REFN", value = user_ref_number),
    tibble(offset = 2, tag = "TYPE", value = user_ref_type),
    tibble(offset = 1, tag = "RIN", value = automated_record_id),
    notes %>% add_offsets(1),
    source_citations %>% add_offsets(1),
    change_date %>% add_offsets(1)
  ) %>% 
    finalise()
  
  
}



note_record <- function(note_ref,
                        submitter_text,
                        user_ref_number = character(),
                        user_ref_type = character(),
                        automated_record_id = character(),
                        source_citations = source_citation(),
                        change_date = change_date(character())){
  
  bind_rows(
    tibble(offset = 0, id = ref_to_xref(note_ref, "T"), tag = "NOTE", value = submitter_text),
    tibble(offset = 1, tag = "REFN", value = user_ref_number),
    tibble(offset = 2, tag = "TYPE", value = user_ref_type),
    tibble(offset = 1, tag = "RIN", value = automated_record_id),
    source_citations %>% add_offsets(1),
    change_date %>% add_offsets(1)
  ) %>% 
    finalise()
  
  
}



repository_record <- function(repo_ref,
                              repo_name,
                              repo_address = address_structure(character()),
                              repo_notes = note_structure(),
                              user_ref_number = character(),
                              user_ref_type = character(),
                              automated_record_id = character(),
                              change_date = change_date(character())){

  bind_rows(
    tibble(offset = 0, id = ref_to_xref(repo_ref, "R"), tag = "REPO"),
    tibble(offset = 1, tag = "NAME", value = repo_name),
    repo_address %>% add_offsets(1),
    repo_notes %>% add_offsets(1),
    tibble(offset = 1, tag = "REFN", value = user_ref_number),
    tibble(offset = 2, tag = "TYPE", value = user_ref_type),
    tibble(offset = 1, tag = "RIN", value = automated_record_id),
    change_date %>% add_offsets(1)
  ) %>% 
    finalise()
  
    
}



source_record <- function(source_ref,
                          events_recorded = character(),
                          date_period = character(),
                          source_jurisdiction_place = character(),
                          responsible_agency = character(),
                          data_notes = note_structure(),
                          source_originator = character(),
                          source_title = character(),
                          source_filed_by = character(),
                          source_publ_facts = character(),
                          source_text = character(),
                          source_repo_citation = source_repository_citation(),
                          user_ref_number = character(),
                          user_ref_type = character(),
                          automated_record_id = character(),
                          change_date = change_date(),
                          notes = note_structure(),
                          multimedia_links = multimedia_link()){
  
  bind_rows(
    tibble(offset = 0, id = ref_to_xref(source_ref, "S"), tag = "SOUR"),
    tibble(offset = 1, tag = "DATA", value = ""),
    tibble(offset = 2, tag = "EVEN", value = events_recorded),
    tibble(offset = 3, tag = "DATE", value = date_period),
    tibble(offset = 3, tag = "PLAC", value = source_jurisdiction_place),
    tibble(offset = 2, tag = "AGNC", value = responsible_agency),
    data_notes %>% add_offsets(2),
    tibble(offset = 1, tag = "AUTH", value = source_originator),
    tibble(offset = 1, tag = "TITL", value = source_title),
    tibble(offset = 1, tag = "ABBR", value = source_filed_by),
    tibble(offset = 1, tag = "PUBL", value = source_publ_facts),
    tibble(offset = 1, tag = "TEXT", value = source_text),
    source_repo_citation %>% add_offsets(1),
    tibble(offset = 1, tag = "REFN", value = user_ref_number),
    tibble(offset = 2, tag = "TYPE", value = user_ref_type),
    tibble(offset = 1, tag = "RIN", value = automated_record_id),
    change_date %>% add_offsets(1),
    notes %>% add_offsets(1),
    multimedia_links %>% add_offsets(1)
  ) %>% 
    finalise()
  
}



submission_record <- function(submission_ref,
                              submitter_ref = character(),
                              name_of_family_file = character(),
                              temple_code = character(),
                              num_anc_generations = character(),
                              num_des_generations = character(),
                              ordinance_flag = character(),
                              automated_record_id = character(),
                              notes = note_structure(),
                              change_date = change_date()){
 
  
  bind_rows(
    tibble(offset = 0, id = ref_to_xref(submission_ref, "SN"), tag = "SUBN"),
    tibble(offset = 1, tag = "SUBM", value = ref_to_xref(submitter_ref, "U")),
    tibble(offset = 1, tag = "FAMF", value = name_of_family_file),
    tibble(offset = 1, tag = "TEMP", value = temple_code),
    tibble(offset = 1, tag = "ANCE", value = num_anc_generations),
    tibble(offset = 1, tag = "DESC", value = num_des_generations),
    tibble(offset = 1, tag = "ORDI", value = ordinance_flag),
    tibble(offset = 1, tag = "RIN", value = automated_record_id),
    notes %>% add_offsets(1),
    change_date %>% add_offsets(1)
  ) %>% 
    finalise()
  
   
}


submitter_record <- function(submitter_ref,
                             submitter_name = character(),
                             submitter_address = address_structure(character()),
                             multimedia_links = multimedia_link(),
                             language_preference = character(),
                             submitter_registered_rfn = character(),
                             automated_record_id = character(),
                             notes = note_structure(),
                             change_date = change_date()){
  
  bind_rows(
    tibble(offset = 0, id = ref_to_xref(submitter_ref, "U"), tag = "SUBM"),
    tibble(offset = 1, tag = "NAME", value = submitter_name,
    submitter_address %>% add_offsets(1),
    multimedia_links %>% add_offsets(1),
    tibble(offset = 1, tag = "LANG", value = language_preference),
    tibble(offset = 1, tag = "RFN", value = submitter_registered_rfn),
    tibble(offset = 1, tag = "RIN", value = automated_record_id),
    notes %>% add_offsets(1),
    change_date %>% add_offsets(1)
  ) %>% 
    finalise()
  
  
}



footer_section <- function(){
  tibble(offset = 0, id = "TR", tag = "TRLR", value = "") %>% 
    finalise()
}
