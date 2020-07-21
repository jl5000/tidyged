
add_family <- function(gedcom,
                       husband = character(),
                       wife = character(),
                       children = character(),
                       number_of_children = character(),
                       submitter = character(),
                       restriction_notice = character(),
                       user_reference_number = character(),
                       user_reference_type = character(),
                       automated_record_id = character()) {
  
  xref <- assign_xref(xref_prefix_fam(), gedcom = gedcom)
  
  fam_record <- FAMILY_RECORD(xref_fam = xref,
                              restriction_notice = restriction_notice,
                              xref_husb = character(),#TODO lookup
                              xref_wife = character(),
                              xrefs_chil = character(),
                              count_of_children = number_of_children,
                              xrefs_subm = character(),
                              user_reference_number = user_reference_number,
                              user_reference_type = user_reference_type,
                              automated_record_id = automated_record_id) 
  
  gedcom %>% 
    tibble::add_row(fam_record, .before = nrow(.)) %>% 
    set_active_record(xref)
  
  
}


add_family_event <- function(gedcom,
                             family_xref = character(),
                             event_type = character(),
                             event_subtype = character(),
                             event_descriptor = character(),
                             husband_age_at_event = character(),
                             wife_age_at_event = character(),
                             event_date = date_value(),
                             event_cause = character(),
                             event_place = PLACE_STRUCTURE(character()),
                             event_address = ADDRESS_STRUCTURE(character())) {
  
  check_active_record_valid(gedcom, record_string_fam(), is_family)
  
  
}

remove_family <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_fam(), is_family)
  
  
  
  null_active_record(gedcom)  
}


update_family <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_fam(), is_family)
  
  
}
