
add_family <- function(gedcom,
                       husband = character(),
                       wife = character(),
                       children = character(),
                       number_of_children = character(),
                       submitters = character(),
                       restriction_notice = character(),
                       user_reference_number = character(),
                       user_reference_type = character(),
                       automated_record_id = character()) {
  
  xref <- assign_xref(xref_prefix_fam(), gedcom = gedcom)
  
  xref_husb <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), husband)
  xref_wife <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), wife)
  xrefs_chil <- purrr::map_chr(children, find_xref,
                               gedcom = gedcom, record_xrefs = xrefs_individuals(gedcom), 
                               tags = c("NAME", "ROMN", "FONE"))
  xrefs_subm <- purrr::map_chr(submitters, find_xref, 
                               gedcom = gedcom, record_xrefs = xrefs_submitters(gedcom), tags = "NAME")
  
  fam_record <- FAMILY_RECORD(xref_fam = xref,
                              restriction_notice = restriction_notice,
                              xref_husb = xref_husb,
                              xref_wife = xref_wife,
                              xrefs_chil = xrefs_chil,
                              count_of_children = number_of_children,
                              xrefs_subm = xrefs_subm,
                              user_reference_number = user_reference_number,
                              user_reference_type = user_reference_type,
                              automated_record_id = automated_record_id) 
  
  temp <- gedcom %>%
    tibble::add_row(fam_record, .before = nrow(.)) %>%
    set_active_record(xref_husb) %>% 
    add_individual_family_link_as_spouse(xref) %>%
    set_active_record(xref_wife) %>% 
    add_individual_family_link_as_spouse(xref)
  
  for(i in seq_along(xrefs_chil)) {
    
    temp <- temp %>% 
      set_active_record(xrefs_chil[i]) %>% 
      add_individual_family_link_as_child(xref)    
  }
  
  set_active_record(temp, xref)
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
