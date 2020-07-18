
add_family <- function(gedcom,
                       husband = character(),
                       wife = character(),
                       children = character(),
                       number_of_children = character(),
                       submitter = character(),
                       restriction_notice = character(),
                       user_ref_number = character(),
                       user_ref_type = character(),
                       automated_record_id = character()) {
  
  
  FAMILY_RECORD(xref_fam = assign_xref(xref_prefix_fam(), gedcom = gedcom),
                restriction_notice = restriction_notice,
                xref_husb = character(),
                xref_wife = character(),
                xrefs_chil = character(),
                count_of_children = character(),
                xrefs_subm = character(),
                lds_spouse_sealings = list(),
                user_reference_number = character(),
                user_reference_type = character(),
                automated_record_id = character(),
                date_changed = CHANGE_DATE(),
                notes = list(),
                source_citations = list(),
                multimedia_links = list())
  
  
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
  
  
  
}

remove_family <- function(gedcom) {
  
  
  
}


update_family <- function(gedcom) {
  
  
  
  
}
