


add_individual <- function(gedcom,
                           names = list(),
                           sex = character(),
                           child_to_family_links = list(),
                           spouse_to_family_links = list(),
                           restriction_notice = character(),
                           events = list(),
                           attributes = list(),
                           xrefs_subm = character(),
                           associations = list(),
                           xrefs_alia = character(),
                           xrefs_subm_interested_in_ancestors = character(),
                           xrefs_subm_interested_in_descendents = character(),
                           permanent_record_file_number = character(),
                           ancestral_file_number = character(),
                           user_reference_number = character(),
                           user_reference_type = character(),
                           automated_record_id = character(),
                           notes = list(),
                           source_citations = list(),
                           multimedia_links = list()) {
  
  #add individual record
  #add links to families (child + spouse)
  
  xref <- assign_xref(xref_prefix_indi(), gedcom = gedcom)
  
  ind_record <- INDIVIDUAL_RECORD(xref_indi = assign_xref(xref_prefix_indi(), gedcom = gedcom),
                                  restriction_notice = character(),
                                  names = list(),
                                  sex_value = sex,
                                  events = list(),
                                  attributes = list(),
                                  ordinance = list(),
                                  child_to_family_links = list(),
                                  spouse_to_family_links = list(),
                                  xrefs_subm = character(),
                                  associations = list(),
                                  xrefs_alia = character(),
                                  xrefs_subm_interested_in_ancestors = character(),
                                  xrefs_subm_interested_in_descendents = character(),
                                  permanent_record_file_number = character(),
                                  ancestral_file_number = character(),
                                  user_reference_number = character(),
                                  user_reference_type = character(),
                                  automated_record_id = character(),
                                  date_changed = CHANGE_DATE(),
                                  notes = list(),
                                  source_citations = list(),
                                  multimedia_links = list()) %>% 
    set_active_record(xref)
  
  
}


add_individual_event <- function(gedcom) {
  
  
  
}


add_individual_attribute <- function(gedcom) {
  
  
}

add_individual_association <- function(gedcom) {
  
  
}


add_individual_family_link_as_spouse <- function(gedcom) {
  
  
}

add_individual_family_link_as_child <- function(gedcom) {
  
  
}

remove_individual <- function(gedcom) {
  
  
  
  
}


update_individual <- function(gedcom) {
  
  
  
  
}
