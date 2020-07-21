
add_multimedia <- function(gedcom) {
  
  xref <- assign_xref(xref_prefix_obje(), gedcom = gedcom)
  
}

update_multimedia <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_obje(), is_multimedia)
  
  
}

remove_multimedia <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_obje(), is_multimedia)
  
  
  
  
  
  null_active_record(gedcom)
}
