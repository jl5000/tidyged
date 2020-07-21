
add_source <- function(gedcom) {
  
  xref <- assign_xref(xref_prefix_sour(), gedcom = gedcom)
  
}

update_source <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_sour(), is_source)
  
}

remove_source <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_sour(), is_source)
  
  
  
  
  
  null_active_record(gedcom)
}
