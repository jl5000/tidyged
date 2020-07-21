
add_note <- function(gedcom) {
  
  xref <- assign_xref(xref_prefix_note(), gedcom = gedcom)
  
}

update_note <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_note(), is_note)
  
}

remove_note <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_note(), is_note)
  
  
  
  null_active_record(gedcom)
}
