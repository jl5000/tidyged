
add_repository <- function(gedcom) {
  
  xref <- assign_xref(xref_prefix_repo(), gedcom = gedcom)
  
}

update_repository <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_repo(), is_repository)
  
}

remove_repository <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_repo(), is_repository)
  
  
  
  null_active_record(gedcom)
}
