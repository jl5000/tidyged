
add_source <- function(gedcom) {
  
  xref <- assign_xref(xref_prefix_sour(), gedcom = gedcom)
  
}

update_source <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A source record must be activated to update it")
  
}

remove_source <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A source record must be activated to remove it")
  
}
