
add_multimedia <- function(gedcom) {
  
  xref <- assign_xref(xref_prefix_obje(), gedcom = gedcom)
  
}

update_multimedia <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A multimedia record must be activated to update it")
  
  
}

remove_multimedia <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A multimedia record must be activated to remove it")
  
  
}
