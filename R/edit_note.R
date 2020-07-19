
add_note <- function(gedcom) {
  
  xref <- assign_xref(xref_prefix_note(), gedcom = gedcom)
  
}

update_note <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A note record must be activated to update it")
  
}

remove_note <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A note record must be activated to remove it")
  
}
