
add_repository <- function(gedcom) {
  
  xref <- assign_xref(xref_prefix_repo(), gedcom = gedcom)
  
}

update_repository <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A repository record must be activated to update it")
  
}

remove_repository <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A repository record must be activated to remove it")
  
}
