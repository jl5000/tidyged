

# only indi, fam, multi, note
# asso, event detail, pers name pieces, 
add_source_citation_to_record <- function(gedcom) {
  
  xref <- get_active_record(gedcom)
  
  if(is.null(xref))
    stop("No record is activated. A record must be activated to add a source citation to it")
  
  if(is_repository(gedcom, xref) |
     is_source(gedcom, xref) |
     is_submitter(gedcom, xref))
    stop("Source citations cannot be added to the active record")
  
  
}