

add_record_note <- function(gedcom) {
  
  xref <- get_active_record(gedcom)
  
  if(is.null(xref))
    stop("No record is activated. A record must be activated to add a note to it")
  
  if(is_note(gedcom, xref))
    stop("Notes cannot be added to the active record")
    
    
}


add_record_multimedia_link <- function(gedcom) {
  
  xref <- get_active_record(gedcom)
  
  if(is.null(xref))
    stop("No record is activated. A record must be activated to add a multimedia link to it")
  
  if(is_multimedia(gedcom, xref) |
     is_note(gedcom, xref) |
     is_repository(gedcom, xref) |
     is_submission(gedcom, xref))
    stop("Multimedia links cannot be added to the active record")
  
  
}

# not for repo/source/subn/subm record
add_record_source_citation <- function(gedcom) {
  
  xref <- get_active_record(gedcom)
  
  if(is.null(xref))
    stop("No record is activated. A record must be activated to add a source citation to it")
  
  if(is_repository(gedcom, xref) |
     is_source(gedcom, xref) |
     is_submitter(gedcom, xref) |
     is_submission(gedcom, xref))
    stop("Source citations cannot be added to the active record")
  
  
}

