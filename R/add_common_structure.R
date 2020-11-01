

add_note_to_record <- function(gedcom) {
  
  xref <- get_active_record(gedcom)
  
  if(is.null(xref))
    stop("No record is activated. A record must be activated to add a note to it")
  
  if(is_note(gedcom, xref))
    stop("Notes cannot be added to the active record")
    
  #record
  #source DATA
  #indi association
  #indi links
  #event_detail
  #name_pieces
  #place_structure
  #source_citation
  #source_repository_citation
    
}



# not for repo/source/subn/subm record
add_source_citation_to_record <- function(gedcom) {
  
  xref <- get_active_record(gedcom)
  
  if(is.null(xref))
    stop("No record is activated. A record must be activated to add a source citation to it")
  
  if(is_repository(gedcom, xref) |
     is_source(gedcom, xref) |
     is_submitter(gedcom, xref) |
     is_submission(gedcom, xref))
    stop("Source citations cannot be added to the active record")
  
  
}

