

add_record_note <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A record must be activated to add a note to it")
  
  if(filter(gedcom, record == attr(gedcom, "active_record")$tag %in% c("NOTE")))
    stop("Notes cannot be added to the active record")
    
    
}


add_record_multimedia_link <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A record must be activated to add a multimedia link to it")
  
  if(filter(gedcom, record == attr(gedcom, "active_record")$tag %in% c("OBJE", "NOTE", "REPO", "SUBN")))
    stop("Multimedia links cannot be added to the active record")
  
  
}

# not for repo/source/subn/subm record
add_record_source_citation <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A record must be activated to add a source citation to it")
  
  if(filter(gedcom, record == attr(gedcom, "active_record")$tag %in% c("REPO", "SOUR", "SUBM", "SUBN")))
    stop("Source citations cannot be added to the active record")
  
  
}

