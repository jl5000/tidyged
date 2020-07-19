
add_note <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A record must be activated to add a note to it")
  
}


add_multimedia_link <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A record must be activated to add a multimedia link to it")
  
}

add_source_citation <- function(gedcom) {
  
  if(is.null(attr(gedcom, "active_record")))
    stop("No record is activated. A record must be activated to add a source citation to it")
  
}