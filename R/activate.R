

#' Flag a record as being active
#' 
#' This allows an easy mechanism to edit particular records without specifying them each time.
#'
#' @param gedcom A tidygedcom object
#' @param xref The xref of the record to activate
#'
#' @return The same tidygedcom object with an "active_record" attribute set to the xref of the record
set_active_record <- function(gedcom, xref) {
  if(length(xref) > 0) attr(gedcom, "active_record") <- xref 
  return(gedcom)
}
get_active_record <- function(gedcom) {
  attr(gedcom, "active_record")
}

check_active_record_valid <- function(gedcom, record_type, record_type_fn) {
  
  xref <- get_active_record(gedcom)
  
  if(is.null(xref))
    stop("No record is activated. A ", record_type, " record must be activated to update or remove it")
  
  if(!record_type_fn(gedcom, xref))
    stop("The active record is not a ", record_type, " record")
  
}

null_active_record <- function(gedcom) {
  attr(gedcom, "active_record") <- NULL
  return(gedcom)
}




#' Find an xref of a record given a particular regex pattern.
#'
#' This is a helper function to identify the xref of a record given a piece of information such
#' as a name or reference number.
#' 
#' Sometimes an xref may be provided directly, in which case it's returned straight back.
#'
#' @param gedcom A tidygedcom object
#' @param record_xrefs A list of potential xrefs to consider
#' @param tags The tags to look at when comparing values
#' @param search_pattern A regex pattern to search for
#'
#' @return A single xref for the given record
find_xref <- function(gedcom, record_xrefs, tags, search_pattern) {
  
  if(length(search_pattern) == 0) return(character())
  if(grepl(xref_pattern(), search_pattern)) return(search_pattern)
  
  possibilities <- gedcom %>% 
    dplyr::filter(record %in% record_xrefs) %>% 
    dplyr::filter(tag %in% tags) %>% 
    dplyr::filter(stringr::str_detect(value, search_pattern))
  
  if(length(unique(possibilities$record)) == 0) {
    
    stop("No records found for the given regex", " (", search_pattern, "). Try supplying the xref explicitly.")
    
  } else if(length(unique(possibilities$record)) > 1) {
    
    stop("More than one record found for the given regex: ",
         paste(unique(possibilities$value), collapse = ", "),
         ". \nTry being more specific or supplying the xref explicitly.")
    
  } else if(length(unique(possibilities$record)) == 1) {
    
    xref <- unique(possibilities$record)
  }
  
}


#' Activate an Individual record
#' 
#' This allows a mechanism to easily edit particular records. Either the individual name can be provided
#' (can be a regex pattern) or the xref can be provided explicitly. Providing the name instead of the
#' xref can make your code more readable, provided that the name returns a unique record! 
#'
#' @param gedcom A tidygedcom object 
#' @param individual_name The name of the individual whose record should be activated
#' @param xref The xref of the Individual record to activate
#'
#' @return The same tidygedcom object with "active_record" attribute set to the specific 
#' Individual record to allow easy editing.
#' @export
activate_individual_record <- function(gedcom, 
                                       individual_name = character(),
                                       xref = character()) {
  
  if(length(xref) == 0) {
    
    xref <- find_xref(gedcom, 
                      xrefs_individuals(gedcom), 
                      c("NAME", "ROMN", "FONE"), 
                      individual_name)
  }
  
  set_active_record(gedcom, xref)
}


#' Activate a Family record
#' 
#' This allows a mechanism to easily edit particular records. For Family records, the explicit xref 
#' is required. 
#'
#' @param gedcom A tidygedcom object 
#' @param xref The xref of the Family record to activate
#'
#' @return The same tidygedcom object with "active_record" attribute set to the specific 
#' Family record to allow easy editing.
#' @export
activate_family_record <- function(gedcom, 
                                   xref) {
  
  set_active_record(gedcom, xref)
}


#' Activate a Submitter record
#' 
#' This allows a mechanism to easily edit particular records. Either the submitter name can be provided
#' (can be a partial name) or the xref can be provided explicitly. Providing the name instead of the
#' xref can make your code more readable, provided that the name returns a unique record! 
#'
#' @param gedcom A tidygedcom object 
#' @param submitter_name The name of the submitter whose record should be activated
#' @param xref The xref of the Submitter record to activate
#'
#' @return The same tidygedcom object with "active_record" attribute set to the specific 
#' Submitter record to allow easy editing.
#' @export
activate_submitter_record <- function(gedcom, 
                                      submitter_name = character(),
                                      xref = character()) {
  
  if(length(xref) == 0) {
    
    xref <- find_xref(gedcom, 
                      xrefs_submitters(gedcom), 
                      "NAME", 
                      submitter_name)
  }
  
  set_active_record(gedcom, xref)
}


#' Activate a Multimedia record
#' 
#' This allows a mechanism to easily edit particular records. Either the file reference can be provided
#' or the xref can be provided explicitly. Providing the file reference instead of the
#' xref can make your code more readable, provided that it returns a unique record! 
#'
#' @param gedcom A tidygedcom object 
#' @param file_reference The file reference of the Multimedia record to be activated
#' @param xref The xref of the Multimedia record to activate
#'
#' @return The same tidygedcom object with "active_record" attribute set to the specific 
#' Multimedia record to allow easy editing.
#' @export
activate_multimedia_record <- function(gedcom, 
                                       file_reference = character(),
                                       xref = character()) {
  
  if(length(xref) == 0) {
    
    xref <- find_xref(gedcom, 
                      xrefs_multimedia(gedcom), 
                      "FILE", 
                      file_reference)
  }
  
  set_active_record(gedcom, xref)
}


#' Activate a Note record
#' 
#' This allows a mechanism to easily edit particular records. Either a note excerpt can be provided, 
#' or the xref can be provided explicitly. Providing a note excerpt instead of the
#' xref can make your code more readable, provided that the excerpt returns a unique record! 
#'
#' @param gedcom A tidygedcom object 
#' @param note_excerpt An excerpt of the Note record to be activated
#' @param xref The xref of the Note record to activate
#'
#' @return The same tidygedcom object with "active_record" attribute set to the specific 
#' Note record to allow easy editing.
#' @export
activate_note_record <- function(gedcom, 
                                 note_excerpt = character(),
                                 xref = character()) {
  
  if(length(xref) == 0) {
    
    xref <- find_xref(gedcom, 
                      xrefs_notes(gedcom), 
                      c("NOTE", "CONT", "CONC"), 
                      note_excerpt)
  }
  
  set_active_record(gedcom, xref)
}


#' Activate a Source record
#' 
#' This allows a mechanism to easily edit particular records. Either the source title can be provided
#' or the xref can be provided explicitly. Providing the source title instead of the
#' xref can make your code more readable, provided that the name returns a unique record! 
#'
#' @param gedcom A tidygedcom object 
#' @param source_title The title of the Source record to be activated
#' @param xref The xref of the Source record to activate
#'
#' @return The same tidygedcom object with "active_record" attribute set to the specific 
#' Source record to allow easy editing.
#' @export
activate_source_record <- function(gedcom, 
                                   source_title = character(),
                                   xref = character()) {
  
  if(length(xref) == 0) {
    
    xref <- find_xref(gedcom, 
                      xrefs_sources(gedcom), 
                      "TITL", 
                      source_title)
  }
  
  set_active_record(gedcom, xref)
}


#' Activate a Repository record
#' 
#' This allows a mechanism to easily edit particular records. Either the repository name can be provided
#' (can be a partial name) or the xref can be provided explicitly. Providing the name instead of the
#' xref can make your code more readable, provided that the name returns a unique record! 
#'
#' @param gedcom A tidygedcom object 
#' @param repository_name The name of the repository whose record should be activated
#' @param xref The xref of the Repository record to activate
#'
#' @return The same tidygedcom object with "active_record" attribute set to the specific 
#' Repository record to allow easy editing.
#' @export
activate_repository_record <- function(gedcom, 
                                       repository_name = character(),
                                       xref = character()) {
  
  if(length(xref) == 0) {
    
    xref <- find_xref(gedcom, 
                      xrefs_repositories(gedcom), 
                      "NAME", 
                      repository_name) 
  }
  
  set_active_record(gedcom, xref)
}
