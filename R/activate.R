

#' Flag a record as being active
#' 
#' This allows an easy mechanism to edit particular records without specifying them each time.
#'
#' @param gedcom A tidygedcom object
#' @param xref The xref of the record to activate
#'
#' @return The same tidygedcom object with an "active_record" attribute set to the xref of the record
set_active_record <- function(gedcom, xref) {
  attr(gedcom, "active_record") <- xref
  gedcom
}



#' Find an xref of a record given a particular value.
#'
#' This is a helper function to identify the xref of a record given a piece of information such
#' as a name or reference number.
#'
#' @param gedcom A tidygedcom object
#' @param record_ids A list of potential xrefs to consider
#' @param tags The tags to look at when comparing values
#' @param search_string The identifying information to search for
#' @param allow_broken Whether to allow the search_string to be matched piece-wise or exactly. 
#' For example, if TRUE, "Joe Bloggs" will match "Joe Ian Bloggs".
#'
#' @return A single xref for the given record
find_xref <- function(gedcom, record_ids, tags, search_string, allow_broken = FALSE) {
  
  if(allow_broken) search_string <- stringr::str_replace_all(search_string, " ", ".*")
  
  possibilities <- gedcom %>% 
    dplyr::filter(id %in% record_ids) %>% 
    dplyr::filter(tag %in% tags) %>% 
    dplyr::filter(stringr::str_detect(value, search_string))
  
  if(length(unique(possibilities$id)) == 0) {
    
    stop("Record activation failed - no records found. Try supplying the xref explicitly.")
    
  } else if(length(unique(possibilities$id)) > 1) {
    
    stop("Record activation failed - more than one record found: ",
         paste(unique(possibilities$value), collapse = ", "),
         ". \nTry being more specific or supplying the xref explicitly.")
    
  } else if(length(unique(possibilities$id)) == 1) {
    
    xref <- unique(possibilities$id)
  }
  
}


#' Activate an Individual record
#' 
#' This allows a mechanism to easily edit particular records. Either the individual name can be provided
#' (can be a partial name) or the xref can be provided explicitly. Providing the name instead of the
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
    
    individuals <- dplyr::filter(gedcom, level == 0 & tag == "INDI")$id
    
    xref <- find_xref(gedcom, 
                      individuals, 
                      c("NAME", "ROMN", "FONE"), 
                      individual_name,
                      allow_broken = TRUE)
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
    
    submitters <- dplyr::filter(gedcom, level == 0 & tag == "SUBM")$id
    
    xref <- find_xref(gedcom, 
                      submitters, 
                      "NAME", 
                      submitter_name,
                      allow_broken = TRUE)
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
    
    objects <- dplyr::filter(gedcom, level == 0 & tag == "OBJE")$id
    
    xref <- find_xref(gedcom, 
                      objects, 
                      "FILE", 
                      file_reference,
                      allow_broken = FALSE)
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
    
    notes <- dplyr::filter(gedcom, level == 0 & tag == "NOTE")$id
    
    xref <- find_xref(gedcom, 
                      notes, 
                      c("NOTE", "CONT", "CONC"), 
                      note_excerpt,
                      allow_broken = FALSE)
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
    
    sources <- dplyr::filter(gedcom, level == 0 & tag == "SOUR")$id
    
    xref <- find_xref(gedcom, 
                      sources, 
                      "TITL", 
                      source_title,
                      allow_broken = FALSE)
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
    
    repositories <- dplyr::filter(gedcom, level == 0 & tag == "REPO")$id
    
    xref <- find_xref(gedcom, 
                      repositories, 
                      "NAME", 
                      repository_name,
                      allow_broken = FALSE) 
  }
  
  set_active_record(gedcom, xref)
}
