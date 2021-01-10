

#' Flag a record as being active
#' 
#' This allows an easy mechanism to edit particular records without specifying them each time.
#'
#' @param gedcom A tidyged object
#' @param xref The xref of the record to activate
#'
#' @return The same tidyged object with an "active_record" attribute set to the xref of the record
set_active_record <- function(gedcom, xref) {
  if(length(xref) > 0) attr(gedcom, "active_record") <- xref 
  return(gedcom)
}

#' Get the active record in a tidyged object
#' 
#' @param gedcom A tidyged object.
#' @return The xref of the active record.
#'
#' @export
get_active_record <- function(gedcom) {
  attr(gedcom, "active_record")
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
#' @param gedcom A tidyged object
#' @param record_xrefs A list of potential xrefs to consider
#' @param tags The tags to look at when comparing values
#' @param search_pattern A regex pattern to search for
#'
#' @return A single xref for the given record
find_xref <- function(gedcom, record_xrefs, tags, search_pattern) {
  
  if(length(search_pattern) == 0) return(character())
  if(grepl(xref_pattern(), search_pattern)) return(search_pattern)
  
  possibilities <- gedcom %>% 
    temporarily_remove_name_slashes() %>% 
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
#' @param gedcom A tidyged object. 
#' @param individual The xref or name of the Individual record to be activated.
#'
#' @return The same tidyged object with "active_record" attribute set to the specific 
#' Individual record to allow easy editing.
#' @export
activate_individual_record <- function(gedcom, individual) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_individual)
  set_active_record(gedcom, xref)
}


#' Activate a Family Group record
#' 
#' This allows a mechanism to easily edit particular records. For Family group records, 
#' the explicit xref is required. 
#'
#' @param gedcom A tidyged object. 
#' @param family The xref of the Family Group record to be activated.
#'
#' @return The same tidyged object with "active_record" attribute set to the specific 
#' Family group record to allow easy editing.
#' @export
activate_family_group_record <- function(gedcom, family) {
  
  xref <- get_valid_xref(gedcom, family, .pkgenv$record_string_fam, is_family)
  set_active_record(gedcom, xref)
}


#' Activate a Submitter record
#' 
#' This allows a mechanism to easily edit particular records. Either the submitter name can be provided
#' (can be a partial name) or the xref can be provided explicitly. Providing the name instead of the
#' xref can make your code more readable, provided that the name returns a unique record! 
#'
#' @param gedcom A tidyged object. 
#' @param submitter The xref or name of the Submitter record to be activated.
#'
#' @return The same tidyged object with "active_record" attribute set to the specific 
#' Submitter record to allow easy editing.
#' @export
activate_submitter_record <- function(gedcom, submitter) {
  
  xref <- get_valid_xref(gedcom, submitter, .pkgenv$record_string_subm, is_submitter)
  set_active_record(gedcom, xref)
}


#' Activate a Multimedia record
#' 
#' This allows a mechanism to easily edit particular records. Either the file reference can be provided
#' or the xref can be provided explicitly. Providing the file reference instead of the
#' xref can make your code more readable, provided that it returns a unique record! 
#'
#' @param gedcom A tidyged object. 
#' @param multimedia The xref or file reference of the Multimedia record to be activated.
#'
#' @return The same tidyged object with "active_record" attribute set to the specific 
#' Multimedia record to allow easy editing.
#' @export
activate_multimedia_record <- function(gedcom, multimedia) {
  
  xref <- get_valid_xref(gedcom, multimedia, .pkgenv$record_string_obje, is_multimedia)
  set_active_record(gedcom, xref)
}


#' Activate a Note record
#' 
#' This allows a mechanism to easily edit particular records. Either a note excerpt can be provided, 
#' or the xref can be provided explicitly. Providing a note excerpt instead of the
#' xref can make your code more readable, provided that the excerpt returns a unique record! 
#'
#' @param gedcom A tidyged object.
#' @param note The xref or an excerpt of the Note record to be activated.
#'
#' @return The same tidyged object with "active_record" attribute set to the specific 
#' Note record to allow easy editing.
#' @export
activate_note_record <- function(gedcom, note) {
  
  xref <- get_valid_xref(gedcom, note, .pkgenv$record_string_note, is_note)
  set_active_record(gedcom, xref)
}


#' Activate a Source record
#' 
#' This allows a mechanism to easily edit particular records. Either the source title can be provided
#' or the xref can be provided explicitly. Providing the source title instead of the
#' xref can make your code more readable, provided that the name returns a unique record! 
#'
#' @param gedcom A tidyged object.
#' @param source The xref or title of the Source record to be activated.
#'
#' @return The same tidyged object with "active_record" attribute set to the specific 
#' Source record to allow easy editing.
#' @export
activate_source_record <- function(gedcom, source) {
  
  xref <- get_valid_xref(gedcom, source, .pkgenv$record_string_sour, is_source)
  set_active_record(gedcom, xref)
}


#' Activate a Repository record
#' 
#' This allows a mechanism to easily edit particular records. Either the repository name can be provided
#' (can be a partial name) or the xref can be provided explicitly. Providing the name instead of the
#' xref can make your code more readable, provided that the name returns a unique record! 
#'
#' @param gedcom A tidyged object.
#' @param repository The xref or name of the Repository record to be activated.
#'
#' @return The same tidyged object with "active_record" attribute set to the specific 
#' Repository record to allow easy editing.
#' @export
activate_repository_record <- function(gedcom, repository) {
  
  xref <- get_valid_xref(gedcom, repository, .pkgenv$record_string_repo, is_repository)
  set_active_record(gedcom, xref)
  
}
