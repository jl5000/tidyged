

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




#' Find an xref of a record given a particular regex pattern
#'
#' This is a helper function to identify the xref of a record given a piece of information such
#' as a name or reference number.
#' 
#' Sometimes an xref may be provided directly, in which case it's returned straight back.
#'
#' @param gedcom A tidyged object.
#' @param record_xrefs A list of potential xrefs to consider.
#' @param tags The tags to look at when comparing values.
#' @param search_pattern A regex pattern to search for.
#'
#' @return A single xref for the given record
find_xref <- function(gedcom, record_xrefs, tags, search_pattern) {
  
  if(length(search_pattern) == 0) return(character())
  if(grepl(xref_pattern(), search_pattern)) return(search_pattern)
  
  possibilities <- gedcom %>% 
    temporarily_remove_name_slashes() %>% 
    dplyr::filter(record %in% record_xrefs, tag %in% tags, stringr::str_detect(value, search_pattern))
  
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


#' Activate a record
#' 
#' Set a specific record to be the active record.
#' 
#' @details This allows a mechanism to easily edit particular records. Either a specific record attribute can be provided
#' (can be a regex pattern) or the xref can be provided explicitly. Providing an attribute instead of the
#' xref can make your code more readable, provided that it returns a unique record! 
#' 
#' The attributes that can be provided for each type of record are:
#' 
#' - Individuals: Full name (NAME tag), phonetic variation (FONE), romanised variation (ROMN);
#' - Multimedia: File reference (FILE);
#' - Note: User text (NOTE);
#' - Source: Title (TITL);
#' - Repository: Name (NAME).
#'
#' @param gedcom A tidyged object. 
#' @param record The xref or attribute of the record to be activated.
#'
#' @return The same tidyged object with "active_record" attribute set to the specific 
#' record to allow easy editing.
#' @export
activate_individual_record <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_indi, is_individual)
  set_active_record(gedcom, xref)
}


#' @rdname activate_individual_record
#' @export
activate_family_group_record <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_fam, is_family)
  set_active_record(gedcom, xref)
}


#' @rdname activate_individual_record
#' @export
activate_submitter_record <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_subm, is_submitter)
  set_active_record(gedcom, xref)
}


#' @rdname activate_individual_record
#' @export
activate_multimedia_record <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_obje, is_multimedia)
  set_active_record(gedcom, xref)
}


#' @rdname activate_individual_record
#' @export
activate_note_record <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_note, is_note)
  set_active_record(gedcom, xref)
}


#' @rdname activate_individual_record
#' @export
activate_source_record <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_sour, is_source)
  set_active_record(gedcom, xref)
}


#' @rdname activate_individual_record
#' @export
activate_repository_record <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_repo, is_repository)
  set_active_record(gedcom, xref)
  
}
