

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




#' Find an xref of a record given a set of terms
#'
#' This is a helper function to identify the xref of a record given a piece of information such
#' as a name or reference number.
#' 
#' Sometimes an xref may be provided directly, in which case it's returned straight back.
#'
#' @param gedcom A tidyged object.
#' @param record_xrefs A list of potential xrefs to consider.
#' @param tags The tags to look at when comparing values.
#' @param search_pattern A set of terms to search for (separated by spaces).
#'
#' @return A single xref for the given record.
find_xref <- function(gedcom, record_xrefs, tags, search_pattern) {
  
  if(length(search_pattern) == 0) return(character())
  if(grepl(tidyged.internals::reg_xref(), search_pattern)) return(search_pattern)
  
  #split up search pattern by spaces
  search_pattern_terms <- unlist(stringr::str_split(search_pattern, " "))
  
  matches <- gedcom$value %>% 
    purrr::map(stringr::str_detect, pattern = search_pattern_terms) %>% 
    purrr::map(all) %>% 
    unlist()
  
  possibilities <- gedcom[gedcom$record %in% record_xrefs &
                            gedcom$tag %in% tags & 
                            matches,]
  
  if(length(unique(possibilities$record)) == 0) {
    
    stop("No records found for the given terms", " (", search_pattern, "). Try supplying the xref explicitly.")
    
  } else if(length(unique(possibilities$record)) > 1) {
    
    choice <- utils::select.list(title = paste("More than one record found for the given terms:", search_pattern),
                                         choices = describe_records(gedcom, unique(possibilities$record)),
                                         multiple = FALSE)
    
    xref <- stringr::str_extract(choice, "@[a-zA-Z0-9]{1,20}@")
    
  } else if(length(unique(possibilities$record)) == 1) {
    
    xref <- unique(possibilities$record)
  }
  
}


#' Activate a record
#' 
#' Set a specific record to be the active record.
#' 
#' @details This allows a mechanism to easily edit particular records. Either a specific record attribute can be provided
#' (terms separated by spaces) or the xref can be provided explicitly. Providing an attribute instead of the
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
activate_indi <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_indi, is_indi)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_famg <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_famg, is_famg)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_subm <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_subm, is_subm)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_media <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_obje, is_media)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_note <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_note, is_note)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_sour <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_sour, is_sour)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_repo <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_repo, is_repo)
  set_active_record(gedcom, xref)
  
}
