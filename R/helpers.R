

#' Construct a full personal name
#' 
#' This function constructs a full personal name from individual name pieces.
#'
#' @param prefix The name prefix.
#' @param given The given name(s).
#' @param nickname The nickname.
#' @param surname_prefix The surname prefix.
#' @param surname The surname.
#' @param suffix The name suffix.
#'
#' @return The full name with all name pieces combined.
#' @tests
#' expect_error(construct_full_name(surname_prefix = "de la"))
#' expect_equal(construct_full_name(prefix = "Lt. Cdr."), "Lt. Cdr.")
#' expect_equal(construct_full_name(given = "Joe"), "Joe")
#' expect_equal(construct_full_name(given = "Joe,Adam"), "Joe Adam")
#' expect_equal(construct_full_name(given = "Joey,Joe, Joe"), "Joey Joe Joe")
#' expect_equal(construct_full_name(nickname = "Nobby"), "'Nobby'")
#' expect_equal(construct_full_name(surname = "Bloggs"), "/Bloggs/")
#' expect_equal(construct_full_name(suffix = "Jr."), "Jr.")
#' expect_equal(construct_full_name(suffix = "Jr.,Esq."), "Jr. Esq.")
#' expect_equal(construct_full_name(prefix = "Lt. Cdr.", given = "Joe,Adam", nickname = "Nobby", 
#'                                  surname_prefix = "de la", surname = "Bloggs",
#'                                  suffix = "Jr., Esq."),
#'              "Lt. Cdr. Joe Adam 'Nobby' de la /Bloggs/ Jr. Esq.")
construct_full_name <- function(prefix = character(), 
                                given = character(), 
                                nickname = character(), 
                                surname_prefix = character(), 
                                surname = character(), 
                                suffix = character()) {
  
  if(length(surname_prefix) == 1 & length(surname) == 0)
    stop("Surname prefix given without a surname")
  
  paste(
    prefix,  #up to first comma
    stringr::str_replace_all(given, ", ?", " "), 
    ifelse(length(nickname) == 1, paste0("'", nickname, "'"), ""), #up to first comma
    surname_prefix, 
    ifelse(length(surname) == 1, paste0("/", surname, "/"), ""),
    stringr::str_replace_all(suffix, ", ?", " ")
  ) %>% 
    stringr::str_squish()
  
}


#' Temporarily remove forward slashes from surnames
#'
#' @param gedcom A tidyged object.
#'
#' @return A tidyged object with all forward slashes removed from surnames.
temporarily_remove_name_slashes <- function(gedcom) {
  
  gedcom %>% 
    dplyr::mutate(value = dplyr::if_else(purrr::map_lgl(record, is_indi, gedcom=gedcom) &
                                           tag %in% c("NAME", "FONE", "ROMN"),
                                         stringr::str_remove_all(value, "/"),
                                         value))
  
}


#' Derive a valid cross-reference identifier
#' 
#' Get a valid xref provided explicitly or implicitly (through an identifying attribute or
#' active record).
#' 
#' @details This helper function is designed to derive and run validation checks on an xref
#' provided explicitly or implicitly. An xref is provided implicitly either through the active
#' record of the tidyged object, or through a descriptor identifying a unique record.
#' 
#' The descriptors used for each record are: name (individual, repository, and submitter), 
#' title (source), file reference (multimedia), excerpt (note). 
#' 
#' Once found, the xref is checked to ensure it is of the appropriate type.
#'
#' @param gedcom A tidyged object.
#' @param xref_or_descriptor An xref or descriptor uniquely identifying the record.
#' @param record_type A character string describing the record type. Generally one of
#' the global record_string_* values.
#' @param record_type_fn A function to check the record type. Generally one of the is_*
#' functions.
#'
#' @return A valid xref identifier.
get_valid_xref <- function(gedcom, xref_or_descriptor, record_type, record_type_fn) {
  
  if (length(xref_or_descriptor) == 0 || xref_or_descriptor == "") {
    # xref not given explicitly, get it from active record
    xref <- get_active_record(gedcom)
    
  } else if (grepl(tidyged.internals::xref_pattern(), xref_or_descriptor)) {
    # xref given explicitly
    xref <- xref_or_descriptor
    
  } else {
    # xref given by descriptor, find it
    if (record_type == .pkgenv$record_string_indi) {
      
      xref <- find_xref(gedcom, xrefs_indi(gedcom), c("NAME", "ROMN", "FONE"), xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_famg) {
      
      stop("The selected family record is not valid")
      
    } else if(record_type == .pkgenv$record_string_repo) {
      
      xref <- find_xref(gedcom, xrefs_repo(gedcom), "NAME", xref_or_descriptor) 
      
    } else if(record_type == .pkgenv$record_string_sour) {
      
      xref <- find_xref(gedcom, xrefs_sour(gedcom), "TITL", xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_obje) {
      
      xref <- find_xref(gedcom, xrefs_media(gedcom), "FILE", xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_note) {
      
      xref <- find_xref(gedcom, xrefs_note(gedcom), "NOTE", xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_subm) {
      
      xref <- find_xref(gedcom, xrefs_subm(gedcom), "NAME", xref_or_descriptor)
    }
    
  }

  if(is.null(xref))
    stop("No xref is provided and no ", record_type, " record is activated.")
  
  if(!record_type_fn(gedcom, xref))
    stop("The provided or active record is not a ", record_type, " record")
  
  xref
}



#' Remove multiple records at once
#'
#' @param gedcom A tidyged object.
#' @param xrefs A vector of xrefs to remove.
#'
#' @return An updated tidyged object with the records removed.
#' @export
remove_records <- function(gedcom, xrefs) {
  
  for (xref in xrefs) {
    message(describe_records(gedcom, xref, short_desc = TRUE), " removed")
    
    if (is_indi(gedcom, xref)) {
      gedcom <- remove_indi(gedcom, xref)
    } else if(is_famg(gedcom, xref)) {
      gedcom <- remove_famg(gedcom, xref)
    } else if(is_media(gedcom, xref)) {
      gedcom <- remove_media(gedcom, xref)
    } else if(is_sour(gedcom, xref)) {
      gedcom <- remove_sour(gedcom, xref)
    } else if(is_repo(gedcom, xref)) {
      gedcom <- remove_repo(gedcom, xref)
    } else if(is_note(gedcom, xref)) {
      gedcom <- remove_note(gedcom, xref)
    } else {
      stop("Record ", xref, " is not recognised")
    }
  }
  gedcom
}

