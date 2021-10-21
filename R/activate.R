

#' Flag a record as being active
#' 
#' This allows an easy mechanism to edit particular records without specifying them each time.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the record to activate.
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
active_record <- function(gedcom) {
  attr(gedcom, "active_record")
}


null_active_record <- function(gedcom) {
  attr(gedcom, "active_record") <- NULL
  return(gedcom)
}


#' Activate a record
#' 
#' Set a specific record to be the active record.
#'
#' @param gedcom A tidyged object. 
#' @param record The xref of the record to be activated.
#'
#' @return The same tidyged object with "active_record" attribute set to the specific 
#' record to allow easy editing.
#' @export
activate_indi <- function(gedcom, record) {
  
  xref <- queryged::get_valid_xref(gedcom, record, .pkgenv$record_string_indi, is_indi)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_famg <- function(gedcom, record) {
  
  xref <- queryged::get_valid_xref(gedcom, record, .pkgenv$record_string_famg, is_famg)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_subm <- function(gedcom, record) {
  
  xref <- queryged::get_valid_xref(gedcom, record, .pkgenv$record_string_subm, is_subm)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_media <- function(gedcom, record) {
  
  xref <- queryged::get_valid_xref(gedcom, record, .pkgenv$record_string_obje, is_media)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_note <- function(gedcom, record) {
  
  xref <- queryged::get_valid_xref(gedcom, record, .pkgenv$record_string_note, is_note)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_sour <- function(gedcom, record) {
  
  xref <- queryged::get_valid_xref(gedcom, record, .pkgenv$record_string_sour, is_sour)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_repo <- function(gedcom, record) {
  
  xref <- queryged::get_valid_xref(gedcom, record, .pkgenv$record_string_repo, is_repo)
  set_active_record(gedcom, xref)
  
}
