

unique_record_count <- function(gedcom, tag) {sum(gedcom$level == 0 & gedcom$tag == tag)}

#' Get the number of records in a tidyged object
#'
#' These functions return the number of records of a particular type in a tidyged object.
#'
#' @param gedcom A tidyged object.
#'
#' @return The number of records of the relevant type.
#' @export
num_indi <- function(gedcom) { unique_record_count(gedcom, .pkgenv$record_tag_indi) }

#' @export
#' @rdname num_indi
num_famg <- function(gedcom) { unique_record_count(gedcom, .pkgenv$record_tag_famg) }

#' @export
#' @rdname num_indi
num_subm <- function(gedcom) { unique_record_count(gedcom, .pkgenv$record_tag_subm) }

#' @export
#' @rdname num_indi
num_media <- function(gedcom) { unique_record_count(gedcom, .pkgenv$record_tag_obje) }

#' @export
#' @rdname num_indi
num_note <- function(gedcom) { unique_record_count(gedcom, .pkgenv$record_tag_note) }

#' @export
#' @rdname num_indi
num_repo <- function(gedcom) { unique_record_count(gedcom, .pkgenv$record_tag_repo) }

#' @export
#' @rdname num_indi
num_sour <- function(gedcom) { unique_record_count(gedcom, .pkgenv$record_tag_sour) }



is_record_type <- function(gedcom, xref, tag) {
  gedcom[gedcom$record == xref,]$tag[1] == tag
}

#' Check whether a given record is a particular type
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the record.
#'
#' @return A logical indicating whether the record is of a particular type.
#' @export
is_indi <- function(gedcom, xref) { is_record_type(gedcom, xref, .pkgenv$record_tag_indi) }

#' @export
#' @rdname is_indi
is_famg <- function(gedcom, xref) { is_record_type(gedcom, xref, .pkgenv$record_tag_famg) }

#' @export
#' @rdname is_indi
is_subm <- function(gedcom, xref)  { is_record_type(gedcom, xref, .pkgenv$record_tag_subm) }

#' @export
#' @rdname is_indi
is_repo <- function(gedcom, xref) { is_record_type(gedcom, xref, .pkgenv$record_tag_repo) }

#' @export
#' @rdname is_indi
is_media <- function(gedcom, xref) { is_record_type(gedcom, xref, .pkgenv$record_tag_obje) }

#' @export
#' @rdname is_indi
is_note <- function(gedcom, xref)       { is_record_type(gedcom, xref, .pkgenv$record_tag_note) }

#' @export
#' @rdname is_indi
is_sour <- function(gedcom, xref)     { is_record_type(gedcom, xref, .pkgenv$record_tag_sour) }
