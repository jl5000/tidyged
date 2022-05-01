# Stop the NOTES from R CMD CHECK when creating columns with mutate()
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("level", "record", "tag", "value", ".", "num_children", "xref",
                           "mother_xref", "father_xref", "num_siblings", "full", "n"))


.pkgenv <- new.env(parent=emptyenv())

.pkgenv$record_tag_indi <- "INDI"
.pkgenv$record_tag_famg <- "FAM"
.pkgenv$record_tag_subm <- "SUBM"
.pkgenv$record_tag_repo <- "REPO"
.pkgenv$record_tag_obje <- "OBJE"
.pkgenv$record_tag_note <- "NOTE"
.pkgenv$record_tag_sour <- "SOUR"

.pkgenv$record_string_indi <- "Individual"
.pkgenv$record_string_famg <- "Family group"
.pkgenv$record_string_subm <- "Submitter"
.pkgenv$record_string_repo <- "Repository"
.pkgenv$record_string_obje <- "Multimedia"
.pkgenv$record_string_note <- "Note"
.pkgenv$record_string_sour <- "Source"



##' Return the current date in DATE_EXACT format
##' 
##' See \code{tidyged.internals::\link[tidyged.internals:date_current]{date_current}} for details.
##' 
##' @importFrom tidyged.internals date_current
##' @name date_current
##' @export
NULL

##' Construct a DATE_EXACT string
##' 
##' See \code{tidyged.internals::\link[tidyged.internals:date_exact]{date_exact}} for details.
##' 
##' @importFrom tidyged.internals date_exact
##' @name date_exact
##' @export
NULL

##' Construct a DATE_PERIOD string
##' 
##' See \code{tidyged.internals::\link[tidyged.internals:date_period]{date_period}} for details.
##' 
##' @importFrom tidyged.internals date_period
##' @name date_period
##' @export
NULL

##' Construct a DATE_RANGE string
##' 
##' See \code{tidyged.internals::\link[tidyged.internals:date_range]{date_range}} for details.
##' 
##' @importFrom tidyged.internals date_range
##' @name date_range
##' @export
NULL

##' Construct a DATE_CALENDAR string
##' 
##' See \code{tidyged.internals::\link[tidyged.internals:date_calendar]{date_calendar}} for details.
##' 
##' @importFrom tidyged.internals date_calendar
##' @name date_calendar
##' @export
NULL

##' Construct a DATE_APPROXIMATED string
##' 
##' See \code{tidyged.internals::\link[tidyged.internals:date_approximated]{date_approximated}} for details.
##' 
##' @importFrom tidyged.internals date_approximated
##' @name date_approximated
##' @export
NULL

