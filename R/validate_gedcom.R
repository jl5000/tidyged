
#' Validate a GEDCOM file
#' 
#' Conduct some simple (but not exhaustive) checks on a GEDCOM file
#' 
#' This function is called when importing a GEDCOM file. The checks contained within are relatively
#' simple since there are a wealth of GEDCOM validators already available.
#'
#' @param gedcom A tidygedcom object
#'
#' @return Nothing
validate_gedcom <- function(gedcom) {
  
  if(sum(gedcom$level == 0 & gedcom$tag == "HEAD") != 1) stop("GEDCOM has no single header")
  if(sum(gedcom$level == 0 & gedcom$tag == "TRLR") != 1) stop("GEDCOM has no single trailer")
  if(num_subn(gedcom) > 1) warning("File has more than one submission record")
  if(num_subm(gedcom) == 0) warning("File does not have a submitter defined")
  if(sum(gedcom$record == "HD" & gedcom$tag == "SOUR") != 1) warning("GEDCOM header has no single system ID")
  if(sum(gedcom$record == "HD" & gedcom$tag == "SUBM") != 1) warning("GEDCOM header has no single submitter")
  if(sum(gedcom$record == "HD" & gedcom$tag == "GEDC") != 1) warning("GEDCOM header is lacking file information")
  
}
