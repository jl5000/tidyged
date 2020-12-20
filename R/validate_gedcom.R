
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
  
  validate_header(gedcom)

  if(sum(gedcom$level == 0 & gedcom$tag == "HEAD") != 1) stop("GEDCOM has no single header")
  if(sum(gedcom$level == 0 & gedcom$tag == "TRLR") != 1) stop("GEDCOM has no single trailer")
  if(num_subm(gedcom) > 1) stop("File has more than one submitter record")
  if(sum(gedcom$record == "HD" & gedcom$tag == "SOUR") != 1) warning("GEDCOM header has no single system ID")
  if(sum(gedcom$record == "HD" & gedcom$tag == "SUBM") != 1) warning("GEDCOM header has no single submitter")
  if(sum(gedcom$record == "HD" & gedcom$tag == "GEDC") != 1) warning("GEDCOM header is lacking file information")
  
}


validate_header <- function(gedcom) {
  
  if(!all.equal(gedcom$level[1:6], c(0,1,2,2,3,1)) |
     !all.equal(gedcom$tag[1:6], c("HEAD","GEDC","VERS","FORM","VERS","CHAR")) |
     !all.equal(gedcom$value[1:6], c("", "", "5.5.5", "LINEAGE-LINKED", "5.5.5", "UTF-8")))
    stop("Malformed header")
  
}

