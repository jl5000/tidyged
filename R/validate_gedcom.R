
validate_gedcom <- function(gedcom) {
  
  if(sum(gedcom$level == 0 & gedcom$tag == "HEAD") != 1) stop("GEDCOM has no single header")
  if(sum(gedcom$level == 0 & gedcom$tag == "TRLR") != 1) stop("GEDCOM has no single trailer")
  if(num_subn(gedcom) > 1) stop("File has more than one submission record")
  
  
  
}
