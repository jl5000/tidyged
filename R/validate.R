

check_gedcom_file <- function(gedcom) {
  
  
  
}


validate_input_size <- function(input, max_dim, min_char = NULL, max_char = NULL) {
  if (length(input) > max_dim)
    stop("Input ", input, " has too many dimensions. The limit is ", max_dim)
  
  if (length(input) > 0 && !is.null(max_char) && max(nchar(input)) > max_char)
    stop("Input ", input, " has too many characters. The limit is ", max_char)
  
  if (length(input) > 0 && !is.null(min_char) && min(nchar(input)) < min_char)
    stop("Input ", input, " has too few characters. The minimum is ", min_char)
}

check_gedcom_line_size <- function() {
  
}


validate_input <- function(input, choices) {
  if (length(input) == 1 && input %nin% choices) 
    stop("Invalid argument value: ", input, ".\n  The valid values are: ", 
         paste(choices, collapse = ", "))
}

check_pedigree_linkage_type <- function(input) {
  
  choices <- c("adopted", "birth", "foster", "sealing")
  validate_input(input, choices)
}

check_child_linkage_status <- function(input) {
  
  choices <- c("challenged", "disproven", "proven")
  validate_input(input, choices)
}

check_restriction_notice <- function(input) {
  
  choices <- c("confidential", "locked", "privacy")
  validate_input(input, choices)
}

check_family_event <- function(input) {
  
  choices <- c("ANUL", "CENS", "DIV", "DIVF",
               "ENGA", "MARB", "MARC", "MARR",
               "MARL", "MARS", "RESI", "EVEN")
  validate_input(input, choices)
}

check_individual_attribute <- function(input) {
  
  choices <- c("CAST", "DSCR", "EDUC", "IDNO",
               "NATI", "NCHI", "NMR", "OCCU",
               "PROP", "RELI", "RESI", "SSN",
               "TITL", "FACT")
  validate_input(input, choices)
}

check_individual_event <- function(input) {
  
  choices <- c("BIRT", "CHR", "DEAT", "BURI", "CREM",
               "ADOP", "BAPM", "BARM", "BASM", "BLES",
               "CHRA", "CONF", "FCOM", "ORDN", "NATU",
               "EMIG", "IMMI", "CENS", "PROB", "WILL",
               "GRAD", "RETI", "EVEN")
  validate_input(input, choices)
}

check_adoptive_parent <- function(input) {
  
  choices <- c("HUSB", "WIFE", "BOTH")
  validate_input(input, choices)
}

check_media_format <- function(input) {
  
  choices <- c( "bmp", "gif", "jpg", "ole", "pcx", "tif", "wav")
  validate_input(input, choices)
}

check_media_type <- function(input) {
  
  choices <- c("audio", "book", "card", "electronic", "fiche", 
               "film", "magazine", "manuscript", "map", 
               "newspaper", "photo", "tombstone", "video")
  validate_input(input, choices)
}

check_certainty_assessment <- function(input) {
  
  choices <- as.character(0:3)
  validate_input(input, choices)
}

check_character_set <- function(input) {
  
  choices <- c("ANSEL", "UTF-8", "UNICODE", "ASCII")
  validate_input(input, choices)
}

check_sex <- function(input) {
  
  choices <- c("M", "F", "U")
  validate_input(input, choices)
}

check_ordinance_flag <- function(input) {
  
  choices <- c("yes", "no")
  validate_input(input, choices)
}






