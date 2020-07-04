
unique_record_count <- function(gedcom, tag) {sum(gedcom$level == 0 & gedcom$tag == tag)}

num_indi <- function(gedcom) {unique_record_count(gedcom, "INDI")}
num_fam <- function(gedcom) {unique_record_count(gedcom, "FAM")}
num_subm <- function(gedcom) {unique_record_count(gedcom, "SUBM")}
num_subn <- function(gedcom) {unique_record_count(gedcom, "SUBN")}
num_media <- function(gedcom) {unique_record_count(gedcom, "OBJE")}
num_note <- function(gedcom) {unique_record_count(gedcom, "NOTE")}
num_repo <- function(gedcom) {unique_record_count(gedcom, "REPO")}
num_sour <- function(gedcom) {unique_record_count(gedcom, "SOUR")}

summary.tidygedcom <- function(gedcom) {
  eol <- "\n"
  header <- dplyr::filter(gedcom, id == "HD")
  
  #SOUR, VERS, NAME, CORP
  #CHAR
  #SUBM
  #LANG
  #NOTE
  #COPR
  paste("GEDCOM file summary:", eol, eol,
        summary_line(header, "SUBM", 1, "Submitter:"), eol,
        summary_line(header, "NOTE", 1, "Description:"), eol,
        summary_line(header, "LANG", 1, "Language:"), eol,
        summary_line(header, "CHAR", 1, "Character Set:"), eol,
        
        summary_line(header, "COPR", 1, "Copyright:"), eol,
        
        summary_line(header, "SOUR", 1, "Source system:"), eol,
        summary_line(header, "VERS", 2, "Source system version:"), eol,
        summary_line(header, "NAME", 2, "Product Name:"), eol,
        summary_line(header, "CORP", 2, "Product Source:"), eol
  ) %>% cat()
}

str.tidygedcom <- function(gedcom) {
  eol <- "\n"
  gedc_row <- which(gedcom$tag == "GEDC")
  paste0("GEDCOM version ", gedcom$value[gedc_row + 1], " (", gedcom$value[gedc_row + 2], ")", eol, eol,
        "Individuals:\t\t", num_indi(gedcom), eol,
        "Families:\t\t", num_fam(gedcom), eol,
        "Submitters:\t\t", num_subm(gedcom), eol,
        "Submissions:\t\t", num_subn(gedcom), eol,
        "Multimedia objects:\t", num_media(gedcom), eol, 
        "Notes:\t\t\t", num_note(gedcom), eol,
        "Sources:\t\t", num_sour(gedcom), eol,
        "Repositories:\t\t", num_repo(gedcom), eol 
  ) %>% cat()
}