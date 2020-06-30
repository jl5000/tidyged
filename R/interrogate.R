
unique_record_count <- function(gedcom, tag) {sum(gedcom$level == 0 & gedcom$tag == tag)}

num_indi <- function(gedcom) {unique_record_count(gedcom, "INDI")}
num_fam <- function(gedcom) {unique_record_count(gedcom, "FAM")}
num_subm <- function(gedcom) {unique_record_count(gedcom, "SUBM")}
num_media <- function(gedcom) {unique_record_count(gedcom, "OBJE")}
num_note <- function(gedcom) {unique_record_count(gedcom, "NOTE")}
num_repo <- function(gedcom) {unique_record_count(gedcom, "REPO")}
num_sour <- function(gedcom) {unique_record_count(gedcom, "SOUR")}

summary.tidygedcom <- function(gedcom) {
  #SOUR, VERS, NAME, CORP
  #CHAR
  #LANG
  #NOTE
  paste("GEDCOM file summary:\n",
        summary_line(gedcom, "NOTE", 1, "Description:"),
        summary_line(gedcom, "LANG", 1, "Language:"),
        summary_line(gedcom, "DATE", 1, "Date:"),
        summary_line(gedcom, "NAME", 2, "Product Name:")
    
    
  ) %>% cat()
}

str.tidygedcom <- function(gedcom) {
  eol <- "\n"
  gedc_row <- which(gedcom$tag == "GEDC")
  paste0("GEDCOM version ", gedcom$value[gedc_row + 1], " (", gedcom$value[gedc_row + 2], ")", eol, eol,
        "Individuals:\t\t", num_indi(gedcom), eol,
        "Families:\t\t", num_fam(gedcom), eol,
        "Submitters:\t\t", num_subm(gedcom), eol,
        "Multimedia objects:\t", num_media(gedcom), eol, 
        "Notes:\t\t\t", num_note(gedcom), eol,
        "Sources:\t\t", num_sour(gedcom), eol,
        "Repositories:\t\t", num_repo(gedcom), eol 
  ) %>% cat()
}