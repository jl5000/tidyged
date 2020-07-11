
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
  subm_name <- gedcom_value(gedcom, "HD", "SUBM", 1)
  
  paste("GEDCOM file summary:", eol, eol,
        "Submitter:", "\t", "\t", gedcom_value(gedcom, subm_name, "NAME", 1), eol, 
        "Description:", "\t", "\t", gedcom_value(gedcom, "HD", "NOTE", 1), eol,
        "Language:", "\t", "\t", gedcom_value(gedcom, "HD", "LANG", 1), eol,
        "Character Set:", "\t", gedcom_value(gedcom, "HD", "CHAR", 1), eol, eol,
        
        "Copyright:", "\t", "\t", gedcom_value(gedcom, "HD", "COPR", 1), eol, eol,
        
        "Source system:", "\t", gedcom_value(gedcom, "HD", "SOUR", 1), eol,
        "Source system version: ", gedcom_value(gedcom, "HD", "VERS", 2), eol,
        "Product Name:", "\t", "\t", gedcom_value(gedcom, "HD", "NAME", 2), eol,
        "Product Source:", "\t", gedcom_value(gedcom, "HD", "CORP", 2), eol
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

individuals <- function(gedcom) {
  #Tibble with xref, name, sex, DOB, place of birth, mother, father, num_siblings, num_marriages, num_chil, died, place of death
  ind_xrefs <- unique(dplyr::filter(gedcom, tag == "INDI")$id)
  ind_names <- purrr::map_chr(ind_xrefs, gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  ind_sex <- purrr::map_chr(ind_xrefs, gedcom_value, gedcom = gedcom, tag = "SEX", level = 1)
  famc_xrefs <- "???"
  
  tibble::tibble(xref = ind_xrefs,
                 name = ind_names,
                 sex = ind_sex,
                 date_of_birth = 1,
                 place_of_birth = 1,
                 mother = 1, #child_to_family_link
                 father = 1, #child_to_family_link
                 num_siblings = 1, #child_to_family_link
                 num_spouses = 1, #spouse_to_family_link
                 num_children = 1, #spouse_to_family_link
                 date_of_death = 1,
                 place_of_death = 1)
}

families <- function(gedcom) {
  
}