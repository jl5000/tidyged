
unique_record_count <- function(gedcom, tag) {sum(gedcom$level == 0 & gedcom$tag == tag)}

num_indi <- function(gedcom) {unique_record_count(gedcom, record_tag_indi())}
num_fam <- function(gedcom) {unique_record_count(gedcom, record_tag_fam())}
num_subm <- function(gedcom) {unique_record_count(gedcom, record_tag_subm())}
num_subn <- function(gedcom) {unique_record_count(gedcom, record_tag_subn())}
num_media <- function(gedcom) {unique_record_count(gedcom, record_tag_obje())}
num_note <- function(gedcom) {unique_record_count(gedcom, record_tag_note())}
num_repo <- function(gedcom) {unique_record_count(gedcom, record_tag_repo())}
num_sour <- function(gedcom) {unique_record_count(gedcom, record_tag_sour())}


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
  
  ind_xrefs <- unique(dplyr::filter(gedcom, tag == "INDI", level == 0)$record)
  ind_names <- purrr::map_chr(ind_xrefs, gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  ind_sex <- purrr::map_chr(ind_xrefs, gedcom_value, gedcom = gedcom, tag = "SEX", level = 1)
  ind_dobs <- purrr::map_chr(ind_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "BIRT")
  ind_pobs <- purrr::map_chr(ind_xrefs, gedcom_value, gedcom = gedcom, tag = "PLAC", level = 2, after_tag = "BIRT")
  ind_dods <- purrr::map_chr(ind_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "DEAT")
  ind_pods <- purrr::map_chr(ind_xrefs, gedcom_value, gedcom = gedcom, tag = "PLAC", level = 2, after_tag = "DEAT")
  ind_famc <- purrr::map_chr(ind_xrefs, gedcom_value, gedcom = gedcom, tag = "FAMC", level = 1)
  #ind_fams <- purrr::map_chr(ind_xrefs, gedcom_value, gedcom = gedcom, tag = "FAMS", level = 1)
  moth_xref <- purrr::map_chr(ind_famc, gedcom_value, gedcom = gedcom, tag = "WIFE", level = 1) 
  ind_moth <- purrr::map_chr(moth_xref, gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  fath_xref <- purrr::map_chr(ind_famc, gedcom_value, gedcom = gedcom, tag = "HUSB", level = 1) 
  ind_fath <- purrr::map_chr(fath_xref, gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  ind_sibl <- purrr::map_chr(ind_famc, gedcom_value, gedcom = gedcom, tag = "NCHI", level = 1)
  date_chan <- purrr::map_chr(ind_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = ind_xrefs,
                 name = stringr::str_remove_all(ind_names, "/"),
                 sex = ind_sex,
                 date_of_birth = ind_dobs,
                 place_of_birth = ind_pobs,
                 date_of_death = ind_dods,
                 place_of_death = ind_pods,
                 mother_xref = moth_xref,
                 mother = stringr::str_remove_all(ind_moth, "/"),
                 father_xref = fath_xref,
                 father = stringr::str_remove_all(ind_fath, "/"),
                 num_siblings = ind_sibl) %>% 
    dplyr::add_count(mother_xref, father_xref, name = "full") %>%
    dplyr::mutate(num_siblings = ifelse(num_siblings == "", 
                                        as.character(full - 1),
                                        as.character(as.integer(num_siblings) - 1)),
                  num_siblings = ifelse(mother_xref == "" | father_xref == "",
                                        "", num_siblings)) %>% 
    dplyr::mutate(num_children = stringr::str_count(paste(moth_xref,collapse = "|"), xref) +
                    stringr::str_count(paste(fath_xref,collapse = "|"), xref),
                  last_modified = date_chan) %>% 
    dplyr::select(-full, -mother_xref, -father_xref)
}

families <- function(gedcom) {
  
  fam_xrefs <- unique(dplyr::filter(gedcom, tag == "FAM", level == 0)$record)
  husb_xrefs <- purrr::map_chr(fam_xrefs, gedcom_value, gedcom = gedcom, tag = "HUSB", level = 1)
  wife_xrefs <- purrr::map_chr(fam_xrefs, gedcom_value, gedcom = gedcom, tag = "WIFE", level = 1)
  husb_names <- purrr::map_chr(husb_xrefs, gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  wife_names <- purrr::map_chr(wife_xrefs, gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  marr_dates <- purrr::map_chr(fam_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "MARR")
  marr_places <- purrr::map_chr(fam_xrefs, gedcom_value, gedcom = gedcom, tag = "PLAC", level = 2, after_tag = "MARR")
  num_chil <- purrr::map_chr(fam_xrefs, gedcom_value, gedcom = gedcom, tag = "NCHI", level = 1)
  date_chan <- purrr::map_chr(fam_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = fam_xrefs,
                 husband = stringr::str_remove_all(husb_names, "/"),
                 wife = stringr::str_remove_all(wife_names, "/"),
                 marriage_date = marr_dates,
                 marriage_place = marr_places,
                 num_children = num_chil) %>% 
    dplyr::mutate(num_children = ifelse(num_children == "",
                                        purrr::map_chr(xref, ~sum(dplyr::filter(gedcom, record == .x)$tag == "CHIL")),
                                        num_children),
                  last_modified = date_chan)
  
}

multimedia <- function(gedcom) {
  
  obje_xrefs <- unique(dplyr::filter(gedcom, tag == "OBJE", level == 0)$record)
  file_refs <- purrr::map_chr(obje_xrefs, gedcom_value, gedcom = gedcom, tag = "FILE", level = 1)
  file_titles <- purrr::map_chr(obje_xrefs, gedcom_value, gedcom = gedcom, tag = "TITL", level = 2)
  file_forms <- purrr::map_chr(obje_xrefs, gedcom_value, gedcom = gedcom, tag = "FORM", level = 2)
  file_sour <- purrr::map_chr(obje_xrefs, gedcom_value, gedcom = gedcom, tag = "TYPE", level = 3)
  date_chan <- purrr::map_chr(obje_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = obje_xrefs,
                 file_ref = file_refs,
                 file_title = file_titles,
                 file_format = file_forms,
                 source_media = file_sour,
                 last_modified = date_chan)
  
}

sources <- function(gedcom) {
  
  sour_xrefs <- unique(dplyr::filter(gedcom, tag == "SOUR", level == 0)$record)
  origs <- purrr::map_chr(sour_xrefs, gedcom_value, gedcom = gedcom, tag = "AUTH", level = 1)
  titles <- purrr::map_chr(sour_xrefs, gedcom_value, gedcom = gedcom, tag = "TITL", level = 1)
  date_chan <- purrr::map_chr(sour_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = sour_xrefs,
                 originator = origs,
                 title = titles,
                 last_modified = date_chan)
  
}

repositories <- function(gedcom) {
  
  repo_xrefs <- unique(dplyr::filter(gedcom, tag == "REPO", level == 0)$record)
  repo_names <- purrr::map_chr(repo_xrefs, gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  repo_cits <- purrr::map_chr(repo_xrefs, gedcom_value, gedcom = gedcom, tag = "CITY", level = 2)
  repo_stae <- purrr::map_chr(repo_xrefs, gedcom_value, gedcom = gedcom, tag = "STAE", level = 2)
  repo_coun <- purrr::map_chr(repo_xrefs, gedcom_value, gedcom = gedcom, tag = "CTRY", level = 2)
  date_chan <- purrr::map_chr(repo_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = repo_xrefs,
                 name = repo_names,
                 city = repo_cits,
                 state = repo_stae,
                 country = repo_coun,
                 last_modified = date_chan)
  
}


notes <- function(gedcom) {
  
  note_xrefs <- unique(dplyr::filter(gedcom, tag == "NOTE", level == 0)$record)
  ref_nos <- purrr::map_chr(note_xrefs, gedcom_value, gedcom = gedcom, tag = "REFN", level = 1)
  note_txts <- purrr::map_chr(note_xrefs, gedcom_value, gedcom = gedcom, tag = "NOTE", level = 0)
  date_chan <- purrr::map_chr(note_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = note_xrefs,
                 ref = ref_nos,
                 text = note_txts,
                 last_modified = date_chan)
  
  
}
