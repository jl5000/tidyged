

unique_record_count <- function(gedcom, tag) {sum(gedcom$level == 0 & gedcom$tag == tag)}


#' Get the number of records in a tidygedcom object
#'
#' These functions return the number of records of a particular type in a tidygedcom object.
#'
#' @param gedcom A tidygedcom object.
#'
#' @return The number of records of the relevant type.
#' @export
num_indi <- function(gedcom) { unique_record_count(gedcom, .pkgenv$record_tag_indi) }

#' @export
#' @rdname num_indi
num_fam <- function(gedcom) { unique_record_count(gedcom, .pkgenv$record_tag_fam) }

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


#' Get a summary of a tidygedcom object
#'
#' This function shows key information from the header of a tidygedcom object, including submitter
#' and description.
#'
#' @param object A tidygedcom object.
#' @param ... Not used.
#'
#' @return A printed summary of the GEDCOM file.
#' @export
summary.tidygedcom <- function(object, ...) {
  eol <- "\n"
  subm_name <- gedcom_value(object, "HD", "SUBM", 1)
  title_width <- nchar("Source system version:") + 2
  
  paste("GEDCOM file summary:", eol, eol,
        stringr::str_pad("Submitter:", title_width, "right"), gedcom_value(object, subm_name, "NAME", 1), eol, 
        stringr::str_pad("Description:", title_width, "right"), gedcom_value(object, "HD", "NOTE", 1), eol,
        stringr::str_pad("Language:", title_width, "right"), gedcom_value(object, "HD", "LANG", 1), eol,
        stringr::str_pad("Character set:", title_width, "right"), gedcom_value(object, "HD", "CHAR", 1), eol, eol,
        
        stringr::str_pad("Copyright:", title_width, "right"), gedcom_value(object, "HD", "COPR", 1), eol, eol,
        
        stringr::str_pad("Source system:", title_width, "right"), gedcom_value(object, "HD", "SOUR", 1), eol,
        stringr::str_pad("Source system version:", title_width, "right"), gedcom_value(object, "HD", "VERS", 2, "SOUR"), eol,
        stringr::str_pad("Product name:", title_width, "right"), gedcom_value(object, "HD", "NAME", 2), eol,
        stringr::str_pad("Product source:", title_width, "right"), gedcom_value(object, "HD", "CORP", 2), eol
  ) %>% cat()
}

#' Get the structure of a tidygedcom object
#'
#' This function gives a breakdown of record counts in the GEDCOM file.
#'
#' @param object A tidygedcom object.
#' @param ... Not used.
#'
#' @return A printed summary of the GEDCOM file structure.
#' @export
str.tidygedcom <- function(object, ...) {
  eol <- "\n"
  gedc_row <- which(object$tag == "GEDC")
  title_width <- nchar("Multimedia objects:") + 2
  
  paste0("GEDCOM version ", object$value[gedc_row + 1], " (", object$value[gedc_row + 2], ")", eol, eol,
         stringr::str_pad("Individuals:", title_width, "right"), num_indi(object), eol,
         stringr::str_pad("Families:", title_width, "right"), num_fam(object), eol,
         stringr::str_pad("Submitters:", title_width, "right"), num_subm(object), eol,
         stringr::str_pad("Multimedia objects:", title_width, "right"), num_media(object), eol, 
         stringr::str_pad("Notes:", title_width, "right"), num_note(object), eol,
         stringr::str_pad("Sources:", title_width, "right"), num_sour(object), eol,
         stringr::str_pad("Repositories:", title_width, "right"), num_repo(object), eol 
  ) %>% cat()
}

#' Summarise records in a GEDCOM file
#'
#' These functions give a summary of key information of individuals/families/notes etc. 
#' in the GEDCOM file.
#'
#' @param gedcom A tidygedcom object.
#'
#' @return A tibble summarising records where every row is a record.
#' @export
df_individuals <- function(gedcom) {
  
  ind_xrefs <- xrefs_individuals(gedcom)
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


#' @rdname df_individuals
#' @export
df_families <- function(gedcom) {
  
  fam_xrefs <- xrefs_families(gedcom)
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

#' @rdname df_individuals
#' @export
df_multimedia <- function(gedcom) {
  
  obje_xrefs <- xrefs_multimedia(gedcom)
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

#' @rdname df_individuals
#' @export
df_sources <- function(gedcom) {
  
  sour_xrefs <- xrefs_sources(gedcom)
  origs <- purrr::map_chr(sour_xrefs, gedcom_value, gedcom = gedcom, tag = "AUTH", level = 1)
  titles <- purrr::map_chr(sour_xrefs, gedcom_value, gedcom = gedcom, tag = "TITL", level = 1)
  date_chan <- purrr::map_chr(sour_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = sour_xrefs,
                 originator = origs,
                 title = titles,
                 last_modified = date_chan)
  
}

#' @rdname df_individuals
#' @export
df_repositories <- function(gedcom) {
  
  repo_xrefs <- xrefs_repositories(gedcom)
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

#' @rdname df_individuals
#' @export
df_notes <- function(gedcom) {
  
  note_xrefs <- xrefs_notes(gedcom)
  ref_nos <- purrr::map_chr(note_xrefs, gedcom_value, gedcom = gedcom, tag = "REFN", level = 1)
  note_txts <- purrr::map_chr(note_xrefs, gedcom_value, gedcom = gedcom, tag = "NOTE", level = 0)
  date_chan <- purrr::map_chr(note_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = note_xrefs,
                 ref = ref_nos,
                 text = note_txts,
                 last_modified = date_chan)
  
  
}
