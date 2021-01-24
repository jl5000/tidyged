

unique_record_count <- function(gedcom, tag) {sum(gedcom$level == 0 & gedcom$tag == tag)}

#' Get the number of records in a tidyged object
#'
#' These functions return the number of records of a particular type in a tidyged object.
#'
#' @param gedcom A tidyged object.
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


is_record_type <- function(gedcom, xref, tag) {
  dplyr::filter(gedcom, record == xref)$tag[1] == tag
}

#' Check whether a given record is a particular type
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the record.
#'
#' @return A logical indicating whether the record is of a particular type.
#' @export
is_individual <- function(gedcom, xref) { is_record_type(gedcom, xref, .pkgenv$record_tag_indi) }

#' @export
#' @rdname is_individual
is_family <- function(gedcom, xref)     { is_record_type(gedcom, xref, .pkgenv$record_tag_fam) }

#' @export
#' @rdname is_individual
is_submitter <- function(gedcom, xref)  { is_record_type(gedcom, xref, .pkgenv$record_tag_subm) }

#' @export
#' @rdname is_individual
is_repository <- function(gedcom, xref) { is_record_type(gedcom, xref, .pkgenv$record_tag_repo) }

#' @export
#' @rdname is_individual
is_multimedia <- function(gedcom, xref) { is_record_type(gedcom, xref, .pkgenv$record_tag_obje) }

#' @export
#' @rdname is_individual
is_note <- function(gedcom, xref)       { is_record_type(gedcom, xref, .pkgenv$record_tag_note) }

#' @export
#' @rdname is_individual
is_source <- function(gedcom, xref)     { is_record_type(gedcom, xref, .pkgenv$record_tag_sour) }


#' Get a description of a family group
#'
#' @param gedcom A tidyged object.
#' @param xref An xref of a Family group record.
#'
#' @return A character string describing the members of the family group.
#' @export
#' @tests
#' expect_equal(gedcom() %>% add_family_group() %>% describe_family_group("@F1@"),
#'              "Family @F1@ with no husband, no wife, and no children")
describe_family_group <- function(gedcom, xref) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_fam, is_family)
  
  husb <- dplyr::filter(gedcom, record == xref, tag == "HUSB")$value
  wife <- dplyr::filter(gedcom, record == xref, tag == "WIFE")$value
  chil <- dplyr::filter(gedcom, record == xref, tag == "CHIL")$value
  
  husb_str <- ifelse(length(husb) == 0, 
                     "no husband", 
                     paste("husband:", get_individual_name(gedcom, husb)))
  
  wife_str <- ifelse(length(wife) == 0, 
                     "no wife", 
                     paste("wife:", get_individual_name(gedcom, wife)))
  
  chil_str <- ifelse(length(chil) == 0, 
                     "no children", 
                     paste("children:", paste(purrr::map_chr(chil, get_individual_name, gedcom=gedcom),
                                              collapse = ", ")))
  
  paste0("Family ", xref, " with ", husb_str, ", ", wife_str, ", and ", chil_str)
  
}

#' Get a description of an individual
#'
#' @param gedcom A tidyged object.
#' @param xref An xref of an Individual record.
#' @param abb Whether to abbreviate the output.
#'
#' @return A character string describing the individual.
#' @export
describe_individual <- function(gedcom, xref, abb = FALSE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_individual)
  
  name <- gedcom_value(gedcom, xref, "NAME", 1, "INDI") %>% 
    stringr::str_remove_all("/")
  
  name_str <- ifelse(name == "", "Unnamed individual", name)
  
  #sex <- gedcom_value(gedcom, xref, "SEX", 1, "INDI")
  
  famc <- gedcom_value(gedcom, xref, "FAMC", 1, "INDI")
  
  if (famc != "") {
    
    moth_xref <- gedcom_value(gedcom, famc, "WIFE", 1)
    fath_xref <- gedcom_value(gedcom, famc, "HUSB", 1)
    
    moth_name <- gedcom_value(gedcom, moth_xref, "NAME", 1) %>% 
      stringr::str_remove_all("/")
    fath_name <- gedcom_value(gedcom, fath_xref, "NAME", 1)%>% 
      stringr::str_remove_all("/")
    
    par_str <- dplyr::case_when(fath_name != "" & moth_name != "" ~ paste(fath_name,"and",moth_name),
                                fath_name != "" & moth_name == "" ~ fath_name,
                                fath_name == "" & moth_name != "" ~ moth_name,
                                TRUE ~ "")
  
  } else {
    par_str <- ""
  }
  
  dob <- gedcom_value(gedcom, xref, "DATE", level = 2, after_tag = "BIRT")
  
  dod <- gedcom_value(gedcom, xref, "DATE", level = 2, after_tag = "DEAT")
  
  if(abb) {
    paste0(ifelse(name == "", "Unnamed individual", name), 
           ifelse(dob == "", "", paste(", b:", dob)),
           ifelse(dod == "", "", paste(", d:", dod)))
     
  } else {
    paste0(ifelse(name == "", "Unnamed individual", name), 
           ifelse(dob == "", "", paste(", born", dob)),
           ifelse(dod == "", "", paste(", died", dod)),
           ifelse(par_str == "", "", paste(", child of", par_str)))
  }
}

#' Get a summary of a tidyged object
#'
#' This function shows key information from the header of a tidyged object, including submitter
#' and description.
#'
#' @param object A tidyged object.
#' @param ... Not used.
#'
#' @return A printed summary of the tidyged object.
#' @export
#' @tests
#' expect_snapshot_value(
#'                gedcom(subm("Me"), gedcom_description = "descrip", language = "English",
#'                       gedcom_copyright = "copyright statement") %>% 
#'                 summary(), "json2")
summary.tidyged <- function(object, ...) {
  eol <- "\n"
  subm_name <- gedcom_value(object, "HD", "SUBM", 1)
  # this is the longest string
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


#' Get the structure of a tidyged object
#'
#' This function gives a breakdown of record counts in the GEDCOM file.
#'
#' @param object A tidyged object.
#' @param ... Not used.
#'
#' @return A printed summary of records in the tidyged object.
#' @export
#' @tests
#' expect_snapshot_value(
#'  gedcom(subm("Me")) %>% 
#'   add_individual() %>% 
#'   add_individual() %>% 
#'   add_individual() %>% 
#'   add_family_group() %>% 
#'   add_family_group() %>% 
#'   add_multimedia("ref1", "AAC") %>% 
#'   add_multimedia("ref1", "AAC") %>% 
#'   add_source() %>% 
#'   add_repository("repo") %>% 
#'   add_note("note1") %>% 
#'   add_note("note2") %>% 
#'   str(), "json2")
str.tidyged <- function(object, ...) {
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
#' @param gedcom A tidyged object.
#'
#' @return A tibble summarising records where every row is a record.
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_individual(sex = "M") %>% 
#'  add_individual_names("Joe /Bloggs/") %>% 
#'  add_individual_event_birth(event_date = date_calendar(year = 1950, month = 5, day = 7),
#'                             place_name = "Somewhere") %>% 
#'  add_individual_event_death(event_date = date_calendar(year = 2000, month = 12, day = 1),
#'                             place_name = "Somewhere else") %>% 
#'  add_individual(sex = "F") %>% 
#'  add_individual_names("Jess /Bloggs/") %>% 
#'  add_individual_event_birth(event_date = date_calendar(year = 1948, month = 1, day = 15),
#'                             place_name = "Somewhere") %>% 
#'  add_individual(sex = "F") %>% 
#'  add_individual_names("Jessie /Bloggs/") %>% 
#'  add_individual_event_birth(event_date = date_approximated(date_calendar(year = 1970), about = TRUE),
#'                             place_name = "Elsewhere") %>%
#'  add_family_group(husband = "Joe", wife = "Jess Bloggs", children = "Jessie") %>% 
#'  add_family_event_relationship(event_date = date_calendar(year = 1969, month = 1, day = 30),
#'                                place_name = "Another place") %>% 
#'  remove_dates_for_tests() %>% 
#'  df_individuals(), "json2")
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
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_individual(sex = "M") %>% 
#'  add_individual_names("Joe /Bloggs/") %>% 
#'  add_individual(sex = "F") %>% 
#'  add_individual_names("Jess /Bloggs/") %>% 
#'  add_individual(sex = "F") %>% 
#'  add_individual_names("Jessie /Bloggs/") %>%
#'  add_family_group(husband = "Joe", wife = "Jess Bloggs", children = "Jessie") %>% 
#'  add_family_event_relationship(event_date = date_calendar(year = 1969, month = 1, day = 30),
#'                                place_name = "Another place") %>% 
#'  remove_dates_for_tests() %>% 
#'  df_families(), "json2")
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
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_multimedia(file_reference = "ref1", format = "WAV", source_media = "audio", title = "sounds") %>% 
#'  add_multimedia(file_reference = "ref2", format = "JPEG", source_media = "photo", title = "photo1") %>% 
#'  add_multimedia(file_reference = "ref3", format = "PNG", source_media = "photo", title = "photo2") %>% 
#'  remove_dates_for_tests() %>% 
#'  df_multimedia(), "json2")
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
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_source(originator = "author1", title = "book1") %>% 
#'  add_source(originator = "author2", title = "book2") %>% 
#'  add_source(originator = "author3", title = "book3") %>% 
#'  remove_dates_for_tests() %>% 
#'  df_sources(), "json2")
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
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_repository(name = "repo1", city = "Brighton", state = "E. Sussex", country = "UK") %>% 
#'  add_repository(name = "repo2", city = "Orlando", state = "Florida", country = "USA") %>% 
#'  add_repository(name = "repo3", city = "Yokohama", country = "Japan") %>% 
#'  remove_dates_for_tests() %>% 
#'  df_repositories(), "json2")
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
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_note(text = "This is a note", user_reference_number = 1234) %>% 
#'  add_note(text = "This is also a note", user_reference_number = 5678) %>% 
#'  add_note(text = "This may be a note too", user_reference_number = 987643) %>% 
#'  remove_dates_for_tests() %>% 
#'  df_notes(), "json2")
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
