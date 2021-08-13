

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
  subm_name <- tidyged.internals::gedcom_value(object, "HD", "SUBM", 1)
  # this is the longest string
  title_width <- nchar("Source system version:") + 2
  
  paste("GEDCOM file summary:", eol, eol,
        stringr::str_pad("Submitter:", title_width, "right"), tidyged.internals::gedcom_value(object, subm_name, "NAME", 1), eol, 
        stringr::str_pad("Description:", title_width, "right"), tidyged.internals::gedcom_value(object, "HD", "NOTE", 1), eol,
        stringr::str_pad("Language:", title_width, "right"), tidyged.internals::gedcom_value(object, "HD", "LANG", 1), eol,
        stringr::str_pad("Character set:", title_width, "right"), tidyged.internals::gedcom_value(object, "HD", "CHAR", 1), eol, eol,
        
        stringr::str_pad("Copyright:", title_width, "right"), tidyged.internals::gedcom_value(object, "HD", "COPR", 1), eol, eol,
        
        stringr::str_pad("Source system:", title_width, "right"), tidyged.internals::gedcom_value(object, "HD", "SOUR", 1), eol,
        stringr::str_pad("Source system version:", title_width, "right"), tidyged.internals::gedcom_value(object, "HD", "VERS", 2, "SOUR"), eol,
        stringr::str_pad("Product name:", title_width, "right"), tidyged.internals::gedcom_value(object, "HD", "NAME", 2), eol,
        stringr::str_pad("Product source:", title_width, "right"), tidyged.internals::gedcom_value(object, "HD", "CORP", 2), eol
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
#'   add_indi() %>% 
#'   add_indi() %>% 
#'   add_indi() %>% 
#'   add_famg() %>% 
#'   add_famg() %>% 
#'   add_media("ref1", "AAC") %>% 
#'   add_media("ref1", "AAC") %>% 
#'   add_sour() %>% 
#'   add_repo("repo") %>% 
#'   add_note("note1") %>% 
#'   add_note("note2") %>% 
#'   str(), "json2")
str.tidyged <- function(object, ...) {
  eol <- "\n"
  gedc_row <- which(object$tag == "GEDC")
  title_width <- nchar("Multimedia objects:") + 2
  
  paste0("GEDCOM version ", object$value[gedc_row + 1], " (", object$value[gedc_row + 2], ")", eol, eol,
         stringr::str_pad("Individuals:", title_width, "right"), num_indi(object), eol,
         stringr::str_pad("Families:", title_width, "right"), num_famg(object), eol,
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
#'  add_indi(sex = "M") %>% 
#'  add_indi_names(name_pieces(given = "Joe", surname = "Bloggs")) %>% 
#'  add_indi_fact("bir", date = date_calendar(year = 1950, month = 5, day = 7),
#'                             fact_place = place("Somewhere")) %>% 
#'  add_indi_fact("dea", date = date_calendar(year = 2000, month = 12, day = 1),
#'                             fact_place = place("Somewhere else")) %>% 
#'  add_indi(sex = "F") %>% 
#'  add_indi_names(name_pieces(given = "Jess", surname = "Bloggs")) %>% 
#'  add_indi_fact("bir", date = date_calendar(year = 1948, month = 1, day = 15),
#'                             fact_place = place("Somewhere")) %>% 
#'  add_indi(sex = "F") %>% 
#'  add_indi_names(name_pieces(given = "Jessie", surname = "Bloggs")) %>% 
#'  add_indi_fact("bir", date = date_approximated(date_calendar(year = 1970), about = TRUE),
#'                             fact_place = place("Elsewhere")) %>%
#'  add_famg(husband = "Joe", wife = "@I2@", children = "Jessie") %>% 
#'  add_famg_event("rel", date = date_calendar(year = 1969, month = 1, day = 30),
#'                        event_place = place(name = "Another place")) %>% 
#'  tidyged.internals::remove_dates_for_tests() %>% 
#'  df_indi(), "json2")
df_indi <- function(gedcom) {
  
  ind_xrefs <- xrefs_indi(gedcom)
  ind_names <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  ind_sex <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "SEX", level = 1)
  ind_dobs <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "BIRT")
  ind_pobs <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "PLAC", level = 2, after_tag = "BIRT")
  ind_dods <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "DEAT")
  ind_pods <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "PLAC", level = 2, after_tag = "DEAT")
  ind_famc <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "FAMC", level = 1)
  #ind_fams <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "FAMS", level = 1)
  moth_xref <- purrr::map_chr(ind_famc, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "WIFE", level = 1) 
  ind_moth <- purrr::map_chr(moth_xref, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  fath_xref <- purrr::map_chr(ind_famc, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "HUSB", level = 1) 
  ind_fath <- purrr::map_chr(fath_xref, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  ind_sibl <- purrr::map_chr(ind_famc, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "NCHI", level = 1)
  date_chan <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
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


#' @rdname df_indi
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_indi(sex = "M") %>% 
#'  add_indi_names(name_pieces(given = "Joe", surname = "Bloggs")) %>% 
#'  add_indi(sex = "F") %>% 
#'  add_indi_names(name_pieces(given = "Jess", surname = "Bloggs")) %>% 
#'  add_indi(sex = "F") %>% 
#'  add_indi_names(name_pieces(given = "Jessie", surname = "Bloggs")) %>%
#'  add_famg(husband = "Joe", wife = "@I2@", children = "Jessie") %>% 
#'  add_famg_event("rel", date = date_calendar(year = 1969, month = 1, day = 30),
#'                        event_place = place(name = "Another place")) %>% 
#'  tidyged.internals::remove_dates_for_tests() %>% 
#'  df_famg(), "json2")
df_famg <- function(gedcom) {
  
  fam_xrefs <- xrefs_famg(gedcom)
  husb_xrefs <- purrr::map_chr(fam_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "HUSB", level = 1)
  wife_xrefs <- purrr::map_chr(fam_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "WIFE", level = 1)
  husb_names <- purrr::map_chr(husb_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  wife_names <- purrr::map_chr(wife_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  marr_dates <- purrr::map_chr(fam_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "MARR")
  marr_places <- purrr::map_chr(fam_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "PLAC", level = 2, after_tag = "MARR")
  num_chil <- purrr::map_chr(fam_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "NCHI", level = 1)
  date_chan <- purrr::map_chr(fam_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
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

#' @rdname df_indi
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_media(file_reference = "ref1", format = "WAV", source_media = "audio", title = "sounds") %>% 
#'  add_media(file_reference = "ref2", format = "JPEG", source_media = "photo", title = "photo1") %>% 
#'  add_media(file_reference = "ref3", format = "PNG", source_media = "photo", title = "photo2") %>% 
#'  tidyged.internals::remove_dates_for_tests() %>% 
#'  df_media(), "json2")
df_media <- function(gedcom) {
  
  obje_xrefs <- xrefs_media(gedcom)
  file_refs <- purrr::map_chr(obje_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "FILE", level = 1)
  file_titles <- purrr::map_chr(obje_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "TITL", level = 2)
  file_forms <- purrr::map_chr(obje_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "FORM", level = 2)
  file_sour <- purrr::map_chr(obje_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "TYPE", level = 3)
  date_chan <- purrr::map_chr(obje_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = obje_xrefs,
                 file_ref = file_refs,
                 file_title = file_titles,
                 file_format = file_forms,
                 source_media = file_sour,
                 last_modified = date_chan)
  
}

#' @rdname df_indi
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_sour(originator = "author1", title = "book1") %>% 
#'  add_sour(originator = "author2", title = "book2") %>% 
#'  add_sour(originator = "author3", title = "book3") %>% 
#'  tidyged.internals::remove_dates_for_tests() %>% 
#'  df_sour(), "json2")
df_sour <- function(gedcom) {
  
  sour_xrefs <- xrefs_sour(gedcom)
  origs <- purrr::map_chr(sour_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "AUTH", level = 1)
  titles <- purrr::map_chr(sour_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "TITL", level = 1)
  date_chan <- purrr::map_chr(sour_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = sour_xrefs,
                 originator = origs,
                 title = titles,
                 last_modified = date_chan)
  
}

#' @rdname df_indi
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_repo(name = "repo1", repo_address = address(city = "Brighton", state = "E. Sussex", country = "UK")) %>% 
#'  add_repo(name = "repo2", repo_address = address(city = "Orlando", state = "Florida", country = "USA")) %>% 
#'  add_repo(name = "repo3", repo_address = address(city = "Yokohama", country = "Japan")) %>% 
#'  tidyged.internals::remove_dates_for_tests() %>% 
#'  df_repo(), "json2")
df_repo <- function(gedcom) {
  
  repo_xrefs <- xrefs_repo(gedcom)
  repo_names <- purrr::map_chr(repo_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  repo_cits <- purrr::map_chr(repo_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "CITY", level = 2)
  repo_stae <- purrr::map_chr(repo_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "STAE", level = 2)
  repo_coun <- purrr::map_chr(repo_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "CTRY", level = 2)
  date_chan <- purrr::map_chr(repo_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = repo_xrefs,
                 name = repo_names,
                 city = repo_cits,
                 state = repo_stae,
                 country = repo_coun,
                 last_modified = date_chan)
  
}

#' @rdname df_indi
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_note(text = "This is a note", user_reference_number = 1234) %>% 
#'  add_note(text = "This is also a note", user_reference_number = 5678) %>% 
#'  add_note(text = "This may be a note too", user_reference_number = 987643) %>% 
#'  tidyged.internals::remove_dates_for_tests() %>% 
#'  df_note(), "json2")
df_note <- function(gedcom) {
  
  note_xrefs <- xrefs_note(gedcom)
  ref_nos <- purrr::map_chr(note_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "REFN", level = 1)
  note_txts <- purrr::map_chr(note_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "NOTE", level = 0)
  date_chan <- purrr::map_chr(note_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = note_xrefs,
                 ref = ref_nos,
                 text = note_txts,
                 last_modified = date_chan)
  
  
}


fact_summary <- function(gedcom, xref, indi) {
  
  if(indi) {
    xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  } else {
    xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_famg, is_famg)
  }
  
  # Remove source citation structures and notes and add namespace
  gedcom_ns <- tidyged.internals::remove_section(gedcom, containing_level = 2, containing_tags = "SOUR",
                                                 xrefs = xref) %>% 
    dplyr::filter(tag != "NOTE") %>% 
    mutate_tag_namespace()
  
  if(indi) {
    fact_tags <- c(tidyged.internals::val_individual_event_types(),
                   tidyged.internals::val_attribute_types())
  } else {
    fact_tags <- tidyged.internals::val_family_event_types()
  }
  
  rows_vect <- tidyged.internals::identify_section(gedcom_ns, 1, fact_tags,
                                                   xrefs = xref)
  
  fact_rows <- split(rows_vect, cumsum(gedcom_ns$tag[rows_vect] %in% fact_tags))
  
  # Tags we want for the summary
  details_tags <- c("DATE", "PLAC", paste0("ADR", 1:3), "CITY", "STAE", "CTRY", unname(fact_tags), "TYPE")
  if(indi) {
    details_tags <- c(details_tags, "AGE")
  } else {
    details_tags <- c(details_tags, "HUSB.AGE", "WIFE.AGE")
  }
  
  # loop through each fact block
  purrr::map_dfr(fact_rows, 
                 ~ gedcom_ns %>% 
                   dplyr::select(tag_ns, value) %>% 
                   dplyr::slice(.x) %>% 
                   # get rid of FAM- or INDI-
                   dplyr::mutate(tag_ns = stringr::str_sub(tag_ns, ifelse(indi, 6,5), -1)) %>% 
                   # extract fact type into own column
                   dplyr::mutate(fact_type = stringr::str_extract(tag_ns, "^[A-Z]+")) %>% 
                   # replace fact tags with names
                   dplyr::mutate(fact_type = magrittr::extract(names(fact_tags), match(fact_type, fact_tags))) %>% 
                   # only keep details_tags
                   dplyr::filter(stringr::str_detect(tag_ns, paste(paste0(details_tags, "$"),collapse="|"))) %>%
                   # remove namespace before details_tags
                   dplyr::mutate(tag_ns = stringr::str_extract(tag_ns, 
                                                               paste(paste0(details_tags, "$"),collapse="|"))) %>% 
                   tidyr::pivot_wider(names_from = tag_ns, values_from = value)) %>% 
    dplyr::mutate(dplyr::across(everything(), ~ ifelse(. == "", NA_character_, .))) %>% 
    # add missing columns
    dplyr::bind_rows(purrr::map_dfr(details_tags, ~tibble::tibble(!!.x := character() ) )) %>% 
    # reorder columns
    dplyr::select(fact_type, dplyr::all_of(details_tags))
  
}

#' Create a table summarising all individual/family facts
#' 
#' This function creates a tidy table making it easy to extract fact details for an individual or family group.
#' 
#' @details Notes and source citations are not included in the summary, as well as other more obscure fields.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the Individual or Family Group record.
#'
#' @return A tibble containing a row for each fact.
#' @export
fact_summary_indi <- function(gedcom, xref) {
  fact_summary(gedcom, xref, TRUE)
}

#' @rdname fact_summary_indi
#' @export
fact_summary_famg <- function(gedcom, xref) {
  fact_summary(gedcom, xref, FALSE)
}

