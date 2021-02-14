

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
num_famg <- function(gedcom) { unique_record_count(gedcom, .pkgenv$record_tag_famg) }

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


xrefs_record_type <- function(gedcom, record_tag) {
  dplyr::filter(gedcom, level == 0 & tag == record_tag)$record
}

#' Get the xrefs of particular record types
#'
#' These functions return the xrefs of all records of a particular type in a tidyged object.
#'
#' @param gedcom A tidyged object.
#'
#' @return A vector of xrefs of records of the relevant type.
#' @export
xrefs_indi <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_indi) }

#' @export
#' @rdname xrefs_indi
xrefs_famg <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_famg) }

#' @export
#' @rdname xrefs_indi
xrefs_subm <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_subm) }

#' @export
#' @rdname xrefs_indi
xrefs_sour <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_sour) }

#' @export
#' @rdname xrefs_indi
xrefs_repo <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_repo) }

#' @export
#' @rdname xrefs_indi
xrefs_note <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_note) }

#' @export
#' @rdname xrefs_indi
xrefs_media <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_obje) }



is_record_type <- function(gedcom, xref, tag) {
  gedcom[gedcom$record == xref,]$tag[1] == tag
}

#' Check whether a given record is a particular type
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the record.
#'
#' @return A logical indicating whether the record is of a particular type.
#' @export
is_indi <- function(gedcom, xref) { is_record_type(gedcom, xref, .pkgenv$record_tag_indi) }

#' @export
#' @rdname is_indi
is_famg <- function(gedcom, xref) { is_record_type(gedcom, xref, .pkgenv$record_tag_famg) }

#' @export
#' @rdname is_indi
is_subm <- function(gedcom, xref)  { is_record_type(gedcom, xref, .pkgenv$record_tag_subm) }

#' @export
#' @rdname is_indi
is_repo <- function(gedcom, xref) { is_record_type(gedcom, xref, .pkgenv$record_tag_repo) }

#' @export
#' @rdname is_indi
is_media <- function(gedcom, xref) { is_record_type(gedcom, xref, .pkgenv$record_tag_obje) }

#' @export
#' @rdname is_indi
is_note <- function(gedcom, xref)       { is_record_type(gedcom, xref, .pkgenv$record_tag_note) }

#' @export
#' @rdname is_indi
is_sour <- function(gedcom, xref)     { is_record_type(gedcom, xref, .pkgenv$record_tag_sour) }


#' Get descriptions for records
#'
#' @param gedcom A tidyged object.
#' @param xrefs A vector of record xrefs. Only unique records are used. Header and trailer records
#' are ignored.
#' @param short_desc Whether to return a shorter description.
#'
#' @return A vector of record descriptions.
#' @export
describe_records <- function(gedcom, xrefs, short_desc = FALSE) {
  
  xrefs <- unique(xrefs[!xrefs %in% c("HD","TR")])
  
  descriptions <- NULL
  for (xref in xrefs) {
    if (is_indi(gedcom, xref)) {
      descriptions <- c(descriptions, describe_indi(gedcom, xref, FALSE, short_desc))
    } else if(is_famg(gedcom, xref)) {
      descriptions <- c(descriptions, describe_famg(gedcom, xref, short_desc))
    } else if(is_sour(gedcom, xref)) {
      descriptions <- c(descriptions, describe_sour(gedcom, xref, FALSE, short_desc))
    } else if(is_repo(gedcom, xref)) {
      descriptions <- c(descriptions, describe_repo(gedcom, xref, FALSE, short_desc))
    } else if(is_media(gedcom, xref)) {
      descriptions <- c(descriptions, describe_media(gedcom, xref, FALSE, short_desc))
    } else if(is_note(gedcom, xref)) {
      descriptions <- c(descriptions, describe_note(gedcom, xref, short_desc))
    } else if(is_subm(gedcom, xref)) {
      descriptions <- c(descriptions, describe_subm(gedcom, xref, FALSE, short_desc))
    } else {
      stop("Record ", xref, " is not recognised")
    }
    
  }
  descriptions
}

#' Get a description of a record
#' 
#' Get descriptions of a record at various degrees of detail.
#' 
#' @details This function offers three levels of detail. For example, individual records can be:
#' 
#' "Joe Bloggs" (name_only = TRUE)
#' "Individual @I1@, Joe Bloggs" (short_desc = TRUE)
#' "Individual @I1@, Joe Bloggs, child of X and Y, born on x/x/x in place, died on x/x/x in place" (short_desc = FALSE)
#'
#' @param gedcom A tidyged object.
#' @param xref An xref of a record.
#' @param short_desc Whether to return a shorter description.
#' @param name_only Whether to return the individual/repository name only. If none is found, the xref
#' is returned.
#' @param title_only Whether to return the source title only. If none is found, the xref
#' is returned.
#' @param file_ref_only Whether to return the multimedia file reference only. If none is found, the xref
#' is returned.
#'
#' @return A character string describing the record.
#' @export
#' @tests
#' expect_equal(gedcom() %>% add_famg() %>% describe_famg("@F1@"),
#'              "Family @F1@, headed by no individuals, and no children")
describe_famg <- function(gedcom, xref, short_desc = FALSE) {
  # Family @F1@, headed by x and y, [and (no) children x, y, z]
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_famg, is_famg)
  
  husb <- dplyr::filter(gedcom, record == xref, tag == "HUSB")$value
  wife <- dplyr::filter(gedcom, record == xref, tag == "WIFE")$value
  chil <- dplyr::filter(gedcom, record == xref, tag == "CHIL")$value
  
  fam_str <- paste0("Family ", xref, ", headed by ")
  if(length(husb) + length(wife) == 2) {
    fam_str <- paste0(fam_str, describe_indi(gedcom, husb, name_only = TRUE),
                      " and ", describe_indi(gedcom, wife, name_only = TRUE))
  } else if(length(husb) == 1) {
    fam_str <- paste0(fam_str, describe_indi(gedcom, husb, name_only = TRUE))
  } else if(length(wife) == 1) {
    fam_str <- paste0(fam_str, describe_indi(gedcom, wife, name_only = TRUE))
  } else {
    fam_str <- paste0(fam_str, "no individuals")
  }
  
  if(short_desc) return(fam_str)
  
  chil_names <- purrr::map_chr(chil, describe_indi, gedcom=gedcom, name_only = TRUE)
  
  chil_str <- ifelse(length(chil) == 0, ", and no children", 
                     paste0(", and children: ", paste(chil_names, collapse = ", ")))
  
  paste0(fam_str, chil_str)
  
}

#' @rdname describe_famg
#' @export
describe_indi <- function(gedcom, xref, name_only = FALSE, short_desc = FALSE) {
  # Individual @I1@, Name/Unnamed, [child of x and y, born on x/x/x in place, died on x/x/x in place]
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  name <- gedcom_value(gedcom, xref, "NAME", 1, "INDI") %>% 
    stringr::str_remove_all("/")
  
  if(name_only) return(ifelse(name == "", xref, name))
  
  name_str <- ifelse(name == "", "Unnamed individual", name)
  
  ind_str <- paste0("Individual ", xref, ", ", name_str)
  
  if(short_desc) return(ind_str)
  
  famc <- gedcom_value(gedcom, xref, "FAMC", 1, "INDI")
  
  if (famc != "") {
    
    moth_xref <- gedcom_value(gedcom, famc, "WIFE", 1)
    fath_xref <- gedcom_value(gedcom, famc, "HUSB", 1)
    
    moth_name <- ifelse(moth_xref == "", "", describe_indi(gedcom, moth_xref, name_only = TRUE))
    fath_name <- ifelse(fath_xref == "", "", describe_indi(gedcom, fath_xref, name_only = TRUE))
    
    par_str <- dplyr::case_when(fath_name != "" & moth_name != "" ~ paste(fath_name,"and",moth_name),
                                fath_name != "" & moth_name == "" ~ fath_name,
                                fath_name == "" & moth_name != "" ~ moth_name,
                                TRUE ~ "")
  
     ind_str <- ifelse(par_str == "", ind_str, paste0(ind_str, ", child of ", par_str))
  }
  
  dob <- gedcom_value(gedcom, xref, "DATE", level = 2, after_tag = "BIRT")
  pob <- gedcom_value(gedcom, xref, "PLAC", level = 2, after_tag = "BIRT")
  
  if(dob != "" & pob != "") {
    ind_str <- paste0(ind_str, ", born ", dob, " in ", pob)
  } else if (dob != "") {
    ind_str <- paste0(ind_str, ", born ", dob)
  } else if (pob != "") {
    ind_str <- paste0(ind_str, ", born in ", pob)
  } 
  
  dod <- gedcom_value(gedcom, xref, "DATE", level = 2, after_tag = "DEAT")
  pod <- gedcom_value(gedcom, xref, "PLAC", level = 2, after_tag = "DEAT")
  
  if(dod != "" & pod != "") {
    ind_str <- paste0(ind_str, ", died ", dod, " in ", pod)
  } else if (dod != "") {
    ind_str <- paste0(ind_str, ", died ", dod)
  } else if (pod != "") {
    ind_str <- paste0(ind_str, ", died in ", pod)
  } 
  
  ind_str
}

#' @rdname describe_famg
#' @export
describe_media <- function(gedcom, xref, file_ref_only = FALSE, short_desc = FALSE) {
  # Multimedia @M1@, [titled abc, format jpeg], with file reference xyz
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_obje, is_media)
  
  file_ref <- gedcom_value(gedcom, xref, "FILE", 1)
  
  if (file_ref_only) return(ifelse(file_ref == "", xref, file_ref))
  
  media_str <- paste0("Multimedia ", xref)
  
  if(short_desc) return(ifelse(file_ref == "", media_str, paste0(media_str, ", with file reference ", file_ref)))
  
  titl <- gedcom_value(gedcom, xref, "TITL", 2)
  
  media_str <- ifelse(titl == "", media_str, paste0(media_str, ", titled ", titl))
  
  form <- gedcom_value(gedcom, xref, "FORM", 2)
  
  media_str <- ifelse(form == "", media_str, paste0(media_str, ", format ", form))
  media_str <- ifelse(file_ref == "", media_str, paste0(media_str, ", with file reference ", file_ref))
  media_str
  
}

#' @rdname describe_famg
#' @export
describe_sour <- function(gedcom, xref, title_only = FALSE, short_desc = FALSE) {
  # Source @S1@, titled abc, [by x]
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_sour, is_sour)
  
  titl <- gedcom_value(gedcom, xref, "TITL", 1)
  
  if (title_only) return(ifelse(titl == "", xref, titl))
  
  sour_str <- paste0("Source ", xref)
  sour_str <- ifelse(titl == "", sour_str, paste0(sour_str, ", titled ", titl))
  
  if (short_desc) return(sour_str)
  
  orig <- gedcom_value(gedcom, xref, "AUTH", 1)
  
  ifelse(orig == "", sour_str, paste0(sour_str, ", by ", orig))
}

#' @rdname describe_famg
#' @export
describe_repo <- function(gedcom, xref, name_only = FALSE, short_desc = FALSE) {
  # Repository @R1@, name
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_repo, is_repo)
  
  name <- gedcom_value(gedcom, xref, "NAME", 1)
  
  if(name_only) return(ifelse(name == "", xref, name))
  
  paste0("Repository ", xref, ", ", name)
  
}

#' @rdname describe_famg
#' @export
describe_note <- function(gedcom, xref, short_desc = FALSE) {
  # Note @N1@ with the following text: xyz [excerpt or not]
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_note, is_note)
  
  note_str <- paste0("Note ", xref, ", with the following text: ")
  
  text <- gedcom_value(gedcom, xref, "NOTE", 0)
  
  ifelse(short_desc, paste0(note_str, stringr::str_sub(text, 1, 30), "..."), 
                     paste0(note_str, text))
  
}

#' @rdname describe_famg
#' @export
describe_subm <- function(gedcom, xref, name_only = FALSE, short_desc = FALSE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_subm, is_subm)
  
  name <- gedcom_value(gedcom, xref, "NAME", 1)
  
  if (name_only) return(ifelse(name == "", xref, name))
  
  subm_str <- paste0("Submitter ", xref)
  
  ifelse(name == "", subm_str, paste0(subm_str, ", ", name))
  
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
#'  add_indi_names(given = "Joe", surname = "Bloggs") %>% 
#'  add_indi_event_birth(event_date = date_calendar(year = 1950, month = 5, day = 7),
#'                             place_name = "Somewhere") %>% 
#'  add_indi_event_death(event_date = date_calendar(year = 2000, month = 12, day = 1),
#'                             place_name = "Somewhere else") %>% 
#'  add_indi(sex = "F") %>% 
#'  add_indi_names(given = "Jess", surname = "Bloggs") %>% 
#'  add_indi_event_birth(event_date = date_calendar(year = 1948, month = 1, day = 15),
#'                             place_name = "Somewhere") %>% 
#'  add_indi(sex = "F") %>% 
#'  add_indi_names(given = "Jessie", surname = "Bloggs") %>% 
#'  add_indi_event_birth(event_date = date_approximated(date_calendar(year = 1970), about = TRUE),
#'                             place_name = "Elsewhere") %>%
#'  add_famg(husband = "Joe", wife = "Jess Bloggs", children = "Jessie") %>% 
#'  add_famg_event_relationship(event_date = date_calendar(year = 1969, month = 1, day = 30),
#'                                place_name = "Another place") %>% 
#'  remove_dates_for_tests() %>% 
#'  df_indi(), "json2")
df_indi <- function(gedcom) {
  
  ind_xrefs <- xrefs_indi(gedcom)
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


#' @rdname df_indi
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_indi(sex = "M") %>% 
#'  add_indi_names(given = "Joe", surname = "Bloggs") %>% 
#'  add_indi(sex = "F") %>% 
#'  add_indi_names(given = "Jess", surname = "Bloggs") %>% 
#'  add_indi(sex = "F") %>% 
#'  add_indi_names(given = "Jessie", surname = "Bloggs") %>%
#'  add_famg(husband = "Joe", wife = "Jess Bloggs", children = "Jessie") %>% 
#'  add_famg_event_relationship(event_date = date_calendar(year = 1969, month = 1, day = 30),
#'                                place_name = "Another place") %>% 
#'  remove_dates_for_tests() %>% 
#'  df_famg(), "json2")
df_famg <- function(gedcom) {
  
  fam_xrefs <- xrefs_famg(gedcom)
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

#' @rdname df_indi
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_media(file_reference = "ref1", format = "WAV", source_media = "audio", title = "sounds") %>% 
#'  add_media(file_reference = "ref2", format = "JPEG", source_media = "photo", title = "photo1") %>% 
#'  add_media(file_reference = "ref3", format = "PNG", source_media = "photo", title = "photo2") %>% 
#'  remove_dates_for_tests() %>% 
#'  df_media(), "json2")
df_media <- function(gedcom) {
  
  obje_xrefs <- xrefs_media(gedcom)
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

#' @rdname df_indi
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_sour(originator = "author1", title = "book1") %>% 
#'  add_sour(originator = "author2", title = "book2") %>% 
#'  add_sour(originator = "author3", title = "book3") %>% 
#'  remove_dates_for_tests() %>% 
#'  df_sour(), "json2")
df_sour <- function(gedcom) {
  
  sour_xrefs <- xrefs_sour(gedcom)
  origs <- purrr::map_chr(sour_xrefs, gedcom_value, gedcom = gedcom, tag = "AUTH", level = 1)
  titles <- purrr::map_chr(sour_xrefs, gedcom_value, gedcom = gedcom, tag = "TITL", level = 1)
  date_chan <- purrr::map_chr(sour_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = sour_xrefs,
                 originator = origs,
                 title = titles,
                 last_modified = date_chan)
  
}

#' @rdname df_indi
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_repo(name = "repo1", city = "Brighton", state = "E. Sussex", country = "UK") %>% 
#'  add_repo(name = "repo2", city = "Orlando", state = "Florida", country = "USA") %>% 
#'  add_repo(name = "repo3", city = "Yokohama", country = "Japan") %>% 
#'  remove_dates_for_tests() %>% 
#'  df_repo(), "json2")
df_repo <- function(gedcom) {
  
  repo_xrefs <- xrefs_repo(gedcom)
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

#' @rdname df_indi
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'  add_note(text = "This is a note", user_reference_number = 1234) %>% 
#'  add_note(text = "This is also a note", user_reference_number = 5678) %>% 
#'  add_note(text = "This may be a note too", user_reference_number = 987643) %>% 
#'  remove_dates_for_tests() %>% 
#'  df_note(), "json2")
df_note <- function(gedcom) {
  
  note_xrefs <- xrefs_note(gedcom)
  ref_nos <- purrr::map_chr(note_xrefs, gedcom_value, gedcom = gedcom, tag = "REFN", level = 1)
  note_txts <- purrr::map_chr(note_xrefs, gedcom_value, gedcom = gedcom, tag = "NOTE", level = 0)
  date_chan <- purrr::map_chr(note_xrefs, gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "CHAN")
  
  tibble::tibble(xref = note_xrefs,
                 ref = ref_nos,
                 text = note_txts,
                 last_modified = date_chan)
  
  
}
