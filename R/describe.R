

#' Get descriptions for records
#'
#' @param gedcom A tidyged object.
#' @param xrefs A vector of record xrefs. Only unique records are used. Header and trailer records
#' are ignored.
#' @param short_desc Whether to return a shorter description.
#'
#' @return A vector of record descriptions.
#' @export
#' @tests
#' expect_snapshot_value(describe_records(sample555, sample555$record, TRUE), "json2")
#' expect_snapshot_value(describe_records(sample555, sample555$record), "json2")
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
  
  name <- tidyged.internals::gedcom_value(gedcom, xref, "NAME", 1, "INDI") %>% 
    stringr::str_remove_all("/")
  
  if(name_only) return(ifelse(name == "", xref, name))
  
  name_str <- ifelse(name == "", "Unnamed individual", name)
  
  ind_str <- paste0("Individual ", xref, ", ", name_str)
  
  if(short_desc) return(ind_str)
  
  famc <- tidyged.internals::gedcom_value(gedcom, xref, "FAMC", 1, "INDI")
  
  if (famc != "") {
    
    moth_xref <- tidyged.internals::gedcom_value(gedcom, famc, "WIFE", 1)
    fath_xref <- tidyged.internals::gedcom_value(gedcom, famc, "HUSB", 1)
    
    moth_name <- ifelse(moth_xref == "", "", describe_indi(gedcom, moth_xref, name_only = TRUE))
    fath_name <- ifelse(fath_xref == "", "", describe_indi(gedcom, fath_xref, name_only = TRUE))
    
    par_str <- dplyr::case_when(fath_name != "" & moth_name != "" ~ paste(fath_name,"and",moth_name),
                                fath_name != "" & moth_name == "" ~ fath_name,
                                fath_name == "" & moth_name != "" ~ moth_name,
                                TRUE ~ "")
    
    ind_str <- ifelse(par_str == "", ind_str, paste0(ind_str, ", child of ", par_str))
  }
  
  dob <- tidyged.internals::gedcom_value(gedcom, xref, "DATE", level = 2, after_tag = "BIRT")
  pob <- tidyged.internals::gedcom_value(gedcom, xref, "PLAC", level = 2, after_tag = "BIRT")
  
  if(dob != "" & pob != "") {
    ind_str <- paste0(ind_str, ", born ", dob, " in ", pob)
  } else if (dob != "") {
    ind_str <- paste0(ind_str, ", born ", dob)
  } else if (pob != "") {
    ind_str <- paste0(ind_str, ", born in ", pob)
  } 
  
  dod <- tidyged.internals::gedcom_value(gedcom, xref, "DATE", level = 2, after_tag = "DEAT")
  pod <- tidyged.internals::gedcom_value(gedcom, xref, "PLAC", level = 2, after_tag = "DEAT")
  
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
  
  file_ref <- tidyged.internals::gedcom_value(gedcom, xref, "FILE", 1)
  
  if (file_ref_only) return(ifelse(file_ref == "", xref, file_ref))
  
  media_str <- paste0("Multimedia ", xref)
  
  if(short_desc) return(ifelse(file_ref == "", media_str, paste0(media_str, ", with file reference ", file_ref)))
  
  titl <- tidyged.internals::gedcom_value(gedcom, xref, "TITL", 2)
  
  media_str <- ifelse(titl == "", media_str, paste0(media_str, ", titled ", titl))
  
  form <- tidyged.internals::gedcom_value(gedcom, xref, "FORM", 2)
  
  media_str <- ifelse(form == "", media_str, paste0(media_str, ", format ", form))
  media_str <- ifelse(file_ref == "", media_str, paste0(media_str, ", with file reference ", file_ref))
  media_str
  
}

#' @rdname describe_famg
#' @export
describe_sour <- function(gedcom, xref, title_only = FALSE, short_desc = FALSE) {
  # Source @S1@, titled abc, [by x]
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_sour, is_sour)
  
  titl <- tidyged.internals::gedcom_value(gedcom, xref, "TITL", 1)
  
  if (title_only) return(ifelse(titl == "", xref, titl))
  
  sour_str <- paste0("Source ", xref)
  sour_str <- ifelse(titl == "", sour_str, paste0(sour_str, ", titled ", titl))
  
  if (short_desc) return(sour_str)
  
  orig <- tidyged.internals::gedcom_value(gedcom, xref, "AUTH", 1)
  
  ifelse(orig == "", sour_str, paste0(sour_str, ", by ", orig))
}

#' @rdname describe_famg
#' @export
describe_repo <- function(gedcom, xref, name_only = FALSE, short_desc = FALSE) {
  # Repository @R1@, name
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_repo, is_repo)
  
  name <- tidyged.internals::gedcom_value(gedcom, xref, "NAME", 1)
  
  if(name_only) return(ifelse(name == "", xref, name))
  
  paste0("Repository ", xref, ", ", name)
  
}

#' @rdname describe_famg
#' @export
describe_note <- function(gedcom, xref, short_desc = FALSE) {
  # Note @N1@ with the following text: xyz [excerpt or not]
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_note, is_note)
  
  note_str <- paste0("Note ", xref, ", with the following text: ")
  
  text <- tidyged.internals::gedcom_value(gedcom, xref, "NOTE", 0)
  
  ifelse(short_desc, paste0(note_str, stringr::str_sub(text, 1, 30), "..."), 
         paste0(note_str, text))
  
}

#' @rdname describe_famg
#' @export
describe_subm <- function(gedcom, xref, name_only = FALSE, short_desc = FALSE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_subm, is_subm)
  
  name <- tidyged.internals::gedcom_value(gedcom, xref, "NAME", 1)
  
  if (name_only) return(ifelse(name == "", xref, name))
  
  subm_str <- paste0("Submitter ", xref)
  
  ifelse(name == "", subm_str, paste0(subm_str, ", ", name))
  
}
