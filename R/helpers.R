




#' Temporarily remove forward slashes from surnames
#'
#' @param gedcom A tidyged object.
#'
#' @return A tidyged object with all forward slashes removed from surnames.
temporarily_remove_name_slashes <- function(gedcom) {
  
  gedcom %>% 
    dplyr::mutate(value = dplyr::if_else(purrr::map_lgl(record, is_indi, gedcom=gedcom) &
                                           tag %in% c("NAME", "FONE", "ROMN"),
                                         stringr::str_remove_all(value, "/"),
                                         value))
  
}


#' Derive a valid cross-reference identifier
#' 
#' Get a valid xref provided explicitly or implicitly (through an identifying attribute or
#' active record).
#' 
#' @details This helper function is designed to derive and run validation checks on an xref
#' provided explicitly or implicitly. An xref is provided implicitly either through the active
#' record of the tidyged object, or through a descriptor identifying a unique record.
#' 
#' The descriptors used for each record are: name (individual, repository, and submitter), 
#' title (source), file reference (multimedia), excerpt (note). 
#' 
#' Once found, the xref is checked to ensure it is of the appropriate type.
#'
#' @param gedcom A tidyged object.
#' @param xref_or_descriptor An xref or descriptor uniquely identifying the record.
#' @param record_type A character string describing the record type. Generally one of
#' the global record_string_* values.
#' @param record_type_fn A function to check the record type. Generally one of the is_*
#' functions.
#'
#' @return A valid xref identifier.
get_valid_xref <- function(gedcom, xref_or_descriptor, record_type, record_type_fn) {
  
  if (length(xref_or_descriptor) == 0 || xref_or_descriptor == "") {
    # xref not given explicitly, get it from active record
    xref <- get_active_record(gedcom)
    
  } else if (grepl(tidyged.internals::reg_xref(TRUE), xref_or_descriptor)) {
    # xref given explicitly
    xref <- xref_or_descriptor
    
  } else {
    # xref given by descriptor, find it
    if (record_type == .pkgenv$record_string_indi) {
      
      xref <- find_xref(gedcom, xrefs_indi(gedcom), c("NAME", "ROMN", "FONE"), xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_famg) {
      
      stop("The selected family record is not valid")
      
    } else if(record_type == .pkgenv$record_string_repo) {
      
      xref <- find_xref(gedcom, xrefs_repo(gedcom), "NAME", xref_or_descriptor) 
      
    } else if(record_type == .pkgenv$record_string_sour) {
      
      xref <- find_xref(gedcom, xrefs_sour(gedcom), "TITL", xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_obje) {
      
      xref <- find_xref(gedcom, xrefs_media(gedcom), "FILE", xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_note) {
      
      xref <- find_xref(gedcom, xrefs_note(gedcom), "NOTE", xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_subm) {
      
      xref <- find_xref(gedcom, xrefs_subm(gedcom), "NAME", xref_or_descriptor)
    }
    
  }

  if(is.null(xref))
    stop("No xref is provided and no ", record_type, " record is activated.")
  
  if(!record_type_fn(gedcom, xref))
    stop("The provided or active record is not a ", record_type, " record")
  
  xref
}



#' Remove multiple records at once
#'
#' @param gedcom A tidyged object.
#' @param xrefs A vector of xrefs to remove.
#'
#' @return An updated tidyged object with the records removed.
#' @export
#' @tests
#' expect_snapshot_value(remove_records(sample555, c("@I1@","@I2@")), "json2")
#' expect_snapshot_value(remove_records(sample555, c("@S1@","@R1@")), "json2")
#' expect_snapshot_value(remove_records(sample555, c("@F1@","@I3@")), "json2")
remove_records <- function(gedcom, xrefs) {
  
  for (xref in xrefs) {
    message(describe_records(gedcom, xref, short_desc = TRUE), " removed")
    
    if (is_indi(gedcom, xref)) {
      gedcom <- remove_indi(gedcom, xref)
    } else if(is_famg(gedcom, xref)) {
      gedcom <- remove_famg(gedcom, xref)
    } else if(is_media(gedcom, xref)) {
      gedcom <- remove_media(gedcom, xref)
    } else if(is_sour(gedcom, xref)) {
      gedcom <- remove_sour(gedcom, xref)
    } else if(is_repo(gedcom, xref)) {
      gedcom <- remove_repo(gedcom, xref)
    } else if(is_note(gedcom, xref)) {
      gedcom <- remove_note(gedcom, xref)
    } else {
      stop("Record ", xref, " is not recognised")
    }
  }
  gedcom
}


#' Order children in a Family Group record by birth date
#' 
#' @details Any children without a date a birth are placed last.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of a Family Group record.
#'
#' @return The same tidyged object with rearranged children rows in the Family Group record.
#' @export
order_famg_children <- function(gedcom, xref) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_famg, is_famg)
  
  chil_lines <- dplyr::filter(gedcom, record == xref, tag == "CHIL")
  
  if(nrow(chil_lines) <= 1) return(gedcom)
  
  dob <- purrr::map_chr(chil_lines$value, tidyged.internals::gedcom_value, 
                                    gedcom = gedcom, tag = "DATE", level = 2, after_tag = "BIRT")

  if(all(dob == "")) return(gedcom)
    
  extract_min_year <- function(dob) {
    if(dob == "") return(4000L)
    years <- unlist(stringr::str_extract_all(dob, "\\d{3,4}")) 
    if(length(years) == 1 && years == "") return(4000L)
    min(as.integer(years))
  }
  
  yob <- purrr::map_int(dob, extract_min_year)
  
  chil_lines_yob <- dplyr::mutate(chil_lines, yob = yob) %>% 
    dplyr::arrange(yob) %>% 
    dplyr::select(-yob)
  
  gedcom <- dplyr::anti_join(gedcom, chil_lines_yob,
                             by = c("level", "record", "tag", "value"))
  
  next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, "FAM")
  
  gedcom %>%
    tibble::add_row(chil_lines_yob, .before = next_row)
    
}


#' Update a record's change date
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of a record.
#'
#' @return An updated tidyged object where the specified record has a change date of today.
#' @export
update_change_date <- function(gedcom, xref) {
  
  gedcom <-  tidyged.internals::remove_section(gedcom, 1, "CHAN", "", xrefs = xref, first_only = TRUE)
  
  rec_tag <- dplyr::case_when(is_indi(gedcom, xref) ~ .pkgenv$record_tag_indi,
                              is_famg(gedcom, xref) ~ .pkgenv$record_tag_famg,
                              is_sour(gedcom, xref) ~ .pkgenv$record_tag_sour,
                              is_media(gedcom, xref) ~ .pkgenv$record_tag_obje,
                              is_repo(gedcom, xref) ~ .pkgenv$record_tag_repo,
                              is_note(gedcom, xref) ~ .pkgenv$record_tag_note,
                              is_subm(gedcom, xref) ~ .pkgenv$record_tag_subm)
  
  next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, rec_tag)
  
  chan <- tidyged.internals::CHANGE_DATE() %>% 
    tidyged.internals::add_levels(1)
  
  tibble::add_row(gedcom, chan, .before = next_row) %>% 
    tidyged.internals::finalise()
  
}
