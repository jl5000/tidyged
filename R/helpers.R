




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
#' Validate an xref provided explicitly or implicitly (through the active record).
#' 
#' @details This helper function is designed to derive and run validation checks on an xref
#' provided explicitly or implicitly. An xref is provided implicitly through the active
#' record of the tidyged object.
#' 
#' Once found, the xref is checked to ensure it is of the appropriate type.
#'
#' @param gedcom A tidyged object.
#' @param xref A record xref.
#' @param record_type A character string describing the record type. Generally one of
#' the global record_string_* values.
#' @param record_type_fn A function to check the record type. Generally one of the is_*
#' functions.
#'
#' @return A valid xref identifier.
get_valid_xref <- function(gedcom, xref, record_type, record_type_fn) {
  
  if (length(xref) == 0 || xref == "") {
    # xref not given explicitly, get it from active record
    xref <- active_record(gedcom)
  } 

  if(is.null(xref))
    stop("No xref is provided and no ", record_type, " record is activated.")
  
  if(!grepl(tidyged.internals::reg_xref(TRUE), xref))
    stop("The provided xref is not valid")
  
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
    if(length(years) == 0) return(4000L)
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

#' Remove all creation dates from a tidyged object
#' 
#' @details This is a function used in tests so that the objects created do not
#' change every time.
#'
#' @param gedcom A tidyged object.
#'
#' @return The tidyged object with creation dates removed.
remove_dates_for_tests <- function(gedcom) {
  
  gedcom %>% 
    tidyged.internals::remove_section(1, "CHAN", "") %>% 
    dplyr::filter(!(level == 1 & record == "HD" & tag == "DATE"))
  
}

#' Add a tag namespace column to a tidyged object
#' 
#' @details This function is useful if you want to find the namespace of a particular value
#' for the `find_xref` function.
#'
#' @param gedcom A tidyged object.
#'
#' @return A tidyged object with an additional 'tag_ns' column containing the full namespace of
#' the tag.
#' @export
mutate_tag_namespace <- function(gedcom){
  
  gedcom <- dplyr::mutate(gedcom, tag_ns = NA_character_)
  
  for(lv in min(gedcom$level):max(gedcom$level)) {
    gedcom <- dplyr::mutate(gedcom, 
                            tag_ns = dplyr::if_else(level == lv, 
                                                    paste(tag_ns, tag, sep = "."), 
                                                    tag_ns)) %>% 
      dplyr::mutate(tag_ns = dplyr::if_else(level > lv, NA_character_, tag_ns)) %>% 
      tidyr::fill(tag_ns)
  }
  
  dplyr::mutate(gedcom, tag_ns = toupper(stringr::str_remove(tag_ns, "^NA\\.")))
  
}


create_note_structures <- function(gedcom, notes) {
  purrr::map_chr(notes, 
                 ~if(grepl(tidyged.internals::reg_xref(TRUE), .x)){
                   get_valid_xref(gedcom, .x, .pkgenv$record_string_note, is_note)
                 } else {
                   .x
                 }
  ) %>% 
    purrr::map(tidyged.internals::NOTE_STRUCTURE)
}

create_multimedia_links <- function(gedcom, media_links) {
  purrr::map_chr(media_links, get_valid_xref,
                 gedcom = gedcom,
                 record_type = .pkgenv$record_string_obje, 
                 record_typ_fn = is_media) %>% 
    purrr::map(tidyged.internals::MULTIMEDIA_LINK)
}
