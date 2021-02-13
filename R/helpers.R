

`%nin%` <- Negate(`%in%`)



xrefs_record_type <- function(gedcom, record_tag) {
  dplyr::filter(gedcom, level == 0 & tag == record_tag)$record
}

xrefs_individuals <-  function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_indi) }
xrefs_families <-     function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_famg) }
xrefs_submitters <-   function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_subm) }
xrefs_sources <-      function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_sour) }
xrefs_repositories <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_repo) }
xrefs_notes <-        function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_note) }
xrefs_multimedia <-   function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_obje) }



#' Create a new xref for a record
#' 
#' This function is used to assign xrefs to new records that are created.
#'
#' @param type An alphabetic sequence to be used as a prefix for the xref identifier.
#' GEDCOM files traditionally use a single letter to denote the type of record, e.g.
#' I for Individual, F for Family group, etc.
#' @param ref An explicit reference number after the type if one is to be chosen manually.
#' @param gedcom A tidyged object.
#'
#' @return An xref to use for a new record.
#' @export
assign_xref <- function(type = "", ref = 0, gedcom = tibble::tibble()) {
  
  if (ref == 0) {
    # Are there any existing records of this type?
    gedcom_filt <- gedcom %>% 
      dplyr::filter(stringr::str_detect(record, paste0("^@", type, "\\d+@$"))) 
    
    if(nrow(gedcom_filt) == 0) {
      ref <- 1
    } else {
      ref <- gedcom_filt %>%
        dplyr::pull(record) %>% 
        unique() %>% 
        purrr::keep(stringr::str_detect(., paste0("^@", type, "(\\d+)@$"))) %>% 
        stringr::str_remove_all("@") %>% 
        stringr::str_remove_all("[A-Za-z]") %>% 
        as.numeric() %>% 
        max() + 1
      
    }
    
  } 
  paste0("@", type, ref, "@")
}




#' Extract a particular value from a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param record_xref The xref of the record in which the value may exist.
#' @param tag The tag associated with the value.
#' @param level The level number of the value.
#' @param after_tag Whether the tag should be subordinate to this parent tag. 
#'
#' @return The particular value fitting the criteria of the input arguments. If no value is found,
#' an empty string is returned.
#' @tests
#' expect_equal(gedcom_value(gedcom(), "HD", "LANG", 1), "English")
#' expect_equal(gedcom_value(gedcom(), "HD", "TEST", 1), "")
#' expect_equal(gedcom_value(gedcom(), "HD", "VERS", 2), "5.5.5")
#' expect_equal(gedcom_value(gedcom(), "HD", "VERS", 3), "5.5.5")
gedcom_value <- function(gedcom, record_xref, tag, level, after_tag = NULL) {
  
  gedcom_filtered <- dplyr::filter(gedcom, record %in% record_xref)
  if(nrow(gedcom_filtered) == 0) return("")
  
  active <- is.null(after_tag)
  for(i in seq_len(nrow(gedcom_filtered))) {
    if(is.null(after_tag)) {
      active <- TRUE
    } else if(gedcom_filtered$tag[i] == after_tag && gedcom_filtered$level[i] < level) {
      active <- TRUE
    } else if(active && gedcom_filtered$level[i] < level){
      active <- FALSE
    }
    
    if(active) {
      if(gedcom_filtered$tag[i] == tag & gedcom_filtered$level[i] == level) break  
    }
    
    if(i == nrow(gedcom_filtered)) return("")
  }
  
  if(i == nrow(gedcom_filtered)) return(gedcom_filtered$value[i])
  
  for(j in (i+1):nrow(gedcom_filtered)) {
    if(gedcom_filtered$tag[j] %nin% c("CONT", "CONC") | 
       gedcom_filtered$level[j] != level + 1) {
      j <- j - 1
      break
    }
  }
  
  if(i == j) return(gedcom_filtered$value[i])
  
  text <- gedcom_filtered$value[i]
  for(row in (i+1):j) {
    if(gedcom_filtered$tag[row] == "CONT") text <- paste0(text, "\n", gedcom_filtered$value[row])
    if(gedcom_filtered$tag[row] == "CONC") text <- paste0(text, gedcom_filtered$value[row])
  }
  
  cat(text)
}


#' Construct a full personal name
#' 
#' This function constructs a full personal name from individual name pieces.
#'
#' @param prefix The name prefix.
#' @param given The given name(s).
#' @param nickname The nickname.
#' @param surname_prefix The surname prefix.
#' @param surname The surname.
#' @param suffix The name suffix.
#'
#' @return The full name with all name pieces combined.
#' @tests
#' expect_error(construct_full_name(surname_prefix = "de la"))
#' expect_equal(construct_full_name(prefix = "Lt. Cdr."), "Lt. Cdr.")
#' expect_equal(construct_full_name(given = "Joe"), "Joe")
#' expect_equal(construct_full_name(given = "Joe,Adam"), "Joe Adam")
#' expect_equal(construct_full_name(given = "Joey,Joe, Joe"), "Joey Joe Joe")
#' expect_equal(construct_full_name(nickname = "Nobby"), "'Nobby'")
#' expect_equal(construct_full_name(surname = "Bloggs"), "/Bloggs/")
#' expect_equal(construct_full_name(suffix = "Jr."), "Jr.")
#' expect_equal(construct_full_name(suffix = "Jr.,Esq."), "Jr. Esq.")
#' expect_equal(construct_full_name(prefix = "Lt. Cdr.", given = "Joe,Adam", nickname = "Nobby", 
#'                                  surname_prefix = "de la", surname = "Bloggs",
#'                                  suffix = "Jr., Esq."),
#'              "Lt. Cdr. Joe Adam 'Nobby' de la /Bloggs/ Jr. Esq.")
construct_full_name <- function(prefix = character(), 
                                given = character(), 
                                nickname = character(), 
                                surname_prefix = character(), 
                                surname = character(), 
                                suffix = character()) {
  
  if(length(surname_prefix) == 1 & length(surname) == 0)
    stop("Surname prefix given without a surname")
  
  paste(
    prefix,  #up to first comma
    stringr::str_replace_all(given, ", ?", " "), 
    ifelse(length(nickname) == 1, paste0("'", nickname, "'"), ""), #up to first comma
    surname_prefix, 
    ifelse(length(surname) == 1, paste0("/", surname, "/"), ""),
    stringr::str_replace_all(suffix, ", ?", " ")
  ) %>% 
    stringr::str_squish()
  
}


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


#' Get all spouses for an individual
#'
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param return_name Whether to return the spouse's name(s) instead of the xref(s).
#'
#' @return A character vector of spouse xrefs or names.
#' @export
get_spouses <- function(gedcom,
                       individual = character(),
                       return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  fams_xref <- get_families_as_spouse(gedcom, xref)
  
  # we don't use purrr::map here because the return values could vary in length
  spou_xref <- NULL
  for(i in seq_along(fams_xref)){
    spou_xref <- gedcom %>% 
      dplyr::filter(record == fams_xref[i], tag %in% c("HUSB","WIFE"),
                    value != xref) %>% 
      dplyr::pull(value) %>% 
      c(spou_xref)
  }

  if (return_name) {
    purrr::map_chr(spou_xref, describe_indi, gedcom=gedcom, name_only = TRUE)
  } else {
    spou_xref
  }
}


#' Get all children for an individual
#'
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param return_name Whether to return the childrens name(s) instead of the xref(s).
#'
#' @return A character vector of children xrefs or names.
#' @export
get_children <- function(gedcom,
                         individual = character(),
                         return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  fams_xref <- get_families_as_spouse(gedcom, xref)
  
  chil_xref <- gedcom %>% 
    dplyr::filter(level == 1, record %in% fams_xref, tag == "CHIL") %>% 
    dplyr::pull(value) %>% 
    unique()
  
  if (return_name) {
    purrr::map_chr(chil_xref, describe_indi, gedcom=gedcom, name_only=TRUE)
  } else {
    chil_xref
  }
  
}

#' Get all parents for an individual
#'
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param return_name Whether to return the parents name(s) instead of the xref(s).
#'
#' @return A character vector of parent xrefs or names.
#' @export
get_parents <- function(gedcom,
                         individual = character(),
                         return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  famc_xref <- get_families_as_child(gedcom, xref)
  
  par_xref <- gedcom %>% 
    dplyr::filter(level == 1, record %in% famc_xref, tag %in% c("HUSB","WIFE")) %>% 
    dplyr::pull(value) %>% 
    unique()
  
  if (return_name) {
    purrr::map_chr(par_xref, describe_indi, gedcom=gedcom, name_only=TRUE)
  } else {
    par_xref
  }
  
}

#' Get all families for an individual where they are a spouse
#'
#' @param gedcom A tidygedcom object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#'
#' @return A character vector of family xrefs.
#' @export
get_families_as_spouse <- function(gedcom, individual = character()) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  gedcom %>% 
    dplyr::filter(level == 1, tag %in% c("HUSB", "WIFE"), value == xref) %>% 
    dplyr::pull(record) %>% 
    unique()
  
}

#' Get all families for an individual where they are a child
#'
#' @param gedcom A tidygedcom object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#'
#' @return A character vector of family xrefs.
#' @export
get_families_as_child <- function(gedcom, individual = character()) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  gedcom %>% 
    dplyr::filter(level == 1, tag == "CHIL", value == xref) %>% 
    dplyr::pull(record) %>% 
    unique()
  
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
    
  } else if (grepl(xref_pattern(), xref_or_descriptor)) {
    # xref given explicitly
    xref <- xref_or_descriptor
    
  } else {
    # xref given by descriptor, find it
    if (record_type == .pkgenv$record_string_indi) {
      
      xref <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_famg) {
      
      stop("The selected family record is not valid")
      
    } else if(record_type == .pkgenv$record_string_repo) {
      
      xref <- find_xref(gedcom, xrefs_repositories(gedcom), "NAME", xref_or_descriptor) 
      
    } else if(record_type == .pkgenv$record_string_sour) {
      
      xref <- find_xref(gedcom, xrefs_sources(gedcom), "TITL", xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_obje) {
      
      xref <- find_xref(gedcom, xrefs_multimedia(gedcom), "FILE", xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_note) {
      
      xref <- find_xref(gedcom, xrefs_notes(gedcom), "NOTE", xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_subm) {
      
      xref <- find_xref(gedcom, xrefs_submitters(gedcom), "NAME", xref_or_descriptor)
    }
    
  }

  if(is.null(xref))
    stop("No xref is provided and no ", record_type, " record is activated.")
  
  if(!record_type_fn(gedcom, xref))
    stop("The provided or active record is not a ", record_type, " record")
  
  xref
}



#' Find a particular row position in a tidyged object.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the record where the insertion point will be.
#' @param parent_level The level of the row where the insertion point will be.
#' @param parent_tag The tag of the row where the insertion point will be.
#' @param parent_value The value of the row where the insertion point will be.
#'
#' @return The row after the insertion point in the tidyged object.
find_insertion_point <- function(gedcom,
                                 xref,
                                 parent_level,
                                 parent_tag,
                                 parent_value = NULL) {
  
  active <- FALSE
  for(i in seq_len(nrow(gedcom))) {
    
    if(active && gedcom$level[i] <= parent_level) break
    
    if(gedcom$record[i] == xref && gedcom$level[i] == parent_level && gedcom$tag[i] == parent_tag) {
      if(is.null(parent_value) || gedcom$value[i] == parent_value) {
        active <- TRUE  
      }
    } 
      
  }
  i
}



#' Identify unreferenced records
#' 
#' This function identifies records that are not referenced in any other records.
#' 
#' @details You would expect every record to be referenced by another in some way. For example, Individual
#' records should reference Family Group records (and vice-versa), Repository records should be referenced
#' by Source records, and Source records should be cited by other records.
#' 
#' You can use the output of this function with remove_records() to remove them, or describe_records() to
#' find out more about them.
#'
#' @param gedcom A tidyged object.
#'
#' @return A vector of xrefs that are not referenced anywhere else in the tidyged object.
#' @export
identify_unused_records <- function(gedcom) {
  
  xrefs_indi <- xrefs_individuals(gedcom)
  xrefs_fam <- xrefs_families(gedcom)
  xrefs_media <- xrefs_multimedia(gedcom)
  xrefs_sour <- xrefs_sources(gedcom)
  xrefs_repo <- xrefs_repositories(gedcom)
  xrefs_note <- xrefs_notes(gedcom)
  
  # get unattached individuals
  attached <- unique(dplyr::filter(gedcom, record %in% xrefs_fam, tag %in% c("HUSB","WIFE","CHIL"))$value)
  unattached <- dplyr::setdiff(xrefs_indi, attached)
  
  #also look at family links perspective to check consistency
  attached2 <- unique(dplyr::filter(gedcom, record %in% xrefs_indi, tag %in% c("FAMC","FAMS"))$record)
  unattached2 <- dplyr::setdiff(xrefs_indi, attached2)
  
  if (!identical(sort(attached), sort(attached2)))
    warning("Family group membership and individual family links are inconsistent")
  
  # get empty families
  nonempty <- unique(dplyr::filter(gedcom, record %in% xrefs_fam, tag %in% c("HUSB","WIFE","CHIL"))$record)
  empty <- dplyr::setdiff(xrefs_fam, nonempty) 
  
  #get unused media
  used_media <- unique(dplyr::filter(gedcom, !record %in% xrefs_repo, tag == "OBJE")$value)
  unused_media <- dplyr::setdiff(xrefs_media, used_media) 
  
  #get unused sources
  used_sour <- unique(dplyr::filter(gedcom, !record %in% xrefs_sour, tag == "SOUR")$value)
  unused_sour <- dplyr::setdiff(xrefs_sour, used_sour)
  
  #get unused repos
  used_repo <- unique(dplyr::filter(gedcom, !record %in% xrefs_repo, tag == "REPO")$value)
  unused_repo <- dplyr::setdiff(xrefs_repo, used_repo)
  
  #get unused notes
  used_notes <- unique(dplyr::filter(gedcom, !record %in% xrefs_note, tag == "NOTE")$value)
  unused_notes <- dplyr::setdiff(xrefs_note, used_notes)
  
  c(unattached, empty, unused_media, unused_sour, unused_repo, unused_notes)
  
}


#' Remove multiple records at once
#'
#' @param gedcom A tidyged object.
#' @param xrefs A vector of xrefs to remove.
#'
#' @return An updated tidyged object with the records removed.
#' @export
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


#' Identify the rows of a subrecord in a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param containing_level The level of the first line of the subrecord.
#' @param containing_tag The tag of the first line of the subrecord.
#' @param containing_value The value of the first line of the subrecord.
#' @param xrefs The xrefs of records containing the subrecord (default is all records).
#'
#' @return A vector of rows in the tidyged object of the subrecord(s).
identify_section <- function(gedcom,
                           containing_level,
                           containing_tag,
                           containing_value,
                           xrefs = character()) {
  
  no_xrefs_defined <- length(xrefs) == 0
  
  rows_to_remove <- integer()
  
  active <- FALSE
  for(i in seq_len(nrow(gedcom))) {
    
    if(active) {
      if(gedcom$level[i] <= containing_level) {
        active <- FALSE
      } else {
        rows_to_remove <- c(rows_to_remove, i)
      }
      
    }
    
    if(no_xrefs_defined || gedcom$record[i] %in% xrefs) {
      if(gedcom$level[i] == containing_level & gedcom$tag[i] == containing_tag &
         gedcom$value[i] == containing_value) {
        
        active <- TRUE
        rows_to_remove <- c(rows_to_remove, i) 
      } 
      
    }
  }
  rows_to_remove
  
}


#' Remove a subrecord in a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param containing_level The level of the first line of the subrecord.
#' @param containing_tag The tag of the first line of the subrecord.
#' @param containing_value The value of the first line of the subrecord.
#' @param xrefs The xrefs of records containing the subrecord (default is all records).
#'
#' @return The tidyged object with the subrecord(s) removed.
remove_section <- function(gedcom,
                           containing_level,
                           containing_tag,
                           containing_value,
                           xrefs = character()) {
  
  rows_to_remove <- identify_section(gedcom,
                                     containing_level,
                                     containing_tag,
                                     containing_value,
                                     xrefs)
  
  if(length(rows_to_remove) == 0) {
    gedcom
  } else {
    dplyr::slice(gedcom, -rows_to_remove)
  }
  
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
    remove_change_dates() %>% 
    dplyr::filter(!(level == 1 & record == "HD" & tag == "DATE"))
  
}


#' Remove all CHANge dates from a tidyged object
#'
#' @param gedcom A tidyged object.
#'
#' @return A tidyged object with all CHAN structures removed.
#' @export
remove_change_dates <- function(gedcom) {
  
  gedcom %>% 
    remove_section(1, "CHAN", "")
  
}
