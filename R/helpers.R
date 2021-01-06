

`%nin%` <- Negate(`%in%`)

is_record_type <- function(gedcom, xref, tag) {
  dplyr::filter(gedcom, record == xref)$tag[1] == tag
}

#' Check whether a given record is a particular type
#'
#' @param gedcom A tidygedcom object.
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

xrefs_record_type <- function(gedcom, record_tag) {
  dplyr::filter(gedcom, level == 0 & tag == record_tag)$record
}

xrefs_individuals <-  function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_indi) }
xrefs_families <-     function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_fam)  }
xrefs_submitters <-   function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_subm) }
xrefs_sources <-      function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_sour) }
xrefs_repositories <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_repo) }
xrefs_notes <-        function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_note) }
xrefs_multimedia <-   function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_obje) }

set_class_to_tidygedcom <- function(gedcom) {
  class(gedcom) <- c("tidygedcom", "tbl_df", "tbl", "data.frame")
  gedcom
}


#' Extract a particular value from a tidygedcom object
#'
#' @param gedcom A tidygedcom object
#' @param record_xref The xref of the record in which the value may exist
#' @param tag The tag associated with the value
#' @param level The level number of the value
#' @param after_tag Whether the tag should be subordinate to this parent tag 
#'
#' @return The particular value fitting the criteria of the input arguments. If no value is found,
#' an empty string is returned.
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

get_individual_name <- function(gedcom, xref) { 
  name <- gedcom_value(gedcom, xref, "NAME", 1, "INDI") %>% 
    stringr::str_remove_all("/")
  
  if (name == "") name <- xref
  name
}

get_family_group_description <- function(gedcom, xref) {
  
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

temporarily_remove_name_slashes <- function(gedcom) {
  
  gedcom %>% 
    dplyr::mutate(value = dplyr::if_else(purrr::map_lgl(record, is_individual, gedcom=gedcom) &
                                           tag %in% c("NAME", "FONE", "ROMN"),
                                         stringr::str_remove_all(value, "/"),
                                         value))
  
}

add_levels <- function(df, start_level) {
  
  if (nrow(df) == 0) return(df)
  
  df %>% 
    dplyr::mutate(level = start_level + level)
  
}


finalise <- function(df, global_start_level = 0) {
  
  df %>% 
    dplyr::mutate(level = global_start_level + level) %>%
    tidyr::fill(record)
  
}


get_valid_xref <- function(gedcom, xref_or_descriptor, record_type, record_type_fn) {
  
  if (length(xref_or_descriptor) == 0) {
    
    xref <- get_active_record(gedcom)
    
  } else if (grepl(xref_pattern(), xref_or_descriptor)) {
    
    xref <- xref_or_descriptor
    
  } else {
    
    if (record_type == .pkgenv$record_string_indi) {
      
      xref <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_fam) {
      
      stop("The selected family record is not valid")
      
    } else if(record_type == .pkgenv$record_string_repo) {
      
      xref <- find_xref(gedcom, xrefs_repositories(gedcom), "NAME", xref_or_descriptor) 
      
    } else if(record_type == .pkgenv$record_string_sour) {
      
      xref <- find_xref(gedcom, xrefs_sources(gedcom), "TITL", xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_obje) {
      
      xref <- find_xref(gedcom, xrefs_multimedia(gedcom), "FILE", xref_or_descriptor)
      
    } else if(record_type == .pkgenv$record_string_note) {
      
      xref <- find_xref(gedcom, xrefs_notes(gedcom), "NOTE", xref_or_descriptor)
    }
    
  }

  if(is.null(xref))
    stop("No xref is provided and no ", record_type, " record is activated.")
  
  if(!record_type_fn(gedcom, xref))
    stop("The provided or active record is not a ", record_type, " record")
  
  xref
}

#' Create a new xref for a record
#' 
#' This function is used to assign xrefs to new records that are created.
#'
#' @param type The type of record, given by one of the xref_prefix_*() functions
#' @param ref An explicit reference string (xref without the "@") if one is to be chosen manually
#' @param gedcom A tidygedcom object
#'
#' @return An xref to use for a new record
assign_xref <- function(type, ref = 0, gedcom = tibble::tibble()) {
  
  if (ref == 0) {
    gedcom_filt <- gedcom %>% 
      dplyr::filter(stringr::str_detect(record, paste0("@", type, "\\d+@"))) 
    
    if(nrow(gedcom_filt) == 0) {
      ref <- 1
    } else {
      ref <- gedcom_filt %>%
        dplyr::pull(record) %>% 
        unique() %>% 
        stringr::str_remove_all("@") %>% 
        stringr::str_remove_all("[A-Z]") %>% 
        as.numeric() %>% 
        max() + 1
      
    }
    
  }
  paste0("@", type, ref, "@")
  
}







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

salvage_name_pieces <- function(full_name, name_pieces) {
  
  if(nrow(name_pieces) > 0) return(name_pieces)
  
  if(stringr::str_detect(full_name, "/.+/")) {
    
    surname <- full_name %>% 
      stringr::str_extract("/.+/") %>% 
      stringr::str_remove_all("/")
    
    name_pieces <- PERSONAL_NAME_PIECES(name_piece_surname = surname)
    
  } else {
    stop("The name ", full_name, " is given without any name pieces")
  }
  
  name_pieces
}



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

remove_dates_for_tests <- function(gedcom) {
  
  gedcom %>% 
    remove_change_dates() %>% 
    dplyr::filter(!(level == 1 & record == "HD" & tag == "DATE"))
  
}

remove_context_from_tests <- function() {
  
  files <- list.files("tests/testthat", "^test-.+\\.R$", full.names = TRUE)
  
  for (file in files) {
    text <- readLines(file)
    text <- purrr::discard(text, ~ substr(., 1, 7) == "context")
    writeLines(text, file)
  }
  
}

#' Remove all CHANge dates
#'
#' @param gedcom A tidygedcom object.
#'
#' @return A tidygedcom object with all CHAN structures removed.
#' @export
remove_change_dates <- function(gedcom) {
  
  gedcom %>% 
    remove_section(1, "CHAN", "")
  
}
