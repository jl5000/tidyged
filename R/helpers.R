

`%nin%` <- Negate(`%in%`)


is_individual <- function(gedcom, xref) {
  dplyr::filter(gedcom, record == xref)$tag[1] == record_tag_indi()
}
is_family <- function(gedcom, xref) {
  dplyr::filter(gedcom, record == xref)$tag[1] == record_tag_fam()
}
is_submitter <- function(gedcom, xref) {
  dplyr::filter(gedcom, record == xref)$tag[1] == record_tag_subm()
}
is_submission <- function(gedcom, xref) {
  dplyr::filter(gedcom, record == xref)$tag[1] == record_tag_subn()
}
is_repository <- function(gedcom, xref) {
  dplyr::filter(gedcom, record == xref)$tag[1] == record_tag_repo()
}
is_multimedia <- function(gedcom, xref) {
  dplyr::filter(gedcom, record == xref)$tag[1] == record_tag_obje()
}
is_note <- function(gedcom, xref) {
  dplyr::filter(gedcom, record == xref)$tag[1] == record_tag_note()
}
is_source <- function(gedcom, xref) {
  dplyr::filter(gedcom, record == xref)$tag[1] == record_tag_sour()
}

individual_xrefs <- function(gedcom) { dplyr::filter(gedcom, level == 0 & tag == record_tag_indi())$record }
family_xrefs <-     function(gedcom) { dplyr::filter(gedcom, level == 0 & tag == record_tag_fam())$record }
submitter_xrefs <-  function(gedcom) { dplyr::filter(gedcom, level == 0 & tag == record_tag_subm())$record }
submission_xrefs <- function(gedcom) { dplyr::filter(gedcom, level == 0 & tag == record_tag_subn())$record }
source_xrefs <-     function(gedcom) { dplyr::filter(gedcom, level == 0 & tag == record_tag_sour())$record }
repository_xrefs <- function(gedcom) { dplyr::filter(gedcom, level == 0 & tag == record_tag_repo())$record }
note_xrefs <-       function(gedcom) { dplyr::filter(gedcom, level == 0 & tag == record_tag_note())$record }
multimedia_xrefs <- function(gedcom) { dplyr::filter(gedcom, level == 0 & tag == record_tag_obje())$record }

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
#' @return The particular value fitting the criteria of the input arguments. If not value is found,
#' an empty string is returned. The function will automatically combine values split over CONT and CONC tags.
#' @export
#'
#' @examples
#' gedcom_value(gedcom, "@I1@", "NAME", 1)
gedcom_value <- function(gedcom, record_xref, tag, level, after_tag = NULL) {
  
  gedcom_filtered <- dplyr::filter(gedcom, record %in% record_xref)
  if(nrow(gedcom_filtered) == 0) return("")
  
  active <- is.null(after_tag)
  for(i in 1:nrow(gedcom_filtered)) {
    if(is.null(after_tag)) {
      active <- TRUE
    } else if(gedcom_filtered$tag[i] == after_tag && gedcom_filtered$level[i] < level) {
      active <- TRUE
    } else if(active && gedcom_filtered$level[i] > level){
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


#' Create a new xref for a record
#' 
#' This function is used to assign xrefs to new records that are created.
#'
#' @param type The type of record, given by one of the xref_prefix_*() functions
#' @param ref An explicit reference string (xref without the "@") if one is to be chosen manually
#' @param gedcom A tidygedcm object
#'
#' @return An xref to use for a new record
#' @export
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





#' Split a vector of free text into separate rows of a GEDCOM file
#' 
#' This function only uses the CONC(atenation) tag for splitting free text, and does
#' not use the CONT(inuation) tag. This is because it is easier to implement.
#'
#' @param text A character vector of free text.
#' @param top_tag The GEDCOM tag of the first line.
#' @param char_limit The maximum number of characters allowed on each line. Defaults to 248.
#' @param start_level The level of the first line. Defaults to 0.
#' @tests
#' @return A tidy tibble containing the GEDCOM representation of the character vector.
split_text <- function(text, top_tag, char_limit = 248, start_level = 0) {
   
  purrr::map_dfr(text, function(txt) {
    
    
    
    num_pieces <- ceiling(nchar(txt) / char_limit)
    
    for (i in seq_len(num_pieces)) {
      if (i == 1) {
        return_df <- tibble::tibble(level = start_level, tag = top_tag, 
                                    value = stringr::str_sub(txt, 1, char_limit))
      } else {
        return_df <- 
          dplyr::bind_rows(return_df,
                           tibble::tibble(level = start_level + 1, tag = "CONC", 
                                          value = stringr::str_sub(txt, 
                                                                   char_limit*(i-1) + 1,
                                                                   char_limit*i))) 
      }
      
    }
    return_df
    
  })
  
}

