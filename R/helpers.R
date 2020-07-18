

`%nin%` <- Negate(`%in%`)

xref_prefix_indi <- function() {"I"}
xref_prefix_fam <- function() {"F"}
xref_prefix_subm <- function() {"U"}
xref_prefix_subn <- function() {"G"}
xref_prefix_repo <- function() {"R"}
xref_prefix_obje <- function() {"O"}
xref_prefix_note <- function() {"N"}
xref_prefix_sour <- function() {"S"}


set_class_to_tidygedcom <- function(gedcom) {
  class(gedcom) <- c("tidygedcom", "tbl_df", "tbl", "data.frame")
  gedcom
}


gedcom_value <- function(gedcom, section, tag, level, after_tag = NULL) {
  
  gedcom_filtered <- dplyr::filter(gedcom, id %in% section)
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
    tidyr::fill(id)
  
}


assign_xref <- function(type, ref = 0, gedcom = tibble::tibble()) {
  
  if (ref == 0) {
    gedcom_filt <- gedcom %>% 
      dplyr::filter(stringr::str_detect(id, paste0("@", type, "\\d+@"))) 
    
    if(nrow(gedcom_filt) == 0) {
      ref <- 1
    } else {
      ref <- gedcom_filt %>%
        dplyr::pull(id) %>% 
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

