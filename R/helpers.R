

`%nin%` <- Negate(`%in%`)

set_class_to_tidygedcom <- function(gedcom) {
  class(gedcom) <- c("tidygedcom", "tbl_df", "tbl", "data.frame")
  gedcom
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


ref_to_xref <- function(ref, type) {
  
  if (length(ref) == 0) return(character())
  paste0("@", toupper(type), ref, "@")
  
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
  
