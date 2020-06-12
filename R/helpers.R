

#' Title
#' 
#' @description 
#' 
#' @details
#'
#' @param df 
#' @param start_offset 
#'
#' @return
#' @export
#'
#' @examples
add_offsets <- function(df, start_offset) {
  
  if (nrow(df) == 0) return(df)
  
  df %>% 
    mutate(offset = start_offset + offset)
  
}

finalise <- function(df, global_start_level = 0) {
  
  df %>% 
    rename(level = offset) %>% 
    mutate(level = global_start_level + level) %>%
    fill(id)
  
}


ref_to_xref <- function(ref, type) {
  
  paste0("@", toupper(type), ref, "@")
  
}


split_text <- function(text, char_limit) {
  # 248 character limit  
  if (nchar(text) <= char_limit) return(text)
  
  num_pieces <- nchar(text) %/% char_limit + 1
  stri_split_boundaries(text, num_pieces)
}


#dates/times