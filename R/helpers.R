

`%nin%` <- Negate(`%in%`)

add_levels <- function(df, start_level) {
  
  if (nrow(df) == 0) return(df)
  
  df %>% 
    mutate(level = start_level + level)
  
}

finalise <- function(df, global_start_level = 0) {
  
  df %>% 
    mutate(level = global_start_level + level) %>%
    fill(id)
  
}


ref_to_xref <- function(ref, type) {
  
  if (length(ref) == 0) return(character())
  paste0("@", toupper(type), ref, "@")
  
}


split_text <- function(text, top_tag, char_limit = 248, start_level = 0) {
   
  map_dfr(text, function(txt) {
    
    
    
    num_pieces <- ceiling(nchar(txt) / char_limit)
    
    for (i in seq_len(num_pieces)) {
      if (i == 1) {
        return_df <- tibble(level = start_level, tag = top_tag, 
                            value = stringr::str_sub(txt, 1, char_limit))
      } else {
        return_df <- 
          bind_rows(return_df,
                    tibble(level = start_level + 1, tag = "CONC", 
                           value = stringr::str_sub(txt, 
                                                    char_limit*(i-1) + 1,
                                                    char_limit*i))) 
      }
      
    }
    return_df
    
  })
  
}
  
