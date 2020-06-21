

`%nin%` <- Negate(`%in%`)

#' Title
#' 
#' @description 
#' 
#' @details
#'
#' @param df 
#' @param start_level 
#'
#' @return
#' @export
#'
#' @examples
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
  

#dates/times
date_exact <- function(day = numeric(), 
                       month = numeric(), 
                       year = numeric()) {
  
  if (length(day) + length(month) + length(year) < 3) return(character())
  
  paste(day, toupper(month.abb[month]), year)
  
}
date_value <- function(start_year = numeric(),
                       start_month = numeric(),
                       start_day = numeric(),
                       end_year = numeric(),
                       end_month = numeric(),
                       end_day = numeric(),
                       from = FALSE,
                       to = FALSE,
                       before = FALSE,
                       after = FALSE,
                       between = FALSE,
                       about = FALSE,
                       calc = FALSE,
                       est = FALSE) {
  
  if (length(start_year) == 0) return(character())
  
  val <- ""
  if (from) val <- "FROM"
  if (to & length(end_year) == 0) val <- "TO"
  if (before) val <- "BEF"
  if (after) val <- "AFT"
  if (about) val <- "ABT"
  if (calc) val <- "CAL"
  if (est) val <- "EST"
  if (between) val <- "BET"
    
  if (length(start_day) == 1) val <- paste(val, start_day)
  if (length(start_month) == 1) val <- paste(val, toupper(month.abb[start_month]))
  val <- paste(val, start_year)
  
  if (length(end_year) == 1) {
    if (to) val <- paste(val, "TO")
    if (between) val <- paste(val, "AND")
    
    if (length(end_day) == 1) val <- paste(val, end_day)
    if (length(end_month) == 1) val <- paste(val, toupper(month.abb[end_month]))
    val <- paste(val, end_year)
  }
  
  str_trim(val)
}


group_it <- function(reg) {
  paste0("(?:", reg, ")")
}
anchor_it <- function(reg) {
  paste0("^", reg, "$")
}
regex_combn <- function(reg1, reg2) {
  paste(rep(reg1, each = length(reg2)), reg2, sep = "")
}
day_pattern <- function() {
 "\\d{1,2}" %>% group_it()
}
month_pattern <- function() {
  paste0(toupper(month.abb), collapse = "|") %>% group_it()
}
year_pattern <- function() {
  "\\d{4}(?:/\\d{2})?" %>% group_it()
}
date_exact_pattern <- function() {
  paste(day_pattern(), month_pattern(), year_pattern()) %>% anchor_it()
}
date_pattern <- function(flatten = TRUE) {
  combos <- c(paste(day_pattern(), month_pattern(), year_pattern()),
              paste(month_pattern(), year_pattern()),
              year_pattern())
  if (flatten) {
    combos %>% anchor_it() %>% paste(collapse = "|")
  } else {
    combos
  }
}
date_period_pattern <- function(flatten = TRUE) {
  combos <- c(paste("FROM", date_pattern(FALSE)),
              paste("TO", date_pattern(FALSE)),
              regex_combn(paste("FROM", date_pattern(FALSE)), 
                          paste(" TO", date_pattern(FALSE))))
  if (flatten) {
    combos %>% anchor_it() %>% paste(collapse = "|")
  } else {
    combos
  }
}
date_range_pattern <- function(flatten = TRUE) {
  combos <- c(paste("BEF", date_pattern(FALSE)),
              paste("AFT", date_pattern(FALSE)),
              regex_combn(paste("BET", date_pattern(FALSE)), 
                          paste(" AND", date_pattern(FALSE))))
  if (flatten) {
    combos %>% anchor_it() %>% paste(collapse = "|")
  } else {
    combos
  }
}
date_approximated_pattern <- function(flatten = TRUE) {
  combos <- c(paste("ABT", date_pattern(FALSE)),
              paste("CAL", date_pattern(FALSE)),
              paste("EST", date_pattern(FALSE)))
  if (flatten) {
    combos %>% anchor_it() %>% paste(collapse = "|")
  } else {
    combos
  }
}
date_value_pattern <- function() {
  #date_phrase not implemented
  c(date_pattern(FALSE),
    date_period_pattern(FALSE),
    date_range_pattern(FALSE),
    date_approximated_pattern(FALSE)) %>% 
    anchor_it() %>% paste(collapse = "|")
}