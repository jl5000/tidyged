

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

#' Construct the regex pattern for DATE_EXACT values
#'
#' @tests
#' expect_equal(grepl(date_exact_pattern(), "14 JAN 2005"), TRUE)
#' expect_equal(grepl(date_exact_pattern(), "14 JAM 2005"), FALSE)
#' expect_equal(grepl(date_exact_pattern(), "JAN 2005"), FALSE)
#' expect_equal(grepl(date_exact_pattern(), "14 JAN 2005/06"), TRUE)
#' expect_equal(grepl(date_exact_pattern(), "5 JUL 2005"), TRUE)
#' expect_equal(grepl(date_exact_pattern(), "8 NOV 1956/57"), TRUE)
#' expect_equal(grepl(date_exact_pattern(), "2005"), FALSE)
#' expect_equal(grepl(date_exact_pattern(), "15 NOV 125"), FALSE)
#' expect_equal(grepl(date_exact_pattern(), "JAN 1901/58"), FALSE)
#' expect_equal(grepl(date_exact_pattern(), "5 JUL 2005 "), FALSE)
#' expect_equal(grepl(date_exact_pattern(), " 5 JUL 2005"), FALSE)
#' @return A regex string
#' @export
date_exact_pattern <- function() {
  paste(day_pattern(), month_pattern(), year_pattern()) %>% anchor_it()
}

#' Construct the regex pattern for DATE values
#'
#' @param flatten A logical value which determines whether a single regex string should be
#' returned (flatten = TRUE) or if a vector of them should be returned (flatten = FALSE).
#' The vector output is used if the regexes need to be combined with other regexes. If they
#' do not, then they are anchored with ^ and $ and separated with | (OR).
#' @tests
#' expect_equal(grepl(date_pattern(), "14 JAN 2005"), TRUE)
#' expect_equal(grepl(date_pattern(), "14 JAM 2005"), FALSE)
#' expect_equal(grepl(date_pattern(), "JAN 2005"), TRUE)
#' expect_equal(grepl(date_pattern(), "14 JAN 2005/06"), TRUE)
#' expect_equal(grepl(date_pattern(), "5 JUL 2005"), TRUE)
#' expect_equal(grepl(date_pattern(), "8 NOV 1956/57"), TRUE)
#' expect_equal(grepl(date_pattern(), "2005"), TRUE)
#' expect_equal(grepl(date_pattern(), "15 NOV 125"), FALSE)
#' expect_equal(grepl(date_pattern(), "JAN 1901/58"), TRUE)
#' expect_equal(grepl(date_pattern(), "5 JUL 2005 "), FALSE)
#' expect_equal(grepl(date_pattern(), " 5 JUL 2005"), FALSE)
#' @return Either a single regex string or a vector of them
#' @export
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

#' Construct the regex pattern for DATE_PERIOD values
#'
#' @param flatten A logical value which determines whether a single regex string should be
#' returned (flatten = TRUE) or if a vector of them should be returned (flatten = FALSE).
#' The vector output is used if the regexes need to be combined with other regexes. If they
#' do not, then they are anchored with ^ and $ and separated with | (OR).
#' @tests
#' expect_equal(grepl(date_period_pattern(), "FROM 14 JAN 2005"), TRUE)
#' expect_equal(grepl(date_period_pattern(), "TO 14 JAM 2005"), FALSE)
#' expect_equal(grepl(date_period_pattern(), "FROM JAN 2005"), TRUE)
#' expect_equal(grepl(date_period_pattern(), "FROM 14 JAN 2005/06 TO 2007"), TRUE)
#' expect_equal(grepl(date_period_pattern(), "TO 5 JUL 2005"), TRUE)
#' expect_equal(grepl(date_period_pattern(), "TO  8 NOV 1956/57"), FALSE)
#' expect_equal(grepl(date_period_pattern(), "FROM 2005"), TRUE)
#' expect_equal(grepl(date_period_pattern(), "FROM 15 NOV 125"), FALSE)
#' expect_equal(grepl(date_period_pattern(), " TO JAN 1901/58"), FALSE)
#' expect_equal(grepl(date_period_pattern(), "FROM 5 JUL 2005 "), FALSE)
#' expect_equal(grepl(date_period_pattern(), " TO 5 JUL 2005"), FALSE)
#' @return Either a single regex string or a vector of them
#' @export
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

#' Construct the regex pattern for DATE_RANGE values
#'
#' @param flatten A logical value which determines whether a single regex string should be
#' returned (flatten = TRUE) or if a vector of them should be returned (flatten = FALSE).
#' The vector output is used if the regexes need to be combined with other regexes. If they
#' do not, then they are anchored with ^ and $ and separated with | (OR).
#' @tests
#' expect_equal(grepl(date_range_pattern(), "BEF 14 JAN 2005"), TRUE)
#' expect_equal(grepl(date_range_pattern(), "AFT 14 JAM 2005"), FALSE)
#' expect_equal(grepl(date_range_pattern(), "BEF JAN 2005"), TRUE)
#' expect_equal(grepl(date_range_pattern(), "BET 14 JAN 2005/06 AND 2007"), TRUE)
#' expect_equal(grepl(date_range_pattern(), "AFT 5 JUL 2005"), TRUE)
#' expect_equal(grepl(date_range_pattern(), "AFT  8 NOV 1956/57"), FALSE)
#' expect_equal(grepl(date_range_pattern(), "BEF 2005"), TRUE)
#' expect_equal(grepl(date_range_pattern(), "BEF 15 NOV 125"), FALSE)
#' expect_equal(grepl(date_range_pattern(), " AFT JAN 1901/58"), FALSE)
#' expect_equal(grepl(date_range_pattern(), "BEF 5 JUL 2005 "), FALSE)
#' expect_equal(grepl(date_range_pattern(), " AFT 5 JUL 2005"), FALSE)
#' @return Either a single regex string or a vector of them
#' @export
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

#' Construct the regex pattern for DATE_APPROXIMATED values
#'
#' @param flatten A logical value which determines whether a single regex string should be
#' returned (flatten = TRUE) or if a vector of them should be returned (flatten = FALSE).
#' The vector output is used if the regexes need to be combined with other regexes. If they
#' do not, then they are anchored with ^ and $ and separated with | (OR).
#' @tests
#' expect_equal(grepl(date_approximated_pattern(), "ABT 14 JAN 2005"), TRUE)
#' expect_equal(grepl(date_approximated_pattern(), "CAL 14 JAM 2005"), FALSE)
#' expect_equal(grepl(date_approximated_pattern(), "EST JAN 2005"), TRUE)
#' expect_equal(grepl(date_approximated_pattern(), "ABT 14 JAN 2005/06 AND 2007"), FALSE)
#' expect_equal(grepl(date_approximated_pattern(), "EST 5 JUL 2005"), TRUE)
#' expect_equal(grepl(date_approximated_pattern(), "CAL  8 NOV 1956/57"), FALSE)
#' expect_equal(grepl(date_approximated_pattern(), "ABT 2005"), TRUE)
#' expect_equal(grepl(date_approximated_pattern(), "CAL 15 NOV 125"), FALSE)
#' expect_equal(grepl(date_approximated_pattern(), " EST JAN 1901/58"), FALSE)
#' expect_equal(grepl(date_approximated_pattern(), "CAL 5 JUL 2005 "), FALSE)
#' expect_equal(grepl(date_approximated_pattern(), " CAL 5 JUL 2005"), FALSE)
#' @return Either a single regex string or a vector of them
#' @export
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

#' Construct the regex pattern for DATE_VALUE values
#'
#' @param flatten A logical value which determines whether a single regex string should be
#' returned (flatten = TRUE) or if a vector of them should be returned (flatten = FALSE).
#' The vector output is used if the regexes need to be combined with other regexes. If they
#' do not, then they are anchored with ^ and $ and separated with | (OR).
#' @tests
#' expect_equal(grepl(date_value_pattern(), "14 JAN 2005"), TRUE)
#' expect_equal(grepl(date_value_pattern(), "MAR 1901"), TRUE)
#' expect_equal(grepl(date_value_pattern(), "2010"), TRUE)
#' expect_equal(grepl(date_value_pattern(), "FROM 14 FEB 2005"), TRUE)
#' expect_equal(grepl(date_value_pattern(), "TO JAN 2005"), TRUE)
#' expect_equal(grepl(date_value_pattern(), "FROM 14 JAN 2005/06 TO 2007"), TRUE)
#' expect_equal(grepl(date_value_pattern(), "BEF 5 JUL 2005"), TRUE)
#' expect_equal(grepl(date_value_pattern(), "AFT 8 NOV 1956/57"), TRUE)
#' expect_equal(grepl(date_value_pattern(), "BET 2005 AND MAR 2008"), TRUE)
#' expect_equal(grepl(date_value_pattern(), "CAL 15 NOV 1925"), TRUE)
#' expect_equal(grepl(date_value_pattern(), "EST JAN 1901/58"), TRUE)
#' expect_equal(grepl(date_value_pattern(), "ABT 5 JUL 2005"), TRUE)
#' expect_equal(grepl(date_value_pattern(), "14 JAN 205"), FALSE)
#' expect_equal(grepl(date_value_pattern(), "MAR 1901 "), FALSE)
#' expect_equal(grepl(date_value_pattern(), " 2010"), FALSE)
#' expect_equal(grepl(date_value_pattern(), "FROM 14 FEBR 2005"), FALSE)
#' expect_equal(grepl(date_value_pattern(), "TO  JAN 2005"), FALSE)
#' expect_equal(grepl(date_value_pattern(), "FROM 14 JAN 2005/06 AND 2007"), FALSE)
#' expect_equal(grepl(date_value_pattern(), "BEF 5 JUL 2005 "), FALSE)
#' expect_equal(grepl(date_value_pattern(), "AFT 8 NOV 1956/1957"), FALSE)
#' expect_equal(grepl(date_value_pattern(), "BET 2005 TO MAR 2008"), FALSE)
#' expect_equal(grepl(date_value_pattern(), "CAL 15 NOV 1925/"), FALSE)
#' expect_equal(grepl(date_value_pattern(), "14TH JAN 1901/58"), FALSE)
#' expect_equal(grepl(date_value_pattern(), "ABT 5  JUL 2005"), FALSE)
#' @return Either a single regex string or a vector of them
#' @export
date_value_pattern <- function() {
  #date_phrase not implemented
  c(date_pattern(FALSE),
    date_period_pattern(FALSE),
    date_range_pattern(FALSE),
    date_approximated_pattern(FALSE)) %>% 
    anchor_it() %>% paste(collapse = "|")
}



#' Construct a DATE_EXACT string
#'
#' @param day The day of the month.
#' @param month The month of the year.
#' @param year The year.
#' @tests
#' expect_equal(date_exact(12), character())
#' expect_equal(date_exact(12, 8), character())
#' expect_equal(date_exact(12, 8, 2005), "12 AUG 2005")
#' @return A DATE_EXACT string
#' @export
date_exact <- function(day = numeric(), 
                       month = numeric(), 
                       year = numeric()) {
  
  if (length(day) + length(month) + length(year) < 3) return(character())
  
  paste(day, toupper(month.abb[month]), year)
  
}


#' Construct a DATE_VALUE string
#'
#' @param start_year The year (or start year if a date range/period is provided). This value is required.
#' @param start_month The month of the year (or start month if a date range/period is provided)
#' @param start_day The day of the month (or start day if a date range/period is provided)
#' @param end_year The end year
#' @param end_month The end month of the year
#' @param end_day The end day of the month
#' @param from Whether the date given by start_year/month/day should be considered the beginning of a period.
#' If 'to' is FALSE then the period is semi-infinite.
#' @param to Whether the date given by end_year/month/day should be considered the end of a period.
#' If 'from' is FALSE then the period is semi-infinite.
#' @param before Whether the date given by start_year/month/day should be considered the end of a range.
#' @param after Whether the date given by start_year/month/day should be considered the beginning of a range.
#' @param between Whether the start and end dates should be considered as the bounds of a date range.
#' @param about Whether the date given by start_year/month/day is approximate.
#' @param calc Whether the date given by start_year/month/day is calculated from other values.
#' @param est Whether the date given by start_year/month/day is estimated.
#' @tests
#' expect_equal(date_value(2005), "2005")
#' expect_equal(date_value(2005, 1), "JAN 2005")
#' expect_equal(date_value(2005, 1, 14), "14 JAN 2005")
#' expect_equal(date_value(2005, after = TRUE), "AFT 2005")
#' expect_equal(date_value(2010, before = TRUE), "BEF 2010")
#' expect_equal(date_value(2005, 10, 14, before = TRUE), "BEF 14 OCT 2005")
#' expect_equal(date_value(1900, 6, 30, from = TRUE), "FROM 30 JUN 1900")
#' expect_equal(date_value(2000, 1, 1, to = TRUE), "TO 1 JAN 2000")
#' expect_equal(date_value(2005, 1, 14, 2006, 7, 9, from = TRUE, to = TRUE), "FROM 14 JAN 2005 TO 9 JUL 2006")
#' expect_equal(date_value(2005, 1, 14, 2006, 7, 9, between = TRUE), "BET 14 JAN 2005 AND 9 JUL 2006")
#' expect_equal(date_value(2005, 1, 14, calc = TRUE), "CAL 14 JAN 2005")
#' expect_equal(date_value(2005, est = TRUE), "EST 2005")
#' expect_equal(date_value(2005, 1, about = TRUE), "ABT JAN 2005")
#' @return A DATE_VALUE string
#' @export
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
  
  if (before) {
    val <- "BEF"
  } else if (after) {
    val <- "AFT"
  } else if (between) {
    val <- "BET"
  } else if (about) {
    val <- "ABT"
  } else if (calc) {
    val <- "CAL"
  } else if (est) {
    val <- "EST"
  }
  
  if (length(start_day) == 1) val <- paste(val, start_day)
  if (length(start_month) == 1) val <- paste(val, toupper(month.abb[start_month]))
  val <- paste(val, start_year)
  
  if (length(end_year) == 1) {
    if (to) {
      val <- paste(val, "TO")
    } else if (between) {
      val <- paste(val, "AND")
    }
    
    if (length(end_day) == 1) val <- paste(val, end_day)
    if (length(end_month) == 1) val <- paste(val, toupper(month.abb[end_month]))
    val <- paste(val, end_year)
  }
  
  stringr::str_trim(val)
}

