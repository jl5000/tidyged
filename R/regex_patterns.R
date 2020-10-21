
#' @keywords internal
group_it <- function(reg) {
  paste0("(?:", reg, ")")
}

#' @keywords internal
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

xref_pattern <- function() {
  #p31
  "^@[a-zA-Z0-9]{1,20}@$"
}

latitude_pattern <- function() {
  "^[NS]\\d{1,2}\\.\\d{1,6}$"
}

longitude_pattern <- function() {
  "^[EW]\\d{1,3}\\.\\d{2,6}$"
}

age_at_event_pattern <- function() {
  paste0("[<>]?", #TODO: handle the extra space
         "\\d{1,3}y \\d{1,2}m \\d{1,3}d$|",
         "\\d{1,3}y \\d{1,2}m$|",
         "\\d{1,3}y \\d{1,3}d$|",
         "\\d{1,2}m \\d{1,3}d$|",
         "\\d{1,3}y$|",
         "\\d{1,2}m$|",
         "\\d{1,3}d$|",
         "^CHILD$|",
         "^INFANT$|",
         "^STILLBORN$")
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
date_value_pattern <- function() {
  
  #date_phrase not implemented
  c(date_pattern(FALSE),
    date_period_pattern(FALSE),
    date_range_pattern(FALSE),
    date_approximated_pattern(FALSE)) %>% 
    anchor_it() %>% paste(collapse = "|")
}
