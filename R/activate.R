

#' Flag a record as being active
#' 
#' This allows an easy mechanism to edit particular records without specifying them each time.
#'
#' @param gedcom A tidyged object
#' @param xref The xref of the record to activate
#'
#' @return The same tidyged object with an "active_record" attribute set to the xref of the record
set_active_record <- function(gedcom, xref) {
  if(length(xref) > 0) attr(gedcom, "active_record") <- xref 
  return(gedcom)
}

#' Get the active record in a tidyged object
#' 
#' @param gedcom A tidyged object.
#' @return The xref of the active record.
#'
#' @export
get_active_record <- function(gedcom) {
  attr(gedcom, "active_record")
}


null_active_record <- function(gedcom) {
  attr(gedcom, "active_record") <- NULL
  return(gedcom)
}




#' Find an xref of a record given a set of search terms
#'
#' @details
#' This is a helper function to identify the xref of a record given information such
#' as a name or reference number. You provide a named `search_patterns` vector of namespaced tag-pattern pairs,
#' such as:
#' 
#' c(INDI.NAME = "Homer", INDI.SEX  = "M", INDI.BIRT.DATE = "JAN 1974")
#' 
#' If you're not sure what namespace to use, use the `mutate_tag_namespace` function.
#' 
#' The search patterns will be treated as regular expressions, so they will match a value if it contains
#' the pattern provided. You can anchor your search pattern if you want an exact match, e.g. "^JAN 1974$".
#' If you're not familiar with regular expressions, you may need to escape certain characters such as a
#' full-stop/period (i.e. `\\.`).
#' 
#' @param gedcom A tidyged object.
#' @param search_patterns A named vector of terms to search for (see Details).
#' @param mode Whether to only return an xref if all patterns are matched ("strict"). A value of
#' "best" will return the xref with the most matches. If either of these still result in more than
#'  one xref it will return an error.
#' @param multiple If more than one xref is found (according to mode), whether to return all xrefs
#' or throw an error.
#' @param ignore_case Should case differences be ignored in the match?
#'
#' @return A single xref for the given record. No matches will return an empty character vector.
#' @export
#' @tests
#' expect_error(find_xref(sample555, character()))
#' expect_error(find_xref(sample555, c(INDI.NAME = "test"), mode = "foo"))
#' expect_error(find_xref(sample555, letters[1:5]))
#' expect_error(find_xref(sample555, c(a = "das", "sd", b = "r42")))
#' expect_error(find_xref(sample555, c(INDI.SEX = "L")))
#' expect_error(find_xref(sample555, c(INDI.NAME.SURN = "Williams")))
#' expect_equal(find_xref(sample555, c(FAM.MARR.DATE = "1859")), "@F1@")
#' expect_equal(find_xref(sample555, c(INDI.SEX = "M", INDI.NAME.SURN = "Williams", INDI.BIRT.PLAC = "Falls")), "@I3@")
find_xref <- function(gedcom, search_patterns, mode = "strict", multiple = FALSE, ignore_case = FALSE) {
  
  mode <- tolower(mode)
  if(length(search_patterns) == 0) stop("At least one search pattern must be provided")
  if(!mode %in% c("strict","best")) stop("The mode must be either 'strict' or 'best'")
  if(is.null(names(search_patterns))) stop("The search_patterns vector must be named")
  if(any(names(search_patterns)=="")) stop("Each search pattern must be named with a namespace value")
  
  tags_ns <- toupper(names(search_patterns))
  search_patterns_val <- as.character(search_patterns) #unname
  
  gedcom_ns <- mutate_tag_namespace(gedcom)
  reg_case <- purrr::partial(stringr::regex, ignore_case = ignore_case)
  
  matches <- purrr::map2(tags_ns, search_patterns_val,
                         ~unique(dplyr::filter(gedcom_ns, 
                                        tag_ns == .x,
                                        stringr::str_detect(value, reg_case(.y)))$record))
  
  if(mode == "strict") {
    
    xref <- Reduce(intersect, matches)
    
    if(length(xref) == 0) stop("No records found that match all patterns.")
    if(length(xref) > 1 & !multiple) 
      stop("No unique records found that match all patterns. Try being more specific.")
    
  } else { #best
    
    xref <- tibble::tibble(matches = unlist(matches)) %>%
      dplyr::count(matches) %>% 
      dplyr::filter(n==max(n)) %>% 
      dplyr::pull(matches)
    
    if(nrow(xref) == 0) stop("No records found that match any patterns.")
    if(nrow(xref) > 1 & !multiple) stop("No unique records found that match any patterns. Try being more specific.")
    
  }
  
  xref

  
}


#' Activate a record
#' 
#' Set a specific record to be the active record.
#'
#' @param gedcom A tidyged object. 
#' @param record The xref of the record to be activated.
#'
#' @return The same tidyged object with "active_record" attribute set to the specific 
#' record to allow easy editing.
#' @export
activate_indi <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_indi, is_indi)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_famg <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_famg, is_famg)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_subm <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_subm, is_subm)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_media <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_obje, is_media)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_note <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_note, is_note)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_sour <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_sour, is_sour)
  set_active_record(gedcom, xref)
}


#' @rdname activate_indi
#' @export
activate_repo <- function(gedcom, record) {
  
  xref <- get_valid_xref(gedcom, record, .pkgenv$record_string_repo, is_repo)
  set_active_record(gedcom, xref)
  
}
