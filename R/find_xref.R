

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
#' If you're not familiar with regular expressions, you may need to escape certain characters such as 
#' brackets and a full-stop/period (i.e. `\\.`).
#' 
#' @param gedcom A tidyged object.
#' @param search_patterns A named vector of terms to search for (see Details).
#' @param mode Whether to only return an xref if all patterns are matched ("strict"). A value of
#' "best" will return the xref with the most matches. If either of these still result in more than
#'  one xref it will return an error unless `multiple` is TRUE.
#' @param multiple If more than one xref is found (according to `mode`), whether to return all xrefs
#' or throw an error.
#' @param ignore_case Should case differences be ignored in the match?
#'
#' @return A vector of one or more xrefs.
#' @examples 
#' find_xref(sample555, c(INDI.BURI.PLAC = "Spring Hill"), multiple = FALSE)
#' find_xref(sample555, c(INDI.SEX = "M"), multiple = TRUE)
#' find_xref(sample555, c(FAM.MARR.DATE = "1859"), multiple = FALSE)
#' find_xref(sample555, c(REPO.ADDR.CITY = "Salt Lake"), multiple = TRUE)
#' find_xref(sample555, c(INDI.NAME.SURN = "Williams", INDI.ADOP.DATE = "Never"), 
#' mode = "best", multiple = TRUE)
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
  
  gedcom_ns <- mutate_tag_namespace(gedcom) |> 
    temporarily_remove_name_slashes()
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
    
    xref <- tibble::tibble(matches = unlist(matches)) |>
      dplyr::count(matches) |> 
      dplyr::filter(n==max(n)) |> 
      dplyr::pull(matches)
    
    if(length(xref) == 0) stop("No records found that match any patterns.")
    if(length(xref) > 1 & !multiple) stop("No unique records found that match any patterns. Try being more specific.")
    
  }
  
  xref
  
  
}


#' Helper functions to locate record xrefs
#' 
#' These functions act as wrappers to the `find_xref` function to find one or more record xrefs.
#' 
#' @details If you have your own specific use cases to identify records, it's easy to write your own wrapper.
#' It's best to name your function `find_recordtype_*` and end it with `_all` if it can return multiple xrefs.
#' If you provide more than one search pattern, you should also include the mode argument.
#'
#' @param gedcom A tidyged object.
#' @param pattern The search pattern to use (regular expression).
#' @param ignore_case Should case differences be ignored in the match?
#'
#' @return A character vector of xref(s).
#' @examples 
#' find_indi_name(sample555, "Mary")
#' find_indi_name_all(sample555, "Williams")
#' find_repo_name(sample555, "library", ignore_case = TRUE)
#' find_sour_titl(sample555, "Madison.+Records")
#' @export
find_indi_refn <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(INDI.REFN = pattern), multiple = FALSE, ignore_case = ignore_case)
}
#' @rdname find_indi_refn
#' @export
find_indi_name <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(INDI.NAME = pattern), multiple = FALSE, ignore_case = ignore_case)
}
#' @rdname find_indi_refn
#' @export
find_indi_name_all <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(INDI.NAME = pattern), multiple = TRUE, ignore_case = ignore_case)
}

#' @rdname find_indi_refn
#' @export
find_repo_refn <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(REPO.REFN = pattern), multiple = FALSE, ignore_case = ignore_case)
}
#' @rdname find_indi_refn
#' @export
find_repo_name <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(REPO.NAME = pattern), multiple = FALSE, ignore_case = ignore_case)
}
#' @rdname find_indi_refn
#' @export
find_repo_name_all <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(REPO.NAME = pattern), multiple = TRUE, ignore_case = ignore_case)
}

#' @rdname find_indi_refn
#' @export
find_note_refn <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(NOTE.REFN = pattern), multiple = FALSE, ignore_case = ignore_case)
}
#' @rdname find_indi_refn
#' @export
find_note_text <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(NOTE = pattern), multiple = FALSE, ignore_case = ignore_case)
}
#' @rdname find_indi_refn
#' @export
find_note_text_all <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(NOTE = pattern), multiple = TRUE, ignore_case = ignore_case)
}

#' @rdname find_indi_refn
#' @export
find_media_refn <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(OBJE.REFN = pattern), multiple = FALSE, ignore_case = ignore_case)
}
#' @rdname find_indi_refn
#' @export
find_media_fileref <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(OBJE.FILE = pattern), multiple = FALSE, ignore_case = ignore_case)
}
#' @rdname find_indi_refn
#' @export
find_media_fileref_all <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(OBJE.FILE = pattern), multiple = TRUE, ignore_case = ignore_case)
}

#' @rdname find_indi_refn
#' @export
find_sour_refn <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(SOUR.REFN = pattern), multiple = FALSE, ignore_case = ignore_case)
}
#' @rdname find_indi_refn
#' @export
find_sour_titl <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(SOUR.TITL = pattern), multiple = FALSE, ignore_case = ignore_case)
}
#' @rdname find_indi_refn
#' @export
find_sour_titl_all <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(SOUR.TITL = pattern), multiple = TRUE, ignore_case = ignore_case)
}

#' @rdname find_indi_refn
#' @export
find_famg_refn <- function(gedcom, pattern, ignore_case = FALSE) {
  find_xref(gedcom, search_patterns = c(FAM.REFN = pattern), multiple = FALSE, ignore_case = ignore_case)
}
