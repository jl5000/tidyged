

#' Get all spouses for an individual
#'
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param return_name Whether to return the spouse's name(s) instead of the xref(s).
#'
#' @return A character vector of spouse xrefs or names.
#' @export
get_spouses <- function(gedcom,
                        individual = character(),
                        return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  fams_xref <- get_families_as_spouse(gedcom, xref)
  
  # we don't use purrr::map here because the return values could vary in length
  spou_xref <- NULL
  for(i in seq_along(fams_xref)){
    spou_xref <- gedcom %>% 
      dplyr::filter(record == fams_xref[i], tag %in% c("HUSB","WIFE"),
                    value != xref) %>% 
      dplyr::pull(value) %>% 
      c(spou_xref)
  }
  
  if (return_name) {
    purrr::map_chr(spou_xref, describe_indi, gedcom=gedcom, name_only = TRUE)
  } else {
    spou_xref
  }
}


#' Get all children for an individual
#'
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param return_name Whether to return the childrens name(s) instead of the xref(s).
#'
#' @return A character vector of children xrefs or names.
#' @export
get_children <- function(gedcom,
                         individual = character(),
                         return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  fams_xref <- get_families_as_spouse(gedcom, xref)
  
  chil_xref <- gedcom %>% 
    dplyr::filter(level == 1, record %in% fams_xref, tag == "CHIL") %>% 
    dplyr::pull(value) %>% 
    unique()
  
  if (return_name) {
    purrr::map_chr(chil_xref, describe_indi, gedcom=gedcom, name_only=TRUE)
  } else {
    chil_xref
  }
  
}

#' Get all parents for an individual
#'
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param return_name Whether to return the parents name(s) instead of the xref(s).
#'
#' @return A character vector of parent xrefs or names.
#' @export
get_parents <- function(gedcom,
                        individual = character(),
                        return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  famc_xref <- get_families_as_child(gedcom, xref)
  
  par_xref <- gedcom %>% 
    dplyr::filter(level == 1, record %in% famc_xref, tag %in% c("HUSB","WIFE")) %>% 
    dplyr::pull(value) %>% 
    unique()
  
  if (return_name) {
    purrr::map_chr(par_xref, describe_indi, gedcom=gedcom, name_only=TRUE)
  } else {
    par_xref
  }
  
}

#' Get all families for an individual where they are a spouse
#'
#' @param gedcom A tidygedcom object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#'
#' @return A character vector of family xrefs.
#' @export
get_families_as_spouse <- function(gedcom, individual = character()) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  gedcom %>% 
    dplyr::filter(level == 1, tag %in% c("HUSB", "WIFE"), value == xref) %>% 
    dplyr::pull(record) %>% 
    unique()
  
}

#' Get all families for an individual where they are a child
#'
#' @param gedcom A tidygedcom object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#'
#' @return A character vector of family xrefs.
#' @export
get_families_as_child <- function(gedcom, individual = character()) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  gedcom %>% 
    dplyr::filter(level == 1, tag == "CHIL", value == xref) %>% 
    dplyr::pull(record) %>% 
    unique()
  
}


#' Identify all descendants for an individual
#' 
#' This function identifies records in an entire branch of the family tree below a certain individual.
#' 
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param include_individual Whether to also include the individual themselves.
#' @param include_spouses Whether to also include all spouses of this individual (and their descendants).
#' @param include_families Whether to also include all Family Group records where this individual is a spouse.
#'
#' @return A vector of xrefs of descendants.
#' @export
identify_descendants <- function(gedcom,
                                 individual = character(),
                                 include_individual = FALSE,
                                 include_spouses = FALSE,
                                 include_families = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  return_xrefs <- NULL
  
  spou_xref <- get_spouses(gedcom, xref)
  chil_xref <- get_children(gedcom, xref)
  fams_xref <- get_families_as_spouse(gedcom, xref)
  
  # if spouse is to be included, add their children to be included
  if (include_spouses) {
    # we don't use purrr::map here because the return values could vary in length
    spou_chil <- NULL
    for(i in seq_along(spou_xref)) {
      spou_chil <- c(spou_chil, get_children(gedcom, spou_xref[i]))
    }
    chil_xref <- unique(c(chil_xref, spou_chil))
  }
  
  #deal with family groups first (while the individuals are still in them)
  if (include_families) return_xrefs <- c(return_xrefs, fams_xref)
  if (include_spouses) return_xrefs <- c(return_xrefs, spou_xref)
  if (include_individual) return_xrefs <- c(return_xrefs, xref)
  
  # identify children
  for(i in seq_along(chil_xref)) {
    return_xrefs <- c(return_xrefs,
                      identify_descendants(gedcom, chil_xref[i], TRUE, TRUE,TRUE))
  }
  
  return_xrefs
}


identify_ancestors <- function(gedcom,
                               individual = character(),
                               include_individual = TRUE,
                               include_siblings = FALSE,
                               include_families = FALSE) {
  
  
  
}



