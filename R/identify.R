

#' Get all spouses for an individual
#'
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param return_name Whether to return the spouse's name(s) instead of the xref(s).
#'
#' @return A character vector of spouse xrefs or names.
#' @export
#' @tests
#' expect_equal(get_spouses(sample555, "@I1@"), "@I2@")
#' expect_equal(get_spouses(sample555, "@I2@", TRUE), "Robert Eugene Williams")
#' expect_equal(get_spouses(sample555, "@I3@"), character(0))
get_spouses <- function(gedcom,
                        individual = character(),
                        return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  fams_xref <- get_families_as_spouse(gedcom, xref)
  
  spou_xref <- dplyr::filter(gedcom, level == 1, record %in% fams_xref, tag %in% c("HUSB","WIFE"),
                              value != xref)$value
    
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
#' @tests
#' expect_error(get_children(sample555, "@I4@"))
#' expect_equal(get_children(sample555, "@I1@"), "@I3@")
#' expect_equal(get_children(sample555, "@I2@", TRUE), "Joe Williams")
get_children <- function(gedcom,
                         individual = character(),
                         return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  fams_xref <- get_families_as_spouse(gedcom, xref)
  
  chil_xref <- unique(dplyr::filter(gedcom, level == 1, record %in% fams_xref, tag == "CHIL")$value)
  
  if (return_name) {
    purrr::map_chr(chil_xref, describe_indi, gedcom=gedcom, name_only = TRUE)
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
#' @tests
#' expect_equal(get_parents(sample555, "@I3@"), c("@I1@", "@I2@"))
#' expect_equal(get_parents(sample555, "@I3@", TRUE), c("Robert Eugene Williams", "Mary Ann Wilson"))
get_parents <- function(gedcom,
                        individual = character(),
                        return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  famc_xref <- get_families_as_child(gedcom, xref)
  
  par_xref <- unique(dplyr::filter(gedcom, level == 1, record %in% famc_xref, tag %in% c("HUSB","WIFE"))$value)
  
  if (return_name) {
    purrr::map_chr(par_xref, describe_indi, gedcom=gedcom, name_only = TRUE)
  } else {
    par_xref
  }
  
}

#' Get all families for an individual where they are a spouse
#'
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#'
#' @return A character vector of family xrefs.
#' @export
#' @tests
#' expect_equal(get_families_as_spouse(sample555, "@I1@"), c("@F1@", "@F2@"))
#' expect_equal(get_families_as_spouse(sample555, "@I2@"), "@F1@")
get_families_as_spouse <- function(gedcom, individual = character()) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  unique(dplyr::filter(gedcom, level == 1, tag %in% c("HUSB", "WIFE"), value == xref)$record) 
  
}

#' Get all families for an individual where they are a child
#'
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#'
#' @return A character vector of family xrefs.
#' @export
#' @tests
#' expect_equal(get_families_as_child(sample555, "@I3@"), c("@F1@", "@F2@"))
get_families_as_child <- function(gedcom, individual = character()) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  unique(dplyr::filter(gedcom, level == 1, tag == "CHIL", value == xref)$record)
  
}


#' Get all supporting records for a set of records
#' 
#' This function gets all supporting records (and onwards dependencies) for a set of records. Supporting records
#' are note, multimedia, source, and repository records, i.e. those providing supporting evidence and comments.
#'
#' @param gedcom A tidyged object.
#' @param xrefs The xrefs of records to get supporting records for.
#' @param include_note Whether to include Note records.
#' @param include_media Whether to include Multimedia records.
#' @param include_sour Whether to include Source records.
#' @param include_repo Whether to include Repository records.
#'
#' @return A character vector of supporting record xrefs.
#' @export
#' @tests
#' expect_equal(get_supporting_records(sample555, "@I1@"), c("@S1@", "@R1@"))
get_supporting_records <- function(gedcom,
                                   xrefs,
                                   include_note = TRUE,
                                   include_media = TRUE,
                                   include_sour = TRUE,
                                   include_repo = TRUE) {
  
  if (length(xrefs) == 0) return(NULL)
  
  tags <- NULL
  if (include_note) tags <- c(tags, "NOTE")
  if (include_media) tags <- c(tags, "OBJE")
  if (include_sour) tags <- c(tags, "SOUR")
  if (include_repo) tags <- c(tags, "REPO")
  
  links <- unique(dplyr::filter(gedcom, 
                                record %in% xrefs, 
                                tag %in% tags, 
                                stringr::str_detect(value, tidyged.internals::xref_pattern()))$value)
  
  unique(
    c(links,
    get_supporting_records(gedcom, links, include_note, include_media, include_sour, include_repo))
  )
  
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
#' @tests
#' expect_equal(get_descendants(sample555, "Robert"), "@I3@")
#' expect_equal(get_descendants(sample555, "Robert", TRUE), c("@I1@","@I3@"))
#' expect_equal(get_descendants(sample555, "Robert", TRUE, TRUE), c("@I2@","@I1@","@I3@"))
#' expect_equal(get_descendants(sample555, "Robert", TRUE, TRUE, TRUE), c("@F1@","@F2@","@I2@","@I1@","@I3@"))
get_descendants <- function(gedcom,
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
    spou_chil <- unlist(purrr::map(spou_xref, get_children, gedcom=gedcom))

    chil_xref <- unique(c(chil_xref, spou_chil))
  }
  
  #deal with family groups first (while the individuals are still in them)
  if (include_families) return_xrefs <- c(return_xrefs, fams_xref)
  if (include_spouses) return_xrefs <- c(return_xrefs, spou_xref)
  if (include_individual) return_xrefs <- c(return_xrefs, xref)
  
  # identify children
  for(i in seq_along(chil_xref)) {
    return_xrefs <- c(return_xrefs,
                      get_descendants(gedcom, chil_xref[i], TRUE, TRUE,TRUE))
  }
  
  return_xrefs
}


get_ancestors <- function(gedcom,
                               individual = character(),
                               include_individual = TRUE,
                               include_siblings = FALSE,
                               include_families = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  
  
  
}



xrefs_record_type <- function(gedcom, record_tag) {
  dplyr::filter(gedcom, level == 0 & tag == record_tag)$record
}

#' Get the xrefs of particular record types
#'
#' These functions return the xrefs of all records of a particular type in a tidyged object.
#'
#' @param gedcom A tidyged object.
#'
#' @return A vector of xrefs of records of the relevant type.
#' @export
#' @tests
#' expect_equal(xrefs_indi(sample555), paste0("@I", 1:3, "@"))
xrefs_indi <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_indi) }

#' @export
#' @rdname xrefs_indi
#' @tests
#' expect_equal(xrefs_famg(sample555), paste0("@F", 1:2, "@"))
xrefs_famg <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_famg) }

#' @export
#' @rdname xrefs_indi
#' @tests
#' expect_equal(xrefs_subm(sample555), paste0("@U", 1, "@"))
xrefs_subm <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_subm) }

#' @export
#' @rdname xrefs_indi
#' @tests
#' expect_equal(xrefs_sour(sample555), paste0("@S", 1, "@"))
xrefs_sour <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_sour) }

#' @export
#' @rdname xrefs_indi
xrefs_repo <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_repo) }

#' @export
#' @rdname xrefs_indi
xrefs_note <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_note) }

#' @export
#' @rdname xrefs_indi
xrefs_media <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_obje) }


