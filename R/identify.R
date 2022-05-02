
#' Determine whether an individual is a child of a family by birth
#'
#' @param gedcom A tidyged object.
#' @param indi_xref The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param famg_xref The xref of the Family Group record.
#'
#' @return A logical value indicating whether the individual is a child of a family by birth.
#' @tests
#' expect_equal(is_famg_birth_child(sample555, "@I3@", "@F1@"), TRUE)
#' expect_equal(is_famg_birth_child(sample555, "@I3@", "@F2@"), FALSE)
#' expect_equal(is_famg_birth_child(sample555, "@I1@", "@F1@"), FALSE)
is_famg_birth_child <- function(gedcom,
                                indi_xref,
                                famg_xref){
  
  xref <- get_valid_xref(gedcom, indi_xref, .pkgenv$record_string_indi, is_indi)
  
  chil_rows <- gedcom[gedcom$record == famg_xref & gedcom$level == 1 &
                      gedcom$tag == "CHIL" & gedcom$value == xref,]
  is_child <- nrow(chil_rows) >= 1
  
  if(!is_child) return(FALSE)
  
  # look in birth event
  famc_evt <- tidyged.internals::gedcom_value(gedcom, xref, "FAMC", 2, "BIRT")
  if(famc_evt == famg_xref) return(TRUE)
  
  # get child to family link
  rows <- tidyged.internals::identify_section(gedcom, 
                                              containing_level = 1, 
                                              containing_tags = "FAMC", 
                                              containing_values = famg_xref,
                                              xrefs = xref,
                                              first_only = TRUE)
  
  pedi <- tolower(tidyged.internals::gedcom_value(gedcom[rows,], 
                                                  record_xref = xref, 
                                                  tag = "PEDI", 
                                                  level = 2, 
                                                  after_tag = "FAMC"))
  
  if(pedi == "birth" | pedi == "")  return(TRUE)

  FALSE
}

#' Identify all families for an individual where they are a partner
#' 
#' @param gedcom A tidyged object.
#' @param indi_xref The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#'
#' @return A character vector of family xrefs.
#' @export
#' @examples 
#' get_families_as_partner(sample555, "@I2@")
#' @tests
#' expect_equal(get_families_as_partner(sample555, "@I1@"), c("@F1@", "@F2@"))
#' expect_equal(get_families_as_partner(sample555, "@I2@"), "@F1@")
#' expect_equal(get_families_as_partner(sample555, "@I3@"), character())
get_families_as_partner <- function(gedcom, 
                                   indi_xref = character()) {
  
  xref <- get_valid_xref(gedcom, indi_xref, .pkgenv$record_string_indi, is_indi)
  
  unique(dplyr::filter(gedcom, record == xref, level == 1, tag == "FAMS")$value) 
  
}

#' Identify all families for an individual where they are a child
#'
#' @param gedcom A tidyged object.
#' @param indi_xref The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param birth_only Whether to only return the family containing the biological parents.
#'
#' @return A character vector of family xrefs.
#' @examples 
#' get_families_as_child(sample555, "@I3@")
#' @export
#' @tests
#' expect_equal(get_families_as_child(sample555, "@I3@"), c("@F1@", "@F2@"))
#' expect_equal(get_families_as_child(sample555, "@I1@"), character())
get_families_as_child <- function(gedcom,
                                  indi_xref = character(),
                                  birth_only = FALSE) {
  
  xref <- get_valid_xref(gedcom, indi_xref, .pkgenv$record_string_indi, is_indi)
  
  # return all family links
  famc <- unique(dplyr::filter(gedcom, record == xref, tag == "FAMC")$value)
  
  if(length(famc) == 0) return(famc)
  if(!birth_only) return(famc)
  
  by_birth <- purrr::map_lgl(famc, is_famg_birth_child,
                             gedcom = gedcom,
                             indi_xref = xref)
  
  famc <- famc[by_birth]
  if(length(famc) == 0) return(famc)
  if(length(famc) > 1) warning("The individual has more than one birth family")
  
  famc[1]
}



#' Identify all partners for an individual
#'
#' @param gedcom A tidyged object.
#' @param indi_xref The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param return_name Whether to return the partner's name(s) instead of the xref(s).
#'
#' @return A character vector of partner xrefs or names.
#' @export
#' @examples
#' get_indi_partners(sample555, "@I1@")
#' get_indi_partners(sample555, "@I1@", return_name = TRUE)
#' get_indi_partners(sample555, "@I3@")
#' @tests
#' expect_equal(get_indi_partners(sample555, "@I1@"), "@I2@")
#' expect_equal(get_indi_partners(sample555, "@I2@", TRUE), "Robert Eugene Williams")
#' expect_equal(get_indi_partners(sample555, "@I3@"), character())
get_indi_partners <- function(gedcom,
                              indi_xref = character(),
                              return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, indi_xref, .pkgenv$record_string_indi, is_indi)
  
  fams_xref <- get_families_as_partner(gedcom, xref)
  
  part_xref <- purrr::map(fams_xref, get_famg_partners, gedcom=gedcom) |> 
    purrr::flatten_chr() |>
    unique()
  
  part_xref <- part_xref[part_xref != xref]
  
  if (return_name) {
    purrr::map_chr(part_xref, describe_indi, gedcom=gedcom, name_only = TRUE)
  } else {
    part_xref
  }
}


#' Identify all children for an individual
#'
#' @param gedcom A tidyged object.
#' @param indi_xref The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param birth_only Whether to only return biological children.
#' @param return_name Whether to return the childrens name(s) instead of the xref(s).
#'
#' @return A character vector of children xrefs or names.
#' @export
#' @examples 
#' get_indi_children(sample555, "@I2@")
#' get_indi_children(sample555, "@I2@", return_name = TRUE)
#' @tests
#' expect_error(get_indi_children(sample555, "@I4@"))
#' expect_equal(get_indi_children(sample555, "@I1@"), "@I3@")
#' expect_equal(get_indi_children(sample555, "@I2@", FALSE, TRUE), "Joe Williams")
#' expect_equal(get_indi_children(sample555, "@I3@"), character())
get_indi_children <- function(gedcom,
                              indi_xref = character(),
                              birth_only = FALSE,
                              return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, indi_xref, .pkgenv$record_string_indi, is_indi)
  
  fams_xref <- get_families_as_partner(gedcom, xref)
  
  chil_xref <- purrr::map(fams_xref, get_famg_children, gedcom=gedcom,
                          birth_only=birth_only, return_name=FALSE) |> 
    purrr::flatten_chr() |> 
    unique()
  
  if (return_name) {
    purrr::map_chr(chil_xref, describe_indi, gedcom=gedcom, name_only = TRUE)
  } else {
    chil_xref
  }
  
}

#' Identify all parents for an individual
#'
#' @param gedcom A tidyged object.
#' @param indi_xref The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param birth_only Whether to only return biological parents.
#' @param return_name Whether to return the parents name(s) instead of the xref(s).
#'
#' @return A character vector of parent xrefs or names.
#' @export
#' @examples
#' get_indi_parents(sample555, "@I2@")
#' get_indi_parents(sample555, "@I3@")
#' get_indi_parents(sample555, "@I3@", return_name = TRUE)
#' @tests
#' expect_equal(get_indi_parents(sample555, "@I3@"), c("@I1@", "@I2@"))
#' expect_equal(get_indi_parents(sample555, "@I3@", FALSE, TRUE), c("Robert Eugene Williams", "Mary Ann Wilson"))
#' expect_equal(get_indi_parents(sample555, "@I1@"), character())
get_indi_parents <- function(gedcom,
                             indi_xref = character(),
                             birth_only = FALSE,
                             return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, indi_xref, .pkgenv$record_string_indi, is_indi)
  
  famc_xref <- get_families_as_child(gedcom, xref, birth_only)
  
  par_xref <- purrr::map(famc_xref, get_famg_partners, gedcom=gedcom,
                          return_name=FALSE) |> 
    purrr::flatten_chr() |> 
    unique()

  if (return_name) {
    purrr::map_chr(par_xref, describe_indi, gedcom=gedcom, name_only = TRUE)
  } else {
    par_xref
  }
  
}


#' Identify all siblings for an individual
#'
#' @param gedcom A tidyged object.
#' @param indi_xref The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param birth_only Whether to only return biological siblings.
#' @param inc_half_sibs Whether to include siblings that only share one parent.
#' @param return_name Whether to return the parents name(s) instead of the xref(s).
#'
#' @return A character vector of sibling xrefs or names.
#' @export
#' @tests
#' expect_equal(get_indi_siblings(sample555, "@I3@"), character())
get_indi_siblings <- function(gedcom,
                              indi_xref = character(),
                              birth_only = FALSE,
                              inc_half_sibs = FALSE,
                              return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, indi_xref, .pkgenv$record_string_indi, is_indi)
  
  if (inc_half_sibs) {
    par_xref <- get_indi_parents(gedcom, xref, birth_only)
    
    sib_xref <- purrr::map(par_xref, get_indi_children, gedcom = gedcom, 
                           birth_only = birth_only)
  } else {
    famc_xref <- get_families_as_child(gedcom, xref, birth_only)
    
    sib_xref <- purrr::map(famc_xref, get_famg_children, gedcom=gedcom,
                            birth_only=birth_only, return_name=FALSE)
  }
  
  sib_xref <- sib_xref |> 
    purrr::flatten_chr() |> 
    unique()
  
  sib_xref <- sib_xref[sib_xref != xref]
  
  if (return_name) {
    purrr::map_chr(sib_xref, describe_indi, gedcom=gedcom, name_only = TRUE)
  } else {
    sib_xref
  }
  
}

#' Identify all cousins for an individual
#'
#' @param gedcom A tidyged object.
#' @param indi_xref The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param degree Whether to return first cousins (degree = 1), second cousins (degree = 2), etc.
#' @param inc_half_cous Whether to include half cousins.
#' @param return_name Whether to return the parents name(s) instead of the xref(s).
#'
#' @return A character vector of cousin xrefs or names.
#' @export
get_indi_cousins <- function(gedcom,
                             indi_xref = character(),
                             degree = 1,
                             inc_half_cous = FALSE,
                             return_name = FALSE){
  
  xref <- get_valid_xref(gedcom, indi_xref, .pkgenv$record_string_indi, is_indi)
  
  par_xref = xref
  for(i in seq_len(degree)){
    par_xref <- purrr::map(par_xref, get_indi_parents, gedcom=gedcom,
                           birth_only=TRUE) |> 
      purrr::flatten_chr()
  }
  
  sib_xref <- purrr::map(par_xref, get_indi_siblings, 
                         gedcom=gedcom, 
                         birth_only=TRUE,
                         inc_half_sibs=inc_half_cous) |> 
    purrr::flatten_chr()
  
  cou_xref <- sib_xref
  for(i in seq_len(degree)){
    cou_xref <- purrr::map(cou_xref, get_indi_children, gedcom=gedcom,
                           birth_only=TRUE) |> 
      purrr::flatten_chr()
  }
  
  if (return_name) {
    purrr::map_chr(cou_xref, describe_indi, gedcom=gedcom, name_only = TRUE)
  } else {
    cou_xref
  }
  
}

#' Identify all partners in a Family Group
#'
#' @param gedcom A tidyged object.
#' @param famg_xref The xref of a Family Group record to act on if one 
#' is not activated (will override active record).
#' @param return_name Whether to return the partners name(s) instead of the xref(s).
#'
#' @return A character vector of partner xrefs or names.
#' @export
#' @tests
#' expect_equal(get_famg_partners(sample555, "@F1@"), c("@I1@", "@I2@"))
get_famg_partners <- function(gedcom,
                              famg_xref,
                              return_name = FALSE){
  
  xref <- get_valid_xref(gedcom, famg_xref, .pkgenv$record_string_famg, is_famg)
  
  part_xref <- unique(dplyr::filter(gedcom, level == 1, record == xref, tag %in% c("HUSB","WIFE"))$value)
  
  if (return_name) {
    purrr::map_chr(part_xref, describe_indi, gedcom=gedcom, name_only = TRUE)
  } else {
    part_xref
  }
}

#' Identify all children in a Family Group
#'
#' @param gedcom A tidyged object.
#' @param famg_xref The xref of a Family Group record to act on if one 
#' is not activated (will override active record).
#' @param birth_only Whether to only return biological children.
#' @param return_name Whether to return the child name(s) instead of the xref(s).
#'
#' @return A character vector of partner xrefs or names.
#' @export
#' @tests
#' expect_equal(get_famg_children(sample555, "@F1@"), "@I3@")
#' expect_equal(get_famg_children(sample555, "@F2@"), "@I3@")
#' expect_equal(get_famg_children(sample555, "@F2@", birth_only = TRUE), character())
#' expect_equal(get_famg_children(sample555, "@F2@", return_name = TRUE), "Joe Williams")
get_famg_children <- function(gedcom,
                              famg_xref,
                              birth_only = FALSE,
                              return_name = FALSE){
  
  xref <- get_valid_xref(gedcom, famg_xref, .pkgenv$record_string_famg, is_famg)
  
  chil_xref <- dplyr::filter(gedcom, level == 1, record == xref, tag == "CHIL")$value
  
  if(birth_only){
    
    by_birth <- purrr::map_lgl(chil_xref, is_famg_birth_child,
                               gedcom = gedcom,
                               famg_xref = xref)
    
    chil_xref <- chil_xref[by_birth]
  }
  
  if (return_name) {
    purrr::map_chr(chil_xref, describe_indi, gedcom=gedcom, name_only = TRUE)
  } else {
    chil_xref
  }
}

#' Identify all supporting records for a set of records
#' 
#' This function gets all supporting records (and onwards dependencies) for a set of records. Supporting records
#' are note, multimedia, source, and repository records, i.e. those providing supporting evidence and comments.
#'
#' @param gedcom A tidyged object.
#' @param xrefs The xrefs of records to get supporting records for.
#' @param inc_note Whether to include Note records.
#' @param inc_media Whether to include Multimedia records.
#' @param inc_sour Whether to include Source records.
#' @param inc_repo Whether to include Repository records.
#'
#' @return A character vector of supporting record xrefs.
#' @export
#' @examples 
#' get_supporting_records(sample555, "@I1@")
#' get_supporting_records(sample555, "@F1@")
#' @tests
#' expect_equal(get_supporting_records(sample555, "@I1@"), c("@S1@", "@R1@"))
get_supporting_records <- function(gedcom,
                                   xrefs,
                                   inc_note = TRUE,
                                   inc_media = TRUE,
                                   inc_sour = TRUE,
                                   inc_repo = TRUE) {
  
  if (length(xrefs) == 0) return(character())
  
  tags <- NULL
  if (inc_note) tags <- c(tags, "NOTE")
  if (inc_media) tags <- c(tags, "OBJE")
  if (inc_sour) tags <- c(tags, "SOUR")
  if (inc_repo) tags <- c(tags, "REPO")
  
  links <- unique(dplyr::filter(gedcom, 
                                record %in% xrefs, 
                                tag %in% tags, 
                                stringr::str_detect(value, tidyged.internals::reg_xref(TRUE)))$value)
  
  unique(
    c(links,
    get_supporting_records(gedcom, links, inc_note, inc_media, inc_sour, inc_repo))
  )
  
}


#' Identify all descendants for an individual
#' 
#' This function identifies records in an entire branch of the family tree below a certain individual.
#' 
#' @param gedcom A tidyged object.
#' @param indi_xref The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param inc_indi Whether to also include the individual themselves.
#' @param inc_part Whether to also include all partners of this individual (and their descendants and
#' descendants' partners).
#' @param inc_famg Whether to also include all Family Group records where this individual is a partner 
#' (and all descendants' Family Group records).
#' @param inc_supp Whether to also include all supporting records (Note, Source, Repository, Multimedia).
#' @param birth_only Whether to only include biological descendants.
#'
#' @return A vector of xrefs of descendants.
#' @export
#' @tests
#' expect_equal(get_descendants(sample555, "@I3@"), character())
#' expect_equal(get_descendants(sample555, "@I1@"), "@I3@")
#' expect_equal(get_descendants(sample555, "@I1@", TRUE), c("@I1@","@I3@"))
#' expect_equal(get_descendants(sample555, "@I1@", TRUE, TRUE), c("@I2@","@I1@","@I3@"))
#' expect_equal(get_descendants(sample555, "@I1@", TRUE, TRUE, TRUE), c("@F1@","@F2@","@I2@","@I1@","@I3@"))
get_descendants <- function(gedcom,
                            indi_xref = character(),
                            inc_indi = FALSE,
                            inc_part = FALSE,
                            inc_famg = FALSE,
                            inc_supp = FALSE,
                            birth_only = TRUE) {
  
  xref <- get_valid_xref(gedcom, indi_xref, .pkgenv$record_string_indi, is_indi)
  
  return_xrefs <- character()
  
  part_xref <- get_indi_partners(gedcom, xref)
  chil_xref <- get_indi_children(gedcom, xref, birth_only = birth_only)
  fams_xref <- get_families_as_partner(gedcom, xref)
  
  # if partner is to be included, add their children to be included
  if (inc_part) {
    part_chil <- unlist(purrr::map(part_xref, get_indi_children, gedcom=gedcom,
                                   birth_only = birth_only))

    chil_xref <- unique(c(chil_xref, part_chil))
  }
  
  #deal with family groups first (while the individuals are still in them)
  if (inc_famg) return_xrefs <- c(return_xrefs, fams_xref)
  if (inc_part) return_xrefs <- c(return_xrefs, part_xref)
  if (inc_indi) return_xrefs <- c(return_xrefs, xref)
  
  # identify children
  for(i in seq_along(chil_xref)) {
    return_xrefs <- c(return_xrefs,
                      get_descendants(gedcom, chil_xref[i], TRUE, inc_part, inc_famg, FALSE, birth_only))
  }
  
  # only get supporting records if this is the top level call
  if (inc_supp && length(as.character(sys.call())) == 8 && 
      any(as.character(sys.call()) != c("get_descendants","gedcom","chil_xref[i]",
                                        "TRUE","inc_part","inc_famg","FALSE","birth_only"))){
    
    c(return_xrefs,
      get_supporting_records(gedcom, return_xrefs))
  } else {
    return_xrefs
  }
  
}


#' Identify all ancestors for an individual
#' 
#' This function identifies records in an entire branch of the family tree above a certain individual.
#' 
#' @param gedcom A tidyged object.
#' @param indi_xref The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param inc_indi Whether to also include the individual themselves.
#' @param inc_sibs Whether to also include all siblings of ancestors (siblings of this individual will only be
#' included if the individual is included).
#' @param inc_famg Whether to also include all Family Group records where this individual is a child 
#' (and all ancestors' Family Group records).
#' @param inc_supp Whether to also include all supporting records (Note, Source, Repository, Multimedia).
#' @param birth_only Whether to only include biological ancestors.
#'
#' @return A vector of xrefs of ancestors.
#' @examples 
#' get_ancestors(sample555, "@I3@")
#' get_ancestors(sample555, "@I3@", inc_indi = TRUE)
#' get_ancestors(sample555, "@I3@", inc_indi = TRUE, inc_famg = TRUE)
#' @tests
#' expect_equal(get_ancestors(sample555, "@I1@"), character())
#' @export
get_ancestors <- function(gedcom,
                          indi_xref = character(),
                          inc_indi = FALSE,
                          inc_sibs = FALSE,
                          inc_famg = FALSE,
                          inc_supp = FALSE,
                          birth_only = TRUE) {
  
  xref <- get_valid_xref(gedcom, indi_xref, .pkgenv$record_string_indi, is_indi)
  
  return_xrefs <- character()

  sib_xref <- get_indi_siblings(gedcom, xref, birth_only = birth_only)
  par_xref <- get_indi_parents(gedcom, xref, birth_only = birth_only)
  famc_xref <- get_families_as_child(gedcom, xref, birth_only = birth_only)
  
  if (inc_indi & inc_sibs) {
    sib_par <- unlist(purrr::map(sib_xref, get_indi_parents, gedcom=gedcom,
                                 birth_only=birth_only))
    
    par_xref <- unique(c(par_xref, sib_par))
  }
  
  if (inc_famg) return_xrefs <- c(return_xrefs, famc_xref)
  if (inc_indi & inc_sibs) return_xrefs <- c(return_xrefs, sib_xref)
  if (inc_indi) return_xrefs <- c(return_xrefs, xref)
  
  for(i in seq_along(par_xref)) {
    return_xrefs <- c(return_xrefs,
                      get_ancestors(gedcom, par_xref[i], TRUE, inc_sibs, inc_famg, FALSE, birth_only))
  }
  
  # only get supporting records if this is the top level call
  if (inc_supp && length(as.character(sys.call())) == 8 && 
      any(as.character(sys.call()) != c("get_ancestors","gedcom","par_xref[i]",
                                        "TRUE","inc_sibs","inc_famg","FALSE"))) {
    
    c(return_xrefs,
      get_supporting_records(gedcom, return_xrefs))
  } else {
    return_xrefs
  }
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
#' @examples 
#' xrefs_indi(sample555)
#' xrefs_famg(sample555)
#' xrefs_note(sample555)
#' xrefs_repo(sample555)
#' xrefs_sour(sample555)
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


