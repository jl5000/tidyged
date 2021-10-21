

#' Add an association with another individual
#'
#' @param gedcom A tidyged object.
#' @param associated_with An xref identifying the associated individual.
#' @param association A word or phrase stating the nature of the association.
#' @param association_notes A character vector of notes accompanying this association.
#' These could be xrefs to existing Note records.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param update_date_changed Whether to add/update the change date for the record.
#'
#' @return An updated tidyged object with an expanded Individual record including
#' this association.
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'                         add_indi(qn = "Joe Bloggs") %>% 
#'                         add_indi(qn = "Jimmy Bloggs") %>% 
#'                         add_indi_association(associated_with = "@I1@", association = "Friend") %>% 
#'                         remove_dates_for_tests(), "json2")
add_indi_association <- function(gedcom,
                                 associated_with,
                                 association,
                                 association_notes = character(),
                                 xref = character(),
                                 update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  indi_xref <- get_valid_xref(gedcom, associated_with, .pkgenv$record_string_indi, is_indi)
  
  asso_notes <- create_note_structures(gedcom, association_notes)
  
  asso_str <- tidyged.internals::ASSOCIATION_STRUCTURE(xref_indi = indi_xref,
                                                       relation_is_descriptor = association,
                                                       notes = asso_notes) %>% 
    tidyged.internals::add_levels(1)
  
  next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, "INDI")
  
  gedcom <- tibble::add_row(gedcom, asso_str, .before = next_row)
  
  if(update_date_changed) gedcom <- update_change_date(gedcom, xref)
  
  activate_indi(gedcom, xref)
  
}

#' Add a family link as a spouse
#' 
#' @details These are only to be used by the add_famg function. The spouse is added separately.
#'
#' @param gedcom A tidyged object.
#' @param family_xref The xref of the family associated of which this individual is a spouse.
#' @param linkage_notes A character vector of notes accompanying this linkage.
#' These could be xrefs to existing Note records.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param update_date_changed Whether to add/update the change date for the record.
#'
#' @return An updated tidyged object with an expanded Individual record including
#' this family link.
add_indi_family_link_as_spouse <- function(gedcom, 
                                           family_xref,
                                           linkage_notes = character(),
                                           xref = character(),
                                           update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  link_notes <- create_note_structures(gedcom, linkage_notes)
  
  link <- tidyged.internals::SPOUSE_TO_FAMILY_LINK(xref_fam = family_xref, notes = link_notes) %>% 
    tidyged.internals::add_levels(1)
  
  next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, "INDI")
  
  gedcom <- tibble::add_row(gedcom, link, .before = next_row)
  
  if(update_date_changed) gedcom <- update_change_date(gedcom, xref)
  
  activate_indi(gedcom, xref)
  
}

#' Add a family link as a child
#' 
#' @details These are only to be used by the add_famg function. The child is added separately.
#'
#' @param gedcom A tidyged object.
#' @param family_xref The xref of the family associated of which this individual is a child.
#' @param linkage_type A code used to indicate the child to family relationship. Either, 
#' "birth" (default), "foster", or "adopted".
#' @param linkage_notes A character vector of notes accompanying this linkage.
#' These could be xrefs to existing Note records.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param update_date_changed Whether to add/update the change date for the record.
#'
#' @return An updated tidyged object with an expanded Individual record including
#' this family link.
add_indi_family_link_as_child <- function(gedcom, 
                                          family_xref,
                                          linkage_type = "birth",
                                          linkage_notes = character(),
                                          xref = character(),
                                          update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  link_notes <- create_note_structures(gedcom, linkage_notes)
  
  link <- tidyged.internals::CHILD_TO_FAMILY_LINK(xref_fam = family_xref,
                                                  pedigree_linkage_type = linkage_type,
                                                  notes = link_notes) %>% 
    tidyged.internals::add_levels(1)
  
  next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, "INDI")
  
  gedcom <- tibble::add_row(gedcom, link, .before = next_row)
  
  if(update_date_changed) gedcom <- update_change_date(gedcom, xref)
  
  activate_indi(gedcom, xref)
}


#' Add family links as a child or spouse
#' 
#' This function adds links connecting an Individual record to existing Family Group records. Family 
#' links will be added to the Individual record, and the Family Group records will be updated to link
#' to this individual.
#' 
#' @details The function will only add one link to a Family Group record as a child, and one link
#' to a Family Group record as a spouse.
#'
#' @param gedcom A tidyged object.
#' @param parents A character vector of parent xrefs so that this person can be linked to a Family Group
#' record as a child. 
#' @param child_linkage_type A code used to indicate the relationship with the parent(s). Either, 
#' "birth" (default), "foster", or "adopted".
#' @param child_linkage_notes A character vector of notes accompanying the family linkage as a child.
#' These could be xrefs to existing Note records.
#' @param spouse The xref of the spouse so that this person can be linked to 
#' a Family Group record as a spouse.
#' @param children A character vector of children xrefs so that this person can be linked to a Family Group
#' record as a spouse.
#' @param spouse_linkage_notes A character vector of notes accompanying the family linkage as a spouse
#' These could be xrefs to existing Note records.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param famg_xref_chil The xref of a Family Group record to be added to as a child. This is not required
#' if any parents are given, but if it is provided, it will override any parents given.
#' @param famg_xref_spou The xref of a Family Group record to be added to as a spouse. This is not required
#' if the spouse or children are given, but if it is provided, it will override any spouses or children given.
#' @param update_date_changed Whether to add/update the change date for the records.
#'
#' @return An updated tidyged object with an expanded Individual record including
#' the family link(s) and expanded Family Group record(s) linking to this individual.
#' @export
add_indi_links_to_families <- function(gedcom,
                                       parents = character(),
                                       child_linkage_type = "birth",
                                       child_linkage_notes = character(),
                                       spouse = character(),
                                       children = character(),
                                       spouse_linkage_notes = character(),
                                       xref = character(),
                                       famg_xref_chil = character(),
                                       famg_xref_spou = character(),
                                       update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  if(length(spouse) > 1) stop("Only one spouse should be provided")
  
  # Get famg_xref_chil
  if(length(famg_xref_chil) == 1){
    famg_xref_chil <- get_valid_xref(gedcom, famg_xref_chil, .pkgenv$record_string_famg, is_famg)
  } else if(length(parents) > 0) {
    
    parents <- purrr::map_chr(parents, get_valid_xref, 
                              gedcom = gedcom,
                              record_type = .pkgenv$record_string_indi, 
                              record_type_fn = is_indi)
    
    famg_xref_chil <- dplyr::filter(gedcom, level == 1, tag %in% c("HUSB","WIFE"), value %in% parents) %>% 
      dplyr::count(record) %>% 
      dplyr::filter(n == max(n)) %>% 
      dplyr::pull(record)
    
    if(length(famg_xref_chil) == 0) 
      stop("An existing family could not be found with this parent.
            This function is only to be used to add individuals to an existing Family Group record.")
    
    if(length(famg_xref_chil) > 1) 
      stop("More than one Family Group record was found with this parent.
            Try specifying an additional parent or provide the Family Group record xref directly (with the famg_xref_chil parameter).")
    
  }
  
  # Use famg_xref_chil to add to records
  if(length(famg_xref_chil) == 1){
    
    # add link to indi record
    gedcom <- add_indi_family_link_as_child(gedcom, famg_xref_chil, child_linkage_type, child_linkage_notes,
                                            xref, update_date_changed)
    
    # add child to family record
    next_row <- tidyged.internals::find_insertion_point(gedcom, famg_xref_chil, 0, "FAM")
    gedcom <- tibble::add_row(gedcom,
                              tibble::tibble(record = famg_xref_chil, level = 1, tag = "CHIL", value = xref), 
                              .before = next_row) %>% 
      order_famg_children(famg_xref_chil)
    
    if(update_date_changed) gedcom <- update_change_date(gedcom, famg_xref_chil)
    
  }
  
  
  # Get xref_fams
  if(length(famg_xref_spou) == 1){
    famg_xref_spou <- get_valid_xref(gedcom, famg_xref_spou, .pkgenv$record_string_famg, is_famg)
  } else if(length(children) > 0 | length(spouse) > 0){
    
    xref_fams1 <- NULL
    xref_fams2 <- NULL
    
    if(length(spouse) > 0) {
      spouse <- get_valid_xref(gedcom, spouse, .pkgenv$record_string_indi, is_indi)
      
      xref_fams1 <- dplyr::filter(gedcom, level == 1, tag %in% c("HUSB","WIFE"), value == spouse) %>% 
        dplyr::count(record) %>% 
        dplyr::filter(n == max(n)) %>% 
        dplyr::pull(record)
      
      # remove those which have 2 spouses already
      xref_fams1 <- dplyr::filter(gedcom, record %in% xref_fams1, level == 1, tag %in% c("HUSB","WIFE")) %>% 
        dplyr::count(record) %>% 
        dplyr::filter(n < 2) %>% 
        dplyr::pull(record)
    }
    
    if(length(children) > 0) {
      children <- purrr::map_chr(children, get_valid_xref, 
                                 gedcom = gedcom,
                                 record_type = .pkgenv$record_string_indi, 
                                 record_type_fn = is_indi)
      
      xref_fams2 <- dplyr::filter(gedcom, level == 1, tag == "CHIL", value %in% children) %>% 
        dplyr::count(record) %>% 
        dplyr::filter(n == max(n)) %>% 
        dplyr::pull(record)
    }
    
    famg_xref_spou <- unique(c(xref_fams1, xref_fams2))
    
    if(length(famg_xref_spou) == 0) stop("An existing family could not be found with this spouse/children.
                                    This function is only to be used to add individuals to an existing Family Group record.")
    
    if(length(famg_xref_spou) > 1) 
      stop("More than one Family Group record was found with this spouse/children.
            Try specifying additional children or provide the Family Group record xref directly (with the famg_xref_spou parameter).")
  }
  
  # Use famg_xref_spou to add to records
  if(length(famg_xref_spou) == 1){
    
    # add link to indi record
    gedcom <- add_indi_family_link_as_spouse(gedcom, famg_xref_spou, spouse_linkage_notes, xref, update_date_changed)
    
    # add spouse to family record
    next_row <- tidyged.internals::find_insertion_point(gedcom, famg_xref_spou, 0, "FAM")
    # determine whether to use HUSB or WIFE tag - look at sex first, then look at other tag
    sex <- tidyged.internals::gedcom_value(gedcom, xref, "SEX", 1)
    if(!sex %in% c("M", "F")) {
      other_sex <- dplyr::filter(gedcom, record == famg_xref_spou, level == 1, tag %in% c("HUSB","WIFE"))$tag
      sex <- dplyr::if_else(length(other_sex) == 0 || other_sex == "WIFE", "M", "F")
    }
    gedcom <- tibble::add_row(gedcom,
                              tibble::tibble(record = famg_xref_spou, 
                                             level = 1, 
                                             tag = dplyr::if_else(sex == "M", "HUSB", "WIFE"), 
                                             value = xref), 
                              .before = next_row)
    
    if(update_date_changed) gedcom <- update_change_date(gedcom, famg_xref_spou)
  }
  
  gedcom
}

