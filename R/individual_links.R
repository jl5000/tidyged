

#' Add an association with another individual
#'
#' @param gedcom A tidyged object.
#' @param associated_with A character string identifying the associated individual. This can either 
#' be an xref or term(s) to match to an individual name.
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
#'                         add_indi_association(associated_with = "Joe", association = "Friend") %>% 
#'                         tidyged.internals::remove_dates_for_tests(), "json2")
add_indi_association <- function(gedcom,
                                 associated_with,
                                 association,
                                 association_notes = character(),
                                 xref = character(),
                                 update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  indi_xref <- find_xref(gedcom, xrefs_indi(gedcom), c("NAME", "ROMN", "FONE"), associated_with)
  
  asso_notes <- purrr::map(association_notes, tidyged.internals::NOTE_STRUCTURE)
  
  asso_str <- tidyged.internals::ASSOCIATION_STRUCTURE(xref_indi = indi_xref,
                                                          relation_is_descriptor = association,
                                                          notes = asso_notes) %>% 
    tidyged.internals::add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  tidyged.internals::remove_section(gedcom, 1, "CHAN", "", xrefs = xref)
    asso_str <- dplyr::bind_rows(asso_str, tidyged.internals::CHANGE_DATE() %>% 
                                   tidyged.internals::add_levels(1))
  }
  
  next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, "INDI")
  
  gedcom %>%
    tibble::add_row(asso_str, .before = next_row) %>% 
    tidyged.internals::finalise() %>% 
    activate_indi(xref)
  
}

#' Add a family link as a spouse
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
  
  link_notes <- purrr::map(linkage_notes, tidyged.internals::NOTE_STRUCTURE)
  
  link <- tidyged.internals::SPOUSE_TO_FAMILY_LINK(xref_fam = family_xref, notes = link_notes) %>% 
    tidyged.internals::add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  tidyged.internals::remove_section(gedcom, 1, "CHAN", "", xrefs = xref)
    link <- dplyr::bind_rows(link, tidyged.internals::CHANGE_DATE() %>% 
                               tidyged.internals::add_levels(1))
  }
  
  next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, "INDI")
  
  gedcom %>%
    tibble::add_row(link, .before = next_row) %>% 
    tidyged.internals::finalise() %>% 
    activate_indi(xref)
}

#' Add a family link as a child
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
  
  link_notes <- purrr::map(linkage_notes, tidyged.internals::NOTE_STRUCTURE)
  
  link <- tidyged.internals::CHILD_TO_FAMILY_LINK(xref_fam = family_xref,
                                                  pedigree_linkage_type = linkage_type,
                                                  notes = link_notes) %>% 
    tidyged.internals::add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  tidyged.internals::remove_section(gedcom, 1, "CHAN", "", xrefs = xref)
    link <- dplyr::bind_rows(link, tidyged.internals::CHANGE_DATE() %>% 
                               tidyged.internals::add_levels(1))
  }
  
  next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, "INDI")
  
  gedcom %>%
    tibble::add_row(link, .before = next_row) %>% 
    tidyged.internals::finalise() %>% 
    activate_indi(xref)
}


#' Add family links as a child or spouse
#' 
#' This function adds links connecting an Individual record to existing Family Group records. Family 
#' links will be added to the Individual record, and the Family Group records will be updated to link
#' to this individual.
#'
#' @param gedcom A tidyged object.
#' @param parents A character vector of parents so that this person can be linked to a Family Group
#' record as a child. 
#' @param linkage_type A code used to indicate the relationship with the parent(s). Either, 
#' "birth" (default), "foster", or "adopted".
#' @param linkage_notes A character vector of notes accompanying this linkage.
#' These could be xrefs to existing Note records.
#' @param spouse A character vector (of length 1) of the spouse so that this person can be linked to 
#' a Family Group record as a spouse.
#' @param children A character vector of children so that this person can be linked to a Family Group
#' record as a spouse.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param update_date_changed Whether to add/update the change date for the record.
#'
#' @return An updated tidyged object with an expanded Individual record including
#' the family link(s) and expanded Family Group record(s) linking to this individual.
#' @export
add_indi_links_to_families <- function(gedcom,
                                       parents = character(),
                                       linkage_type = "birth",
                                       linkage_notes = character(),
                                       spouse = character(),
                                       children = character(),
                                       xref = character(),
                                       update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  if(length(spouse) > 1) stop("Only one spouse should be provided")
  
  if(length(parents) > 0) {
    parents <- purrr::map_chr(parents, get_valid_xref, 
                              gedcom = gedcom,
                              record_type = .pkgenv$record_string_indi, 
                              record_type_fn = is_indi)
    
    xref_famc <- dplyr::filter(gedcom, level == 1, tag %in% c("HUSB","WIFE"), value %in% parents) %>% 
      dplyr::count(record) %>% 
      dplyr::filter(n == max(n)) %>% 
      dplyr::pull(record)
    
    if(length(xref_famc) == 1) {
      gedcom <- add_indi_family_link_as_child(gedcom, xref_famc, linkage_type,
                                              linkage_notes, update_date_changed = update_date_changed)
    } else {
      choice <- utils::select.list(title = paste0("In which Family Group is ",
                                                  describe_indi(gedcom, xref, name_only = TRUE),
                                                  " a child?"),
                                   choices = describe_records(gedcom, xref_famc),
                                   multiple = FALSE)
      
      xref_famc <- stringr::str_extract(choice, "@[a-zA-Z0-9]{1,20}@")
      
      gedcom <- add_indi_family_link_as_child(gedcom, xref_famc, linkage_type,
                                              linkage_notes, update_date_changed = update_date_changed)
    }
    
    # add child to family record
    next_row <- tidyged.internals::find_insertion_point(gedcom, xref_famc, 0, "FAM")
    gedcom <- tibble::add_row(gedcom,
                              tibble::tibble(record = xref_famc, level = 1, tag = "CHIL", value = xref), 
                              .before = next_row) %>% 
      order_famg_children(xref_famc)
  }
  
  
  if(length(children) > 0 | length(spouse) > 0) {
    
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
    
    xref_fams <- unique(c(xref_fams1, xref_fams2))
    
    if(length(xref_fams) == 1) {
      gedcom <- add_indi_family_link_as_spouse(gedcom, xref_fams, update_date_changed = update_date_changed)
    } else {
      choice <- utils::select.list(title = paste0("In which Family Group is ",
                                                  describe_indi(gedcom, xref, name_only = TRUE),
                                                  " a spouse?"),
                                   choices = describe_records(gedcom, xref_fams),
                                   multiple = FALSE)
      
      xref_fams <- stringr::str_extract(choice, "@[a-zA-Z0-9]{1,20}@")
      
      gedcom <- add_indi_family_link_as_spouse(gedcom, xref_fams, update_date_changed = update_date_changed)
    }
    
    # add spouse to family record
    next_row <- tidyged.internals::find_insertion_point(gedcom, xref_fams, 0, "FAM")
    # determine whether to use HUSB or WIFE tag - look at sex first, then look at other tag
    sex <- tidyged.internals::gedcom_value(gedcom, xref, "SEX", 1)
    if(!sex %in% c("M", "F")) {
      other_sex <- dplyr::filter(gedcom, record == xref_fams, level = 1, tag %in% c("HUSB","WIFE"))$tag
      sex <- ifelse(length(other_sex) == 0 | other_sex == "WIFE", "M", "F")
    }
    gedcom <- tibble::add_row(gedcom,
                            tibble::tibble(record = xref_fams, level = 1, tag = ifelse(sex == "M", "HUSB", "WIFE"), value = xref), 
                            .before = next_row)
  }
  
  gedcom
}

