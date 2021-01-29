

#' Add an association with another individual
#'
#' @param gedcom A tidyged object.
#' @param associated_with A character string identifying the associated individual. This can either 
#' be an xref or a regular expression to match to an individual name.
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
#'                         add_individual("Joe /Bloggs/") %>% 
#'                         add_individual("Jimmy /Bloggs/") %>% 
#'                         add_individual_association(associated_with = "Joe", association = "Friend") %>% 
#'                         remove_dates_for_tests(), "json2")
add_individual_association <- function(gedcom,
                                       associated_with,
                                       association,
                                       association_notes = character(),
                                       xref = character(),
                                       update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_individual)
  
  indi_xref <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), associated_with)
  
  asso_notes <- purrr::map(association_notes, tidyged.internals::NOTE_STRUCTURE)
  
  asso_str <- tidyged.internals::ASSOCIATION_STRUCTURE(xref_indi = indi_xref,
                                                          relation_is_descriptor = association,
                                                          notes = asso_notes) %>% 
    tidyged.internals::add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  remove_section(gedcom, 1, "CHAN", "", xrefs = xref)
    asso_str <- dplyr::bind_rows(asso_str, tidyged.internals::CHANGE_DATE() %>% 
                                   tidyged.internals::add_levels(1))
  }
  
  next_row <- find_insertion_point(gedcom, xref, 0, "INDI")
  
  gedcom %>%
    tibble::add_row(asso_str, .before = next_row) %>% 
    tidyged.internals::finalise() %>% 
    activate_individual_record(xref)
  
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
#' @export
add_individual_family_link_as_spouse <- function(gedcom, 
                                                 family_xref,
                                                 linkage_notes = character(),
                                                 xref = character(),
                                                 update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_individual)
  
  link_notes <- purrr::map(linkage_notes, tidyged.internals::NOTE_STRUCTURE)
  
  link <- tidyged.internals::SPOUSE_TO_FAMILY_LINK(xref_fam = family_xref, notes = link_notes) %>% 
    tidyged.internals::add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  remove_section(gedcom, 1, "CHAN", "", xrefs = xref)
    link <- dplyr::bind_rows(link, tidyged.internals::CHANGE_DATE() %>% 
                               tidyged.internals::add_levels(1))
  }
  
  next_row <- find_insertion_point(gedcom, xref, 0, "INDI")
  
  gedcom %>%
    tibble::add_row(link, .before = next_row) %>% 
    tidyged.internals::finalise() %>% 
    activate_individual_record(xref)
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
#' @export
add_individual_family_link_as_child <- function(gedcom, 
                                                family_xref,
                                                linkage_type = "birth",
                                                linkage_notes = character(),
                                                xref = character(),
                                                update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_individual)
  
  link_notes <- purrr::map(linkage_notes, tidyged.internals::NOTE_STRUCTURE)
  
  link <- tidyged.internals::CHILD_TO_FAMILY_LINK(xref_fam = family_xref,
                                                     pedigree_linkage_type = linkage_type,
                                                     notes = link_notes) %>% 
    tidyged.internals::add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  remove_section(gedcom, 1, "CHAN", "", xrefs = xref)
    link <- dplyr::bind_rows(link, tidyged.internals::CHANGE_DATE() %>% 
                               tidyged.internals::add_levels(1))
  }
  
  next_row <- find_insertion_point(gedcom, xref, 0, "INDI")
  
  gedcom %>%
    tibble::add_row(link, .before = next_row) %>% 
    tidyged.internals::finalise() %>% 
    activate_individual_record(xref)
}
