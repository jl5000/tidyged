

#' Add an association with another individual
#'
#' @param gedcom A tidygedcom object.
#' @param associated_with A character string identifying the associated individual. This can either 
#' be an xref or a regular expression to match to an individual name.
#' @param association A word or phrase stating the nature of the association.
#' @param association_notes A character vector of notes accompanying this association.
#' These could be xrefs to existing Note records.
#' @param update_date_changed Whether to add/update the change date for the record.
#'
#' @return An updated tidygedcom object with an expanded Individual record including
#' this association.
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'                         add_individual() %>% 
#'                         add_individual_names("Joe Bloggs") %>% 
#'                         add_individual() %>% 
#'                         add_individual_names("Jimmy Bloggs") %>%
#'                         add_individual_association(associated_with = "Joe", association = "Friend") %>% 
#'                         remove_dates_for_tests(), "json2")
add_individual_association <- function(gedcom,
                                       associated_with,
                                       association,
                                       association_notes = character(),
                                       update_date_changed = TRUE) {
  
  check_active_record_valid(gedcom, .pkgenv$record_string_indi, is_individual)
  
  indi_xref <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), associated_with)
  
  asso_notes <- purrr::map(association_notes, ~ if(grepl(xref_pattern(), .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(user_text = .x) }  )
  
  asso_str <- ASSOCIATION_STRUCTURE(xref_indi = indi_xref,
                                    relation_is_descriptor = association,
                                    notes = asso_notes) %>% add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  remove_section(gedcom, 1, "CHAN", "", xrefs = get_active_record(gedcom))
    asso_str <- dplyr::bind_rows(asso_str, CHANGE_DATE() %>% add_levels(1))
  }
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(asso_str, .before = next_row) %>% 
    finalise()
  
}

#' Add a family link as a spouse
#'
#' @param gedcom A tidygedcom object.
#' @param family_xref The xref of the family associated of which this individual is a spouse.
#' @param linkage_notes A character vector of notes accompanying this linkage.
#' These could be xrefs to existing Note records.
#' @param update_date_changed Whether to add/update the change date for the record.
#'
#' @return An updated tidygedcom object with an expanded Individual record including
#' this family link.
#' @export
add_individual_family_link_as_spouse <- function(gedcom, 
                                                 family_xref,
                                                 linkage_notes = character(),
                                                 update_date_changed = TRUE) {
  
  check_active_record_valid(gedcom, .pkgenv$record_string_indi, is_individual)
  
  link_notes <- purrr::map(linkage_notes, ~ if(grepl(xref_pattern(), .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(user_text = .x) }  )
  
  link <- SPOUSE_TO_FAMILY_LINK(xref_fam = family_xref, notes = link_notes) %>% add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  remove_section(gedcom, 1, "CHAN", "", xrefs = get_active_record(gedcom))
    link <- dplyr::bind_rows(link, CHANGE_DATE() %>% add_levels(1))
  }
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(link, .before = next_row) %>% 
    finalise()
}

#' Add a family link as a child
#'
#' @param gedcom A tidygedcom object.
#' @param family_xref The xref of the family associated of which this individual is a child.
#' @param linkage_type A code used to indicate the child to family relationship. Either, 
#' "birth" (default), "foster", or "adopted".
#' @param linkage_notes A character vector of notes accompanying this linkage.
#' These could be xrefs to existing Note records.
#' @param update_date_changed Whether to add/update the change date for the record.
#'
#' @return An updated tidygedcom object with an expanded Individual record including
#' this family link.
#' @export
add_individual_family_link_as_child <- function(gedcom, 
                                                family_xref,
                                                linkage_type = "birth",
                                                linkage_notes = character(),
                                                update_date_changed = TRUE) {
  
  check_active_record_valid(gedcom, .pkgenv$record_string_indi, is_individual)
  
  link_notes <- purrr::map(linkage_notes, ~ if(grepl(xref_pattern(), .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(user_text = .x) }  )
  
  link <- CHILD_TO_FAMILY_LINK(xref_fam = family_xref,
                               pedigree_linkage_type = linkage_type,
                               notes = link_notes) %>% add_levels(1)
  
  if(update_date_changed) {
    gedcom <-  remove_section(gedcom, 1, "CHAN", "", xrefs = get_active_record(gedcom))
    link <- dplyr::bind_rows(link, CHANGE_DATE() %>% add_levels(1))
  }
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(link, .before = next_row) %>% 
    finalise()
}
