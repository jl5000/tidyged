
#' @export
add_individual_association <- function(gedcom,
                                       associated_with,
                                       association,
                                       association_notes = character()) {
  
  check_active_record_valid(gedcom, .pkgenv$record_string_indi, is_individual)
  
  indi_xref <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), associated_with)
  
  asso_notes <- purrr::map(association_notes, ~ if(grepl(xref_pattern, .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  asso_str <- ASSOCIATION_STRUCTURE(xref_indi = indi_xref,
                                    relation_is_descriptor = association,
                                    notes = asso_notes) %>% add_levels(1)
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(asso_str, .before = next_row) %>% 
    finalise()
  
}

#' @export
add_individual_family_link_as_spouse <- function(gedcom, 
                                                 family_xref,
                                                 linkage_notes = character()) {
  
  check_active_record_valid(gedcom, .pkgenv$record_string_indi, is_individual)
  
  link_notes <- purrr::map(linkage_notes, ~ if(grepl(xref_pattern, .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  link <- SPOUSE_TO_FAMILY_LINK(xref_fam = family_xref, notes = link_notes) %>% add_levels(1)
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(link, .before = next_row) %>% 
    finalise()
}

#' @export
add_individual_family_link_as_child <- function(gedcom, 
                                                family_xref,
                                                linkage_type = character(),
                                                linkage_notes = character()) {
  
  check_active_record_valid(gedcom, .pkgenv$record_string_indi, is_individual)
  
  link_notes <- purrr::map(linkage_notes, ~ if(grepl(xref_pattern, .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  link <- CHILD_TO_FAMILY_LINK(xref_fam = family_xref,
                               pedigree_linkage_type = linkage_type,
                               notes = link_notes) %>% add_levels(1)
  
  next_row <- find_insertion_point(gedcom, get_active_record(gedcom), 0, "INDI")
  
  gedcom %>%
    tibble::add_row(link, .before = next_row) %>% 
    finalise()
}
