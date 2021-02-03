

#' Add a Family Group record to a tidyged object
#' 
#' @details If you need to add further information about this family (e.g. events), use the 
#' add_family_event_*() functions.
#' 
#' The function will automatically add links to this family to the respective Individual 
#' records of the wife, husband, and children.
#' 
#' @param gedcom A tidyged object.
#' @param husband A character string identifying the husband of this family. This can either 
#' be an xref or a regular expression to match to an individual name.
#' @param wife A character string identifying the wife of this family. This can either 
#' be an xref or a regular expression to match to an individual name.
#' @param children A character vector of other Individual records that are children of this
#' family. Children can either be referenced by an xref or by a regular expression 
#' to match to an individual name.
#' @param child_linkage_types Codes used to indicate the child to family relationships. If defined,
#' this must be a character vector the same size as children. Values must be one of:
#' "birth" (default), "adopted", or "foster".
#' @param number_of_children The reported number of children known to belong to this family, 
#' regardless of whether the associated children are represented here.
#' @param user_reference_number A unique user-defined number or text that the submitter 
#' uses to identify this record. You can supply more than one in a vector.
#' @param user_reference_type A user-defined definition of the user_reference_number(s). If this
#' parameter is used, there must be a reference type for every reference number defined.
#' @param family_notes A character vector of notes accompanying this Family group record. These could be
#' xrefs to existing Note records.
#' @param multimedia_links A character vector of multimedia file references accompanying this 
#' Family group record. These could be xrefs to existing Multimedia records.
#'
#' @return An updated tidyged object including the Family group record.
#' 
#' @export
add_family_group <- function(gedcom,
                             husband = character(),
                             wife = character(),
                             children = character(),
                             child_linkage_types = rep("birth", length(children)),
                             number_of_children = character(),
                             user_reference_number = character(),
                             user_reference_type = character(),
                             family_notes = character(),
                             multimedia_links = character()) {
  
  xref <- assign_xref(.pkgenv$xref_prefix_fam, gedcom = gedcom)
  
  xref_husb <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), husband)
  xref_wife <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), wife)
  xrefs_chil <- purrr::map_chr(children, find_xref,
                               gedcom = gedcom, record_xrefs = xrefs_individuals(gedcom), 
                               tags = c("NAME", "ROMN", "FONE"))
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                               gedcom = gedcom, record_xrefs = xrefs_multimedia(gedcom), tags = "FILE") %>% 
    purrr::map(tidyged.internals::MULTIMEDIA_LINK)
  
  fam_notes <- purrr::map(family_notes, tidyged.internals::NOTE_STRUCTURE)
  
  fam_record <- tidyged.internals::FAMILY_GROUP_RECORD(xref_fam = xref,
                                                          xref_husb = xref_husb,
                                                          xref_wife = xref_wife,
                                                          xrefs_chil = xrefs_chil,
                                                          count_of_children = number_of_children,
                                                          user_reference_number = user_reference_number,
                                                          user_reference_type = user_reference_type,
                                                          notes = fam_notes,
                                                          multimedia_links = media_links) 
  
  temp <- gedcom %>%
    tibble::add_row(fam_record, .before = nrow(.))
  
  if(length(xref_husb) == 1) {
    temp <- temp %>%
      set_active_record(xref_husb) %>% 
      add_individual_family_link_as_spouse(xref)
  }
  if(length(xref_wife) == 1) {
    temp <- temp %>%
      set_active_record(xref_wife) %>% 
      add_individual_family_link_as_spouse(xref)
  }
  
  for(i in seq_along(xrefs_chil)) {
  
    temp <- temp %>% 
      set_active_record(xrefs_chil[i]) %>% 
      add_individual_family_link_as_child(xref, linkage_type = child_linkage_types[i]) 
  
  }
  
  set_active_record(temp, xref)
}





#' Remove a Family group record from a tidyged object
#' 
#' This function removes a record containing details of a family group.
#' 
#' This function will also automatically remove references to this record in other 
#' individual records. If remove_individuals is set to TRUE, it will also remove
#' all records for individuals in this family (including associations).
#'
#' @param gedcom A tidyged object.
#' @param family_xref The xref of a Family Group record to act on if one is not 
#' activated (will override active record).
#' @param remove_individuals Whether to also remove the records for all Individuals
#' in the family.
#'
#' @return An updated tidyged object excluding the selected Family group record 
#' (and potentially the individuals within it).
#' @export
#' @tests
#' expect_equal(gedcom(subm()) %>% 
#'                add_individual() %>% 
#'                add_individual() %>% 
#'                null_active_record(),
#'              gedcom(subm()) %>% 
#'                add_individual() %>% 
#'                add_individual() %>% 
#'                add_family_group(husband = "@I1@", wife = "@I2@") %>% 
#'                remove_family_group())
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) %>% 
#'                add_individual() %>% 
#'                add_individual() %>% 
#'                add_family_group(husband = "@I1@", wife = "@I2@") %>% 
#'                remove_family_group(remove_individuals = TRUE))
remove_family_group <- function(gedcom, 
                                family_xref = character(),
                                remove_individuals = FALSE) {
  
  xref <- get_valid_xref(gedcom, family_xref, .pkgenv$record_string_fam, is_family)
  
  if(remove_individuals) {
    
    ind_xrefs <- unique(dplyr::filter(gedcom, record == xref,
                                      tag %in% c("HUSB", "WIFE", "CHIL"))$value)
    
    for(ind_xref in ind_xrefs) {
      
      gedcom <- remove_individual(gedcom, ind_xref)
      
    }
  }
  
  gedcom %>% 
    remove_section(1, "FAMC", xref) %>% 
    remove_section(2, "FAMC", xref) %>% 
    remove_section(1, "FAMS", xref) %>% 
    remove_section(2, "FAMS", xref) %>% 
    dplyr::filter(record != xref, value != xref) %>%
    null_active_record()  
}

