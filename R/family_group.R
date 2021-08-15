

#' Add a Family Group record to a tidyged object
#' 
#' @details If you need to add further information about this family (e.g. events), use the 
#' `add_famg_event()` function.
#' 
#' The function will automatically add links to this family to the respective Individual 
#' records of the wife, husband, and children.
#' 
#' @param gedcom A tidyged object.
#' @param husband An xref identifying the husband of this family.
#' @param wife An xref identifying the wife of this family.
#' @param children A character vector of xrefs identifying the children of this family.
#' @param child_linkage_types Codes used to indicate the child to family relationships. If defined,
#' this must be a character vector the same size as children. Values must be one of:
#' "birth" (default), "adopted", or "foster".
#' @param number_of_children The reported number of children known to belong to this family, 
#' regardless of whether the associated children are represented here.
#' @param user_reference_numbers A unique user-defined number or text that the submitter 
#' uses to identify this record. You can supply more than one in a vector. You can also define a
#' user reference type by using a named vector (e.g c(id1 = "123A", id2 = "456B")).
#' @param family_notes A character vector of notes accompanying this Family group record. These could be
#' xrefs to existing Note records.
#' @param multimedia_links A character vector of Multimedia record xrefs accompanying this record.
#'
#' @return An updated tidyged object including the Family group record.
#' 
#' @export
add_famg <- function(gedcom,
                     husband = character(),
                     wife = character(),
                     children = character(),
                     child_linkage_types = rep("birth", length(children)),
                     number_of_children = character(),
                     user_reference_numbers = character(),
                     family_notes = character(),
                     multimedia_links = character()) {
  
  xref <- tidyged.internals::assign_xref_famg(gedcom)

  media_links <- create_multimedia_links(gedcom, multimedia_links)
  fam_notes <- create_note_structures(gedcom, family_notes)
  
  fam_record <- tidyged.internals::FAMILY_GROUP_RECORD(xref_fam = xref,
                                                       xref_husb = husband,
                                                       xref_wife = wife,
                                                       xrefs_chil = children,
                                                       count_of_children = number_of_children,
                                                       user_reference_number = user_reference_numbers,
                                                       notes = fam_notes,
                                                       multimedia_links = media_links) 
  
  temp <- gedcom %>%
    tibble::add_row(fam_record, .before = nrow(.))
  
  if(length(husband) == 1) {
    temp <- temp %>%
      set_active_record(husband) %>% 
      add_indi_family_link_as_spouse(xref)
  }
  if(length(wife) == 1) {
    temp <- temp %>%
      set_active_record(wife) %>% 
      add_indi_family_link_as_spouse(xref)
  }
  
  for(i in seq_along(children)) {
  
    temp <- temp %>% 
      set_active_record(children[i]) %>% 
      add_indi_family_link_as_child(xref, linkage_type = child_linkage_types[i]) 
  
  }
  
  temp %>% 
    order_famg_children(xref) %>% 
    set_active_record(xref)
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
#'                add_indi() %>% 
#'                add_indi() %>% 
#'                null_active_record(),
#'              gedcom(subm()) %>% 
#'                add_indi() %>% 
#'                add_indi() %>% 
#'                add_famg(husband = "@I1@", wife = "@I2@") %>% 
#'                remove_famg())
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) %>% 
#'                add_indi() %>% 
#'                add_indi() %>% 
#'                add_famg(husband = "@I1@", wife = "@I2@") %>% 
#'                remove_famg(remove_individuals = TRUE))
remove_famg <- function(gedcom, 
                        family_xref = character(),
                        remove_individuals = FALSE) {
  
  xref <- get_valid_xref(gedcom, family_xref, .pkgenv$record_string_famg, is_famg)
  
  if(remove_individuals) {
    
    ind_xrefs <- unique(dplyr::filter(gedcom, record == xref, level == 1,
                                      tag %in% c("HUSB", "WIFE", "CHIL"))$value)
    
    for(ind_xref in ind_xrefs) {
      
      gedcom <- remove_indi(gedcom, ind_xref)
      
    }
  }
  
  gedcom %>% 
    tidyged.internals::remove_section(1, "FAMC", xref) %>% 
    tidyged.internals::remove_section(2, "FAMC", xref) %>% 
    tidyged.internals::remove_section(1, "FAMS", xref) %>% 
    #tidyged.internals::remove_section(2, "FAMS", xref) %>% 
    dplyr::filter(record != xref, value != xref) %>%
    null_active_record()  
}

