

#' Add a Family Group record to a tidygedcom object
#'
#' This function adds a record containing details of a family group.
#' 
#' This function will automatically assign a unique xref for this record. Most users
#' will only need to use the husband, wife, children, and family_notes parameters (and of course gedcom).
#' 
#' If you need to add further information about this family (e.g. events), use the 
#' add_family_event_*() functions.
#' 
#' The function will automatically add links to this family to the respective Individual 
#' records of the wife, husband, and children.
#' 
#' @param gedcom A tidygedcom object.
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
#' @param user_reference_number A user-defined number or text that the submitter uses to identify 
#' this record. See the Gedcom 5.5.5 Specification for more details.
#' @param user_reference_type A user-defined definition of the user_reference_number.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param family_notes A character vector of notes accompanying this Family group record. These could be
#' xrefs to existing Note records.
#' @param multimedia_links A character vector of multimedia file references accompanying this 
#' Family group record. These could be xrefs to existing Multimedia records.
#'
#' @return An updated tidygedcom object including the Family group record.
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
                             automated_record_id = character(),
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
    purrr::map(MULTIMEDIA_LINK)
  
  fam_notes <- purrr::map(family_notes, ~ if(grepl(xref_pattern(), .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(user_text = .x) }  )
  
  fam_record <- FAMILY_GROUP_RECORD(xref_fam = xref,
                                    xref_husb = xref_husb,
                                    xref_wife = xref_wife,
                                    xrefs_chil = xrefs_chil,
                                    count_of_children = number_of_children,
                                    user_reference_number = user_reference_number,
                                    user_reference_type = user_reference_type,
                                    automated_record_id = automated_record_id,
                                    notes = fam_notes,
                                    multimedia_links = media_links) 
  
  temp <- gedcom %>%
    tibble::add_row(fam_record, .before = nrow(.))
  
  if(length(xref_husb) == 1) {
    temp <- temp %>%
      set_active_record(xref_husb) %>% 
      add_individual_family_link_as_spouse(xref)
    
    message("Family link also added to the Individual record for husband: ", 
            get_individual_name(temp, xref_husb))
  }
  if(length(xref_wife) == 1) {
    temp <- temp %>%
      set_active_record(xref_wife) %>% 
      add_individual_family_link_as_spouse(xref)
    
    message("Family link also added to the Individual record for wife: ",
            get_individual_name(temp, xref_wife))
  }
  
  for(i in seq_along(xrefs_chil)) {
  
    temp <- temp %>% 
      set_active_record(xrefs_chil[i]) %>% 
      add_individual_family_link_as_child(xref, linkage_type = child_linkage_types[i]) 
    
    message("Family link also added to the Individual record for child: ",
            get_individual_name(temp, xrefs_chil[i]))
  }
  
  set_active_record(temp, xref)
}





#' Remove a Family group record from a tidygedcom object
#' 
#' This function removes a record containing details of a family group.
#' 
#' This function will also automatically remove references to this record in other 
#' individual records. If remove_individuals is set to TRUE, it will also remove
#' all records for individuals in this family (including associations).
#'
#' @param gedcom A tidygedcom object.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param remove_individuals Whether to also remove the individual records for all individuals
#' in the family.
#'
#' @return An updated tidygedcom object excluding the active Family group record (and potentially the 
#' individuals within it).
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
                                xref = character(),
                                remove_individuals = FALSE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_fam, is_family)
  
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



#' Remove Family Group records with no members
#'
#' @param gedcom A tidygedcom object.
#'
#' @return A new tidygedcom object with empty Family Group records removed.
#' @export
remove_empty_family_groups <- function(gedcom) {
  
  tags <- c("HUSB", "WIFE", "CHIL")
  fam_xrefs <- xrefs_families(gedcom)
  num_removed <- 0
  
  for(xref in fam_xrefs) {
    
    members <- dplyr::filter(gedcom, record == xref, tag %in% tags)
    
    if(nrow(members) == 0) {
      gedcom <- gedcom %>% 
        activate_family_group_record(xref) %>% 
        remove_family_group()
      
      num_removed <- num_removed + 1
    }
    
  }
 
  message(num_removed, " empty family groups removed.")
  gedcom
  
}
