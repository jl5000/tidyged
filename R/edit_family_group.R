

#' Add a Family record to a tidygedcom object
#'
#' This function adds a record containing details of a family.
#' 
#' This function will automatically assign a unique xref for this record. Most users
#' will only need to use the husband, wife, children, and family_notes parameters (and of course gedcom).
#' 
#' If you need to add further information about this family (e.g. events), use the 
#' add_family_event_*() functions.
#' 
#' The function will automatically split the family_notes onto separate lines if the 
#' character limit in the Gedcom standard is exceeded. It will also automatically add links
#' to this family to the respective Individual records of the wife, husband, and children.
#' 
#' This function will also automatically add appropriate references to this record in the
#' respective individual records of the husband/wife/children.
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
#' adopted, birth, foster, sealing.
#' @param child_linkage_statuses Codes that allow passing on the users opinion of the 
#' status of a child to family link. If defined, this must be a character vector the same 
#' size as children. Values must be one of: challenged, disproven, proven.
#' @param number_of_children The reported number of children known to belong to this family, 
#' regardless of whether the associated children are represented here.
#' @param submitters A character vector of submitters of this record. A submitter can either be
#' referenced by an xref or by a regular expression to match to a submitter name.
#' @param user_reference_number A user-defined number or text that the submitter uses to identify 
#' this record. See the Gedcom 5.5.1 Standard for more details.
#' @param user_reference_type A user-defined definition of the user_reference_number.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param restriction_notice Only for Ancestral File usage. See the Gedcom 5.5.1 Standard for more 
#' details.
#' @param family_notes A character vector of notes accompanying this Family record. These could be
#' xrefs to existing Note records.
#'
#' @return An updated tidygedcom object including the Family record.
#' 
#' @export
add_family_group <- function(gedcom,
                             husband = character(),
                             wife = character(),
                             children = character(),
                             child_linkage_types = character(),
                             child_linkage_statuses = character(),
                             number_of_children = character(),
                             submitters = character(),
                             user_reference_number = character(),
                             user_reference_type = character(),
                             automated_record_id = character(),
                             restriction_notice = character(),
                             family_notes = character()) {
  
  xref <- assign_xref(xref_prefix_fam(), gedcom = gedcom)
  
  xref_husb <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), husband)
  xref_wife <- find_xref(gedcom, xrefs_individuals(gedcom), c("NAME", "ROMN", "FONE"), wife)
  xrefs_chil <- purrr::map_chr(children, find_xref,
                               gedcom = gedcom, record_xrefs = xrefs_individuals(gedcom), 
                               tags = c("NAME", "ROMN", "FONE"))
  xrefs_subm <- purrr::map_chr(submitters, find_xref, 
                               gedcom = gedcom, record_xrefs = xrefs_submitters(gedcom), tags = "NAME")
  
  fam_notes <- purrr::map(family_notes, ~ if(grepl(xref_pattern, .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  fam_record <- FAMILY_RECORD(xref_fam = xref,
                              restriction_notice = restriction_notice,
                              xref_husb = xref_husb,
                              xref_wife = xref_wife,
                              xrefs_chil = xrefs_chil,
                              count_of_children = number_of_children,
                              xrefs_subm = xrefs_subm,
                              user_reference_number = user_reference_number,
                              user_reference_type = user_reference_type,
                              automated_record_id = automated_record_id,
                              notes = fam_notes) 
  
  temp <- gedcom %>%
    tibble::add_row(fam_record, .before = nrow(.)) %>%
    set_active_record(xref_husb) %>% 
    add_individual_family_link_as_spouse(xref) %>%
    set_active_record(xref_wife) %>% 
    add_individual_family_link_as_spouse(xref)
  
  message("Family link also added to the Individual record for husband")
  message("Family link also added to the Individual record for wife")
  
  for(xref_chil in xrefs_chil) {
    #TODO: Linkage status/type
    temp <- temp %>% 
      set_active_record(xref_chil) %>% 
      add_individual_family_link_as_child(xref) 
    
    message("Family link also added to the Individual record for child ", i)
  }
  
  set_active_record(temp, xref)
}





#' Remove a Family record from a tidygedcom object
#' 
#' This function removes a record containing details of a family.
#' 
#' This function will also automatically remove references to this record in other 
#' individual records. If remove_individuals is set to TRUE, it will also remove
#' all records for individuals in this family (including associations, but not aliases).
#'
#' @param gedcom A tidygedcom object.
#' @param remove_individuals Whether to also remove the individual records for all individuals
#' in the family.
#'
#' @return An updated tidygedcom object excluding the active Family record (and potentially the 
#' individuals within it).
#' @export
remove_family <- function(gedcom, remove_individuals = FALSE) {
  
  check_active_record_valid(gedcom, record_string_fam(), is_family)
  active_record <- get_active_record(gedcom)
  
  ind_xrefs <- unique(dplyr::filter(gedcom, record == active_record,
                                    tag %in% c("HUSB", "WIFE", "CHIL"))$value)
  
  temp <- remove_section(gedcom, 1, "FAMC", active_record) %>% 
    remove_section(2, "FAMC", active_record) %>% 
    remove_section(1, "FAMS", active_record) %>% 
    remove_section(2, "FAMS", active_record)
  
  if(remove_individuals) {
    
    for(xref in ind_xrefs) {
      
      temp <- activate_individual_record(temp, xref = xref) %>% 
        remove_individual()
      
    }
  }
  
  null_active_record(temp)  
}


