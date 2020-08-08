

#' Add a Family record to a tidygedcom object
#'
#' @details This function will automatically assign a unique xref for this record. Most users
#' will only need to use the husband, wife, children, and family_notes parameters (and of course gedcom).
#' 
#' If you need to add further information about this family (e.g. events), use the 
#' add_family_event function.
#' 
#' The function will automatically split the family_notes onto separate lines if the 
#' character limit in the Gedcom standard is exceeded. It will also automatically add links
#' to this family to the respective Individual records of the wife, husband, and children.
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
#' @seealso [add_family_event()]
#' 
#' @export
add_family <- function(gedcom,
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
  
  fam_notes <- purrr::map(family_notes, ~ if(grepl("^@.{1,20}@$", .x)) {
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
  
  for(i in seq_along(xrefs_chil)) {
    #TODO: Linkage status/type
    temp <- temp %>% 
      set_active_record(xrefs_chil[i]) %>% 
      add_individual_family_link_as_child(xref) 
    message("Family link also added to the Individual record for child ", i)
  }
  
  set_active_record(temp, xref)
}


add_family_event <- function(gedcom,
                             family_xref = character(),
                             event_type = character(),
                             event_subtype = character(),
                             event_descriptor = character(),
                             husband_age_at_event = character(),
                             wife_age_at_event = character(),
                             event_date = date_value(),
                             event_cause = character(),
                             event_place = PLACE_STRUCTURE(character()),
                             event_address = ADDRESS_STRUCTURE(character())) {
  
  check_active_record_valid(gedcom, record_string_fam(), is_family)
  
  
}

#' @export
#' @rdname add_family_event
add_family_event_annulment <- purrr::partial(add_family_event, event_type = "ANUL")
#' @export
#' @rdname add_family_event
add_family_event_census <- purrr::partial(add_family_event, event_type = "CENS")
#' @export
#' @rdname add_family_event
add_family_event_divorce <- purrr::partial(add_family_event, event_type = "DIV")
#' @export
#' @rdname add_family_event
add_family_event_divorce_filed <- purrr::partial(add_family_event, event_type = "DIVF")
#' @export
#' @rdname add_family_event
add_family_event_engagement <- purrr::partial(add_family_event, event_type = "ENGA")
#' @export
#' @rdname add_family_event
add_family_event_marriage_banns <- purrr::partial(add_family_event, event_type = "MARB")
#' @export
#' @rdname add_family_event
add_family_event_marriage_contract <- purrr::partial(add_family_event, event_type = "MARC")
#' @export
#' @rdname add_family_event
add_family_event_marriage <- purrr::partial(add_family_event, event_type = "MARR")
#' @export
#' @rdname add_family_event
add_family_event_marriage_license <- purrr::partial(add_family_event, event_type = "MARL")
#' @export
#' @rdname add_family_event
add_family_event_marriage_settlement <- purrr::partial(add_family_event, event_type = "MARS")
#' @export
#' @rdname add_family_event
add_family_event_residence <- purrr::partial(add_family_event, event_type = "RESI")
#' @export
#' @rdname add_family_event
add_family_event_other <- purrr::partial(add_family_event, event_type = "EVEN")




#' Remove a Family record from a tidygedcom object
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
  #TODO: Remove subordinate ADOP tags
  ind_xrefs <- unique(dplyr::filter(gedcom, record == get_active_record(gedcom),
                                    tag %in% c("HUSB", "WIFE", "CHIL"))$value)
  
  temp <- dplyr::filter(gedcom, record != get_active_record(gedcom))
  
  if(remove_individuals) {
    
    for(i in seq_along(ind_xrefs)) {
      
      temp <- activate_individual_record(temp, xref = ind_xrefs[i]) %>% 
        remove_individual()
      
    }
    message("Records for individuals in the family have also been removed.\n",
            "This has also removed associations with these individuals, but not any aliases.")
  }
  
  null_active_record(temp)  
}


