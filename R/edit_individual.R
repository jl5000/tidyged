


#' Add an Individual record to a tidygedcom object
#'
#' This function adds a new record for an individual.
#'
#' This function will automatically assign a unique xref for this record. Most users
#' will only need to use the sex, submitters, and individual_notes parameters (and of course gedcom).
#' 
#' If you need to add further information about this individual (e.g. names), use the 
#' add_individual_* functions.
#' 
#' The function will automatically split the individual_notes onto separate lines if the 
#' character limit in the Gedcom standard is exceeded.
#'
#' @param gedcom A tidygedcom object.
#' @param sex The sex of the individual. Either "M" (male), "F" (female), or "U" (undetermined).
#' @param submitters A character vector of submitters of this record. A submitter can either be
#' referenced by an xref or by a regular expression to match to a submitter name.
#' @param aliases A character vector of other Individual records that are aliases of this
#' individual. An individual can either be referenced by an xref or by a regular expression 
#' to match to an individual name.
#' @param submitters_interested_in_ancestors A character vector of submitters interested in 
#' ancestors of this individual. A submitter can either be referenced by an xref or by a 
#' regular expression to match to a submitter name.
#' @param submitters_interested_in_descendants A character vector of submitters interested in 
#' descendants of this individual. A submitter can either be referenced by an xref or by a 
#' regular expression to match to a submitter name.
#' @param permanent_record_file_number The record number that uniquely identifies this record 
#' within a registered network resource. See the Gedcom 5.5.1 Standard for more details.
#' @param ancestral_file_number A unique permanent record number of an individual record 
#' contained in the Family History Department's Ancestral File.
#' @param user_reference_number A user-defined number or text that the submitter uses to identify 
#' this record. See the Gedcom 5.5.1 Standard for more details.
#' @param user_reference_type A user-defined definition of the user_reference_number.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param individual_notes A character vector of notes accompanying this Individual record.
#' These could be xrefs to existing Note records.
#'
#' @return An updated tidygedcom object including the Individual record.
#' @export
add_individual <- function(gedcom,
                           sex = character(),
                           submitters = character(),
                           aliases = character(),
                           submitters_interested_in_ancestors = character(),
                           submitters_interested_in_descendants = character(),
                           permanent_record_file_number = character(),
                           ancestral_file_number = character(),
                           user_reference_number = character(),
                           user_reference_type = character(),
                           automated_record_id = character(),
                           individual_notes = character()) {
  
  xref <- assign_xref(xref_prefix_indi(), gedcom = gedcom)
  
  xrefs_subm <- purrr::map_chr(submitters, find_xref, 
                               gedcom = gedcom, record_xrefs = xrefs_submitters(gedcom), tags = "NAME")
  
  xrefs_alia <- purrr::map_chr(aliases, find_xref, 
                               gedcom = gedcom, record_xrefs = xrefs_individuals(gedcom), 
                               tags = c("NAME", "ROMN", "FONE"))
  
  xrefs_subm_interested_in_ancestors <- 
    purrr::map_chr(submitters_interested_in_ancestors, find_xref, 
                   gedcom = gedcom, record_xrefs = xrefs_submitters(gedcom), tags = "NAME")
  
  xrefs_subm_interested_in_descendants <- 
    purrr::map_chr(submitters_interested_in_descendants, find_xref, 
                   gedcom = gedcom, record_xrefs = xrefs_submitters(gedcom), tags = "NAME")
  
  indi_notes <- purrr::map(individual_notes, ~ if(grepl(xref_pattern, .x)) {
    NOTE_STRUCTURE(xref_note = .x) } else { NOTE_STRUCTURE(submitter_text = .x) }  )
  
  ind_record <- INDIVIDUAL_RECORD(xref_indi = xref,
                                  sex_value = sex,
                                  xrefs_subm = xrefs_subm,
                                  xrefs_alia = xrefs_alia,
                                  xrefs_subm_interested_in_ancestors = xrefs_subm_interested_in_ancestors,
                                  xrefs_subm_interested_in_descendants = xrefs_subm_interested_in_descendants,
                                  permanent_record_file_number = permanent_record_file_number,
                                  ancestral_file_number = ancestral_file_number,
                                  user_reference_number = user_reference_number,
                                  user_reference_type = user_reference_type,
                                  automated_record_id = automated_record_id,
                                  notes = indi_notes) 
  
  gedcom %>%
    tibble::add_row(ind_record, .before = nrow(.)) %>% 
    set_active_record(xref)
}


#' Remove an Individual record from a tidygedcom object
#' 
#' This function removes an active Individual record from the tidygedcom object.
#' 
#' At a minimum it will also remove references to this individual in Family records.
#' If remove_associations is TRUE (default) it will remove associations with this
#' individual in other Individual records.
#' If remove_aliases is TRUE it will remove other Individual records giving aliases of this
#' individual.
#' 
#' Given a sufficiently complex and joined up GEDCOM file, this can cause a cascade of dependent
#' individual references and records being removed.
#'
#' @param gedcom A tidygedcom object.
#' @param remove_aliases Whether to also remove the individual records given as aliases of
#' this individual. Defaults to FALSE.
#' @param remove_associations Whether to also remove associations with this individual in 
#' other individual records. Defaults to TRUE.
#'
#' @return An updated tidygedcom object excluding the active Individual record.
#' 
#' @export
remove_individual <- function(gedcom, remove_aliases = FALSE, remove_associations = TRUE) {
  
  check_active_record_valid(gedcom, record_string_indi(), is_individual)
  active_record <- get_active_record(gedcom)
  
  if(remove_aliases) {
    aliases <- dplyr::filter(gedcom, record == active_record, tag == "ALIA")$value
    
    for(alias in aliases) {
      message("Alias record ", 
              get_individual_name(gedcom, alias), 
              " for ", get_individual_name(gedcom, active_record),
              " also removed.\n",
              "Alias records for this alias will not be deleted")
      
      gedcom <- activate_individual_record(gedcom, xref = alias) %>% 
        remove_individual()
    }
  }
  
  if(remove_associations) {
    gedcom <- remove_section(gedcom, 1, "ASSO", active_record)
    message("Associations with ", get_individual_name(gedcom, active_record),
            " also removed")
  }
  
  gedcom %>% 
    dplyr::filter(record != active_record, value != active_record) %>% 
    null_active_record()
}


