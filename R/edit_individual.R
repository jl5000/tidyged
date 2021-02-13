


#' Add an Individual record to a tidyged object
#'
#' @details If you need to add further information about this individual (e.g. names), use the 
#' add_indi_* functions.
#' 
#' @param gedcom A tidyged object.
#' @param quick_name A shortcut to quickly define a name for this individual. This is a shortcut for
#' the add_indi_names() function (which you should really use instead), but this is useful
#' for quick demonstrations.
#' @param sex The sex of the individual. Either "M" (male), "F" (female), "U" (undetermined),
#' "X" (intersex), or "N" (not recorded).
#' @param user_reference_number A unique user-defined number or text that the submitter 
#' uses to identify this record. You can supply more than one in a vector.
#' @param user_reference_type A user-defined definition of the user_reference_number(s). If this
#' parameter is used, there must be a reference type for every reference number defined.
#' @param indi_notes A character vector of notes accompanying this Individual record.
#' These could be xrefs to existing Note records.
#' @param multimedia_links A character vector of multimedia file references accompanying this 
#' Individual record. These could be xrefs to existing Multimedia records.
#'
#' @return An updated tidyged object including the Individual record.
#' @export
#' @tests
#' expect_snapshot_value(add_indi(gedcom(subm("Me")),
#'                                      sex = "M", user_reference_number = 1234,
#'                                      user_reference_type = "something",
#'                                      indi_notes = c("Note1", "Note 2")) %>% 
#'                        remove_dates_for_tests(), "json2")
add_indi <- function(gedcom,
                     quick_name = character(),
                     sex = "U",
                     user_reference_number = character(),
                     user_reference_type = character(),
                     indi_notes = character(),
                     multimedia_links = character()) {
  
  xref <- assign_xref(.pkgenv$xref_prefix_indi, gedcom = gedcom)
  
  indiv_notes <- purrr::map(indi_notes, tidyged.internals::NOTE_STRUCTURE)
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_multimedia(gedcom), tags = "FILE") %>% 
    purrr::map(tidyged.internals::MULTIMEDIA_LINK)

  ind_record <- tidyged.internals::INDIVIDUAL_RECORD(xref_indi = xref,
                                                     sex_value = sex,
                                                     user_reference_number = user_reference_number,
                                                     user_reference_type = user_reference_type,
                                                     notes = indiv_notes,
                                                     multimedia_links = media_links) 
  
  temp <- gedcom %>%
    tibble::add_row(ind_record, .before = nrow(.)) %>% 
    set_active_record(xref)
  
  if (length(quick_name) > 0) 
    temp <- add_indi_names(temp, given = quick_name)
  
  temp
}


#' Remove an Individual record from a tidyged object
#' 
#' This function removes an active Individual record from the tidyged object.
#' 
#' At a minimum it will also remove references to this individual in Family group records.
#' If remove_associations is TRUE (default) it will remove associations with this
#' individual in other Individual records.
#' 
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param remove_associations Whether to also remove associations with this individual in 
#' other individual records. Defaults to TRUE.
#'
#' @return An updated tidyged object excluding the selected Individual record.
#' 
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) %>% add_indi() %>% remove_indi())
remove_indi <- function(gedcom, 
                        individual = character(),
                        remove_associations = TRUE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  if(remove_associations) gedcom <- remove_section(gedcom, 1, "ASSO", xref)

  gedcom %>% 
    dplyr::filter(record != xref, value != xref) %>% 
    null_active_record()
}



#' Remove all descendants for an individual
#' 
#' This function removes an entire branch of the family tree below a certain individual.
#' 
#' @details WARNING: This function can result in the removal of a vast amount of data as it relies on
#' recursive deletion. It will tell the user precisely what it is removing. Be sure the function has done 
#' what you expect before accepting the results. It is recommended that you use this function with extreme 
#' caution if you think a descendant (or their spouse) may be connected to an individual on another 
#' branch of your tree.
#' 
#' If you set remove_spouses = TRUE, the function will also remove all spouses of the individual
#' given (at the top level) and their descendants. It does not go as far as removing other spouses of
#' spouses.
#' 
#' If you wanted to just remove all descendants and associated family group records, you would
#' use the function with the default inputs. If you wanted to keep the (memberless) family group
#' records, you would set remove_empty_families = FALSE.
#' 
#' If remove_families, remove_individual, and remove_spouses are all TRUE, then the individual's
#' (memberless) family group record will also be deleted.
#' 
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param remove_individual Whether to also remove the individual themselves.
#' @param remove_spouses Whether to also remove all spouses of this individual (and their descendants).
#' @param remove_empty_families Whether to also remove all of the empty descendant Family Group records.
#'
#' @return A shorter tidyged object without the descendants of the individual.
#' @export
remove_descendants <- function(gedcom,
                               individual = character(),
                               remove_individual = FALSE,
                               remove_spouses = FALSE,
                               remove_empty_families = TRUE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  spou_xref <- get_spouses(gedcom, xref)
  chil_xref <- get_children(gedcom, xref)
  fams_xref <- get_families_as_spouse(gedcom, xref)
  
  # if spouse is to be removed, add their children to be removed
  if (remove_spouses) {
    # we don't use purrr::map here because the return values could vary in length
    spou_chil <- NULL
    for(i in seq_along(spou_xref)) {
      spou_chil <- c(spou_chil, get_children(gedcom, spou_xref[i]))
    }
    chil_xref <- unique(c(chil_xref, spou_chil))
  }
  
  #deal with family groups first (while the individuals are still in them)
  if (remove_spouses & remove_individual & remove_empty_families) {
    for(i in seq_along(fams_xref)) {
      message(describe_famg(gedcom, fams_xref[i], short_desc = TRUE), " removed")
      gedcom <- remove_famg(gedcom, fams_xref[i])
    }
  }
 
  if (remove_spouses) {
    for(i in seq_along(spou_xref)) {
      message(describe_indi(gedcom, spou_xref[i], short_desc = TRUE), " removed")
      gedcom <- remove_indi(gedcom, spou_xref[i])
    }
  }
  
  if (remove_individual) {
    message(describe_indi(gedcom, xref, short_desc = TRUE), " removed")
    gedcom <- remove_indi(gedcom, xref)
  }
  
  # remove children
  for(i in seq_along(chil_xref)) {
    gedcom <- remove_descendants(gedcom, chil_xref[i], TRUE, TRUE, remove_empty_families)
  }
  
  gedcom
}