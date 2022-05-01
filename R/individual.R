


#' Add an Individual record to a tidyged object
#'
#' @details If you need to add further information about this individual (e.g. names), use the 
#' add_indi_* functions.
#' 
#' @param gedcom A tidyged object.
#' @param sex The sex of the individual. Either "M" (male), "F" (female), "U" (undetermined),
#' "X" (intersex), or "N" (not recorded).
#' @param user_reference_numbers A unique user-defined number or text that the submitter 
#' uses to identify this record. You can supply more than one in a vector. You can also define a
#' user reference type by using a named vector (e.g c(id1 = "123A", id2 = "456B")).
#' @param indi_notes A character vector of notes accompanying this Individual record.
#' These could be xrefs to existing Note records.
#' @param multimedia_links A character vector of Multimedia record xrefs accompanying this 
#' record.
#' @param qn A shortcut to quickly define a name for this individual. This is a shortcut for
#' the add_indi_names() function (which you should really use instead), but this is useful
#' for quick demonstrations or tests.
#'
#' @return An updated tidyged object including the Individual record.
#' @export
#' @tests
#' expect_snapshot_value(add_indi(gedcom(subm("Me")),
#'                                      sex = "M", user_reference_number = c(something = 1234),
#'                                      indi_notes = c("Note1", "Note 2")) |> 
#'                        remove_dates_for_tests(), "json2")
add_indi <- function(gedcom,
                     sex = "U",
                     user_reference_numbers = character(),
                     indi_notes = character(),
                     multimedia_links = character(),
                     qn = character()) {
  
  xref <- tidyged.internals::assign_xref_indi(gedcom)
  
  indiv_notes <- create_note_structures(gedcom, indi_notes)
  media_links <- create_multimedia_links(gedcom, multimedia_links)

  ind_record <- tidyged.internals::INDIVIDUAL_RECORD(xref_indi = xref,
                                                     sex_value = sex,
                                                     user_reference_number = user_reference_numbers,
                                                     notes = indiv_notes,
                                                     multimedia_links = media_links) 
  
  temp <- gedcom |>
    tibble::add_row(ind_record, .before = nrow(gedcom)) |> 
    set_active_record(xref)
  
  if (length(qn) > 0) 
    temp <- add_indi_names(temp, name_pieces(given = qn))
  
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
#' other individual records. Defaults to TRUE. You shouldn't really leave dead links to
#' individual records that no longer exist.
#'
#' @return An updated tidyged object excluding the selected Individual record.
#' 
#' @export
#' @tests
#' expect_equal(gedcom(subm()),
#'              gedcom(subm()) |> add_indi() |> remove_indi())
remove_indi <- function(gedcom, 
                        individual = character(),
                        remove_associations = TRUE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  if(remove_associations) gedcom <- tidyged.internals::remove_section(gedcom, 1, "ASSO", xref)

  gedcom |> 
    dplyr::filter(record != xref, !(value == xref & tag != "ASSO")) |>
    null_active_record()
}

