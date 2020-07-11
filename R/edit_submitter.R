
#' @export
add_submitter <- function(gedcom,
                          name,
                          address = ADDRESS_STRUCTURE(character()),
                          multimedia_links = list(),
                          language_preference = character(),
                          submitter_registered_rfn = character(),
                          automated_record_id = character(),
                          notes = list()) {
  
  ref <- num_subm(gedcom) + 1
  
  gedcom %>% 
    tibble::add_row(SUBMITTER_RECORD(xref_subm = ref_to_xref(ref, "U"),
                                     name,
                                     address,
                                     multimedia_links,
                                     language_preference,
                                     submitter_registered_rfn,
                                     automated_record_id,
                                     notes),
                    .before(nrow(.)))
  
}

#' @export
update_submitter <- function(gedcom) {
  
}

#' @export
remove_submitter <- function(gedcom) {
  
}

#' @export
subm <- function(name = unname(Sys.info()["user"]),
                 address = ADDRESS_STRUCTURE(character()),
                 multimedia_links = list(),
                 language_preference = character(),
                 submitter_registered_rfn = character(),
                 automated_record_id = character(),
                 notes = list()) {
  
  #Shortcut used in gedcom function
  SUBMITTER_RECORD(ref_to_xref(1, "U"),
                   name,
                   address,
                   multimedia_links,
                   language_preference,
                   submitter_registered_rfn,
                   automated_record_id,
                   notes)
  
}
