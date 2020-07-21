
#' @export
add_submitter <- function(gedcom,
                          name,
                          address = ADDRESS_STRUCTURE(character()),
                          language_preference = character(),
                          submitter_registered_rfn = character(),
                          automated_record_id = character()) {
  
  xref <- assign_xref(xref_prefix_subm(), gedcom = gedcom)
  
  subm_record <- SUBMITTER_RECORD(xref_subm = xref,
                                  name,
                                  address,
                                  language_preference,
                                  submitter_registered_rfn,
                                  automated_record_id)
  
  gedcom %>% 
    tibble::add_row(subm_record, .before = nrow(.)) %>% 
    set_active_record(xref)
  
}

#' @export
update_submitter <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_subm(), is_submitter)
  
  
  
  
  
}

#' @export
remove_submitter <- function(gedcom) {
  
  check_active_record_valid(gedcom, record_string_subm(), is_submitter)
  
  
  
  null_active_record(gedcom)
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
  SUBMITTER_RECORD(assign_xref(xref_prefix_subm(), 1),
                   name,
                   address,
                   multimedia_links,
                   language_preference,
                   submitter_registered_rfn,
                   automated_record_id,
                   notes)
  
}
