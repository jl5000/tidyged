

set_active_record <- function(gedcom, xref) {
  attr(gedcom, "active_record") <- xref
  gedcom
}


find_xref <- function(gedcom, record_ids, tags, search_string, allow_broken = FALSE) {
  
  if(allow_broken) search_string <- stringr::str_replace_all(search_string, " ", ".*")
  
  possibilities <- gedcom %>% 
    dplyr::filter(id %in% record_ids) %>% 
    dplyr::filter(tag %in% tags) %>% 
    dplyr::filter(stringr::str_detect(value, search_string))
  
  if(length(unique(possibilities$id)) == 0) {
    
    stop("Record activation failed - no records found. Try supplying the xref explicitly.")
    
  } else if(length(unique(possibilities$id)) > 1) {
    
    stop("Record activation failed - more than one record found: ",
         paste(unique(possibilities$value), collapse = ", "),
         ". \nTry being more specific or supplying the xref explicitly.")
    
  } else if(length(unique(possibilities$id)) == 1) {
    
    xref <- unique(possibilities$id)
  }
  
}

activate_individual_record <- function(gedcom, 
                                       individual_name = character(),
                                       xref = character()) {
  
  if(length(xref) == 0) {
    
    individuals <- dplyr::filter(gedcom, level == 0 & tag == "INDI")$id
    
    xref <- find_xref(gedcom, 
                      individuals, 
                      c("NAME", "ROMN", "FONE"), 
                      individual_name,
                      allow_broken = TRUE) 
    
  }
  
  set_active_record(gedcom, xref)
}


activate_family_record <- function(gedcom, 
                                   wife_name = character(),
                                   husband_name = character(),
                                   child_name = character(),
                                   xref = character()) {
  
  if(length(xref) == 0) {
    
    families <- dplyr::filter(gedcom, level == 0 & tag == "FAM")$id
    
    xref <- find_xref(gedcom, 
                      families, 
                      c("HUSB", "WIFE", "CHIL"), 
                      individual_name, #TODO
                      allow_broken = TRUE) 
    
    
    
  }
  
  
  set_active_record(gedcom, xref)
}


activate_submitter_record <- function(gedcom, 
                                      submitter_name = character(),
                                      xref = character()) {
  
  if(length(xref) == 0) {
    
    submitters <- dplyr::filter(gedcom, level == 0 & tag == "SUBM")$id
    
    xref <- find_xref(gedcom, 
                      submitters, 
                      "NAME", 
                      submitter_name,
                      allow_broken = TRUE) 
    
    
    
  }
  
  
  set_active_record(gedcom, xref)
}


activate_multimedia_record <- function(gedcom, 
                                       file_reference = character(),
                                       xref = character()) {
  
  if(length(xref) == 0) {
    
    objects <- dplyr::filter(gedcom, level == 0 & tag == "OBJE")$id
    
    xref <- find_xref(gedcom, 
                      objects, 
                      "FILE", 
                      file_reference,
                      allow_broken = FALSE) 
    
    
    
  }
  
  
  set_active_record(gedcom, xref)
}



activate_note_record <- function(gedcom, 
                                 note_excerpt = character(),
                                 xref = character()) {
  
  if(length(xref) == 0) {
    
    notes <- dplyr::filter(gedcom, level == 0 & tag == "NOTE")$id
    
    xref <- find_xref(gedcom, 
                      notes, 
                      c("NOTE", "CONT", "CONC"), 
                      note_excerpt,
                      allow_broken = FALSE) 
    
    
    
  }
  
  
  set_active_record(gedcom, xref)
}



activate_source_record <- function(gedcom, 
                                   source_title = character(),
                                   xref = character()) {
  
  if(length(xref) == 0) {
    
    sources <- dplyr::filter(gedcom, level == 0 & tag == "SOUR")$id
    
    xref <- find_xref(gedcom, 
                      sources, 
                      "TITL", 
                      source_title,
                      allow_broken = FALSE) 
    
    
    
  }
  
  
  set_active_record(gedcom, xref)
}


activate_repository_record <- function(gedcom, 
                                       repository_name = character(),
                                       xref = character()) {
  
  if(length(xref) == 0) {
    
    repositories <- dplyr::filter(gedcom, level == 0 & tag == "REPO")$id
    
    xref <- find_xref(gedcom, 
                      repositories, 
                      "NAME", 
                      repository_name,
                      allow_broken = FALSE) 
    
    
    
  }
  
  
  set_active_record(gedcom, xref)
}
