

add_note_to_record <- function(gedcom, note) {
  
  xref <- get_active_record(gedcom)
  
  if(is.null(xref))
    stop("No record is activated. A record must be activated to add a note to it")
  
  if(is_note(gedcom, xref))
    stop("Notes cannot be added to the active record")
  
  if(grepl(xref_pattern(), note)) note_text <- gedcom_value(gedcom, note, "NOTE", 0)
  
  response1 <- utils::menu(c("No", "Yes", "Absolutely!"), 
                     title = paste("You are about the add the following note to the record:", note,
                                   ifelse(grepl(xref_pattern(), note), paste0("(", note_text ,")"), "")))
                    
  if(response1 %in% 0:1) return(gedcom)
  
  if(is_indi(gedcom, xref)) {
    
    #if indi
    response2 <- utils::menu(c("The individual", "An association with an individual", "A family link", 
                        "An event", "An attribute", "A name", "A source citation"), 
                      title = "What should the note be attached to? (Select 0 to cancel)")
    
    if(response2 == 0) return(gedcom)
    
    if (response2 == 1) {
      # the individual
      next_row <- find_insertion_point(gedcom, xref, 0, "INDI") 
      note_str <- tidyged.internals::NOTE_STRUCTURE(note) %>%
        tidyged.internals::add_levels(1)
        
    } else if(response2 == 2) {
      # get associations
      asso_xrefs <- dplyr::filter(gedcom, record == xref, tag == "ASSO") %>% 
        dplyr::pull(value)
      
      if(length(asso_xrefs) == 0) stop("Error: No associations found")
      
      asso_names <- purrr::map_chr(asso_xrefs, describe_indi, gedcom = gedcom, name = TRUE)
      
      response3 <- utils::menu(asso_names, title = "Which association should the note be attached to? (Select 0 to cancel)")
      
      if(response3 == 0) return(gedcom)
      
      next_row <- find_insertion_point(gedcom, xref, 1, "ASSO", asso_xrefs[response3]) 
      note_str <- tidyged.internals::NOTE_STRUCTURE(note) %>%
        tidyged.internals::add_levels(2)
      
    } else if(response2 == 3) {
      # get family links
      fams <- dplyr::filter(gedcom, record == xref, tag %in% c("FAMS", "FAMC")) 
      fam_xrefs <- dplyr::pull(fams, value)
      link_types <- dplyr::pull(fams, tag)
      
      if(length(fam_xrefs) == 0) stop("Error: No family links found")
      
      fam_desc <- purrr::map_chr(fam_xrefs, describe_famg, gedcom=gedcom) %>% 
        paste0(" (link as a ", ifelse(link_types == "FAMS", "spouse", "child"), ")")
      
      response3 <- utils::menu(fam_desc, title = "Which family link should the note be attached to?")
      
      if(response3 == 0) return(gedcom)
      
      next_row <- find_insertion_point(gedcom, xref, 1, link_types[response3], fam_xrefs[response3]) 
      note_str <- tidyged.internals::NOTE_STRUCTURE(note) %>%
        tidyged.internals::add_levels(2)
      
    } else if(response2 == 4) {
      # get events
      events <- ""
      if(length(events) == 0) stop("Error: No events found")
      
      response3 <- utils::menu(events, title = "Which event should the note be attached to? (Select 0 to cancel)")
    } else if(response2 == 5) {
      # get attributes
      attributes <- ""
      if(length(attributes) == 0) stop("Error: No attributes found")
      
      response3 <- utils::menu(attributes, title = "Which attribute should the note be attached to? (Select 0 to cancel)")
    } else if(response2 == 6) {
      # get names
      names <- dplyr::filter(gedcom, record == xref, tag %in% c("NAME", "FONE", "ROMN")) %>% 
        dplyr::pull(value)
      
      if(length(names) == 0) stop("Error: No names found")
      
      response3 <- utils::menu(names, title = "Which name should the note be attached to?")
      
    } else if(response2 == 7) {
      # get source citations
      sour_xrefs <- dplyr::filter(gedcom, record == xref, tag == "SOUR") %>% 
        dplyr::pull(value)
      
      if(length(sour_xrefs) == 0) stop("Error: No source citations found")
      
      sour_titles <- purrr::map_chr(sour_xrefs, describe_source, gedcom = gedcom)
        
      response3 <- utils::menu(sour_titles, title = "Which source citation should the note be attached to?")
     
      if(response3 == 0) return(gedcom)
      
      next_row <- find_insertion_point(gedcom, xref, 1, "SOUR", sour_xrefs[response3]) 
      note_str <- tidyged.internals::NOTE_STRUCTURE(note) %>%
        tidyged.internals::add_levels(2)
           
    }
    
    
    
    
  } else if(is_famg(gedcom, xref)) {
    
  } else if(is_media(gedcom, xref)) {
    
  } else if(is_sour(gedcom, xref)) {
    #source DATA?  
  } else if(is_repo(gedcom, xref)) {
    
  } else if(is_subm(gedcom, xref)) {
    
  }
      
  #record
  
  #event_detail
  #name_pieces
  #place_structure
  #source_citation
  gedcom %>% 
    tibble::add_row(note_str, .before = next_row) %>% 
    tidyged.internals::finalise() %>% 
    activate_indi(xref)
  
}




