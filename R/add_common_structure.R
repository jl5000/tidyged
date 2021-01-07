

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
  
  if(is_individual(gedcom, xref)) {
    
    #if indi
    response2 <- utils::menu(c("The individual", "An association with an individual", "A family link", 
                        "An event", "An attribute", "A name", "A source citation"), 
                      title = "What should the note be attached to? (Select 0 to cancel)")
    
    if(response2 == 0) return(gedcom)
    
    if (response2 == 1) {
      # add the note  
    } else if(response2 == 2) {
      # get associations
      associations <- dplyr::filter(gedcom, record == xref, tag == "ASSO") %>% 
        dplyr::pull(value) %>% 
        purrr::map_chr(get_individual_name, gedcom = gedcom)
      
      if(length(associations) == 0) stop("Error: No associations found")
      
      response3 <- utils::menu(associations, title = "Which association should the note be attached to? (Select 0 to cancel)")
      
      # add note
      
    } else if(response2 == 3) {
      # get family links
      links <- dplyr::filter(gedcom, record == xref, tag %in% c("FAMS", "FAMC")) %>% 
        dplyr::pull(value)
      
      if(length(links) == 0) stop("Error: No family links found")
      
      response3 <- utils::menu(purrr::map_chr(links, get_family_group_description, gedcom=gedcom), 
                        title = "Which family link should the note be attached to?")
      
      
      
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
      sources <- dplyr::filter(gedcom, record == xref, tag == "SOUR") %>% 
        dplyr::pull(value)
      
      if(length(sources) == 0) stop("Error: No source citations found")
        
      response3 <- utils::menu(sources, title = "Which source citation should the note be attached to?")
     
      
           
    }
    
    
    
    
  } else if(is_family(gedcom, xref)) {
    
  } else if(is_multimedia(gedcom, xref)) {
    
  } else if(is_source(gedcom, xref)) {
    #source DATA?  
  } else if(is_repository(gedcom, xref)) {
    
  } else if(is_submitter(gedcom, xref)) {
    
  }
      
  #record
  
  #event_detail
  #name_pieces
  #place_structure
  #source_citation

    
}



# only indi, fam, multi, note
# asso, event detail, pers name pieces, 
add_source_citation_to_record <- function(gedcom) {
  
  xref <- get_active_record(gedcom)
  
  if(is.null(xref))
    stop("No record is activated. A record must be activated to add a source citation to it")
  
  if(is_repository(gedcom, xref) |
     is_source(gedcom, xref) |
     is_submitter(gedcom, xref))
    stop("Source citations cannot be added to the active record")
  
  
}

