
address_structure <- function(all_address_lines,
                              city = character(),
                              state = character(),
                              postal_code = character(),
                              country = character(),
                              phone_number = character(),
                              email = character(),
                              fax = character(),
                              web_page = character()) {
  
  # First populate the ADDR and CONT lines (mandatory)
  if (length(all_address_lines) > 4) stop("Address has too many lines.")
  address_lines_all <- tibble()
    
  for (i in seq_along(all_address_lines)) {
      
    if (i == 1) {
      address_lines_all <- bind_rows(
        address_lines_all,
        tibble(offset = 0, tag = "ADDR", value = all_address_lines[i])
      )
    } else {
      address_lines_all <- bind_rows(
        address_lines_all,
        tibble(offset = 1, tag = "CONT", value = all_address_lines[i])
      )
    }
      
  }
  
  # Now populate ADR1/2/3 lines
  if (length(all_address_lines) > 1) {
    for (i in 2:length(all_address_lines)) {
      
      address_lines_all <- bind_rows(
        address_lines_all,
        tibble(offset = 1, tag = paste0("ADR", i-1), value = all_address_lines[i])
      )
    }
  }
  
  bind_rows(
    address_lines_all,
    tibble(offset = 1, tag = "CITY", value = city),
    tibble(offset = 1, tag = "STAE", value = state),
    tibble(offset = 1, tag = "POST", value = postal_code),
    tibble(offset = 1, tag = "CTRY", value = country),
    tibble(offset = 0, tag = "PHON", value = phone_number),
    tibble(offset = 0, tag = "EMAIL", value = email),
    tibble(offset = 0, tag = "FAX", value = fax),
    tibble(offset = 0, tag = "WWW", value = web_page)
  )
  
  
}


association_structure <- function(individual_ref,
                                  relation_is,
                                  source_citations = source_citation(),
                                  notes = note_structure()) {
  
  bind_rows(
    tibble(offset = 0, tag = "ASSO", value = ref_to_xref(individual_ref, "I")),
    tibble(offset = 1, tag = "RELA", value = relation_is),
    source_citations %>% add_offsets(1),
    notes %>% add_offsets(1)
  )
  
}


change_date <- function(date,
                        time = character(),
                        notes = note_structure()) {
  
  bind_rows(
    tibble(offset = 0, tag = "CHAN", value = ""),
    tibble(offset = 1, tag = "DATE", value = date),
    tibble(offset = 2, tag = "TIME", value = time),
    notes %>% add_offsets(1)
  )
  
}


child_to_family_link <- function(family_ref,
                                pedigree_linkage_type = character(),
                                child_linkage_status = character(),
                                notes = note_structure()) {
  
  bind_rows(
    tibble(offset = 0, tag = "FAMC", value = ref_to_xref(family_ref, "F")),
    tibble(offset = 1, tag = "PEDI", value = pedigree_linkage_type),
    tibble(offset = 1, tag = "STAT", value = child_linkage_status),
    notes %>% add_offsets(1)
  )
  
}


event_detail <- function(event_classification = character(),
                         date = character(),
                         place = place_structure(),
                         address = address_structure(character()),
                         responsible_agency = character(),
                         religious_affiliation = character(),
                         cause_of_event = character(),
                         restriction_notice = character(),
                         notes = note_structure(),
                         source_citations = source_citation(),
                         multimedia_links = multimedia_link()) {
  
  bind_rows(
    tibble(offset = 0, tag = "TYPE", value = event_classification),
    tibble(offset = 0, tag = "DATE", value = date),
    place %>% add_offsets(0),
    address %>% add_offsets(0),
    tibble(offset = 0, tag = "AGNC", value = responsible_agency),
    tibble(offset = 0, tag = "RELI", value = religious_affiliation),
    tibble(offset = 0, tag = "CAUS", value = cause_of_event),
    tibble(offset = 0, tag = "RESN", value = restriction_notice),
    notes %>% add_offsets(0),
    source_citations %>% add_offsets(0),
    multimedia_links %>% add_offsets(0)
  )
  
}


family_event_detail <- function(husband_age = character(),
                                wife_age = character(),
                                event_detail = event_detail()) {
  
  temp = bind_rows(
    tibble(offset = 0, tag = "HUSB", value = ""),
    tibble(offset = 1, tag = "HAGE", value = husband_age),
    tibble(offset = 0, tag = "WIFE", value = ""),
    tibble(offset = 1, tag = "WAGE", value = wife_age),
    event_detail %>% add_offsets(0),
  )
  
  if (sum(temp$tag == "HAGE") == 0) temp <- filter(temp, tag != "HUSB")
  if (sum(temp$tag == "WAGE") == 0) temp <- filter(temp, tag != "WIFE")  
  mutate(temp,
         tag = if_else(tag == "HAGE", "AGE", tag),
         tag = if_else(tag == "WAGE", "AGE", tag))
  
}


family_event_structure <- function(family_event,
                                   family_event_detail = family_event_detail()) {
  
  bind_rows(
    tibble(offset = 0, tag = family_event, value = ""),
    family_event_detail %>% add_offsets(1),
  )
  
}


individual_attribute_structure <- function(individual_attribute,
                                           individual_event_detail = individual_event_detail()) {
  
  bind_rows(
    tibble(offset = 0, tag = individual_attribute, value = ""),
    individual_event_detail %>% add_offsets(1),
  )
  
}


individual_event_detail <- function(event_detail = event_detail(),
                                    age = character()) {
  bind_rows(
    event_detail %>% add_offsets(0),
    tibble(offset = 0, tag = "AGE", value = age),
  )
  
  
}


individual_event_structure <- function(individual_event,
                                       individual_event_detail = individual_event_detail(),
                                       family_ref = character(),
                                       adoptive_parent = character()) {
  
  bind_rows(
    tibble(offset = 0, tag = individual_event, value = ""),
    individual_event_detail %>% add_offsets(1),
    tibble(offset = 1, tag = "FAMC", value = ref_to_xref(family_ref, "F")),
    tibble(offset = 2, tag = "ADOP", value = adoptive_parent),
  )
  
}


lds_individual_ordinance <- function() {
  
  tibble()
}


lds_spouse_sealing <- function() {
  
  tibble()
}


multimedia_link <- function(file_reference,
                            media_format = character(),
                            media_type = character(),
                            title = character()) {
  
  if (1=1) {
    
    tibble(offset = 0, tag = "OBJE", value = ref_to_xref(file_reference, "M"))
  
  } else {
    
    bind_rows(
      tibble(offset = 0, tag = "OBJE", value = ""),
      tibble(offset = 1, tag = "FILE", value = file_reference),
      tibble(offset = 2, tag = "FORM", value = media_format),
      tibble(offset = 3, tag = "MEDI", value = media_type),
      tibble(offset = 1, tag = "TITL", value = title)
    )
  }
}

note_structure <- function(notes) {
  
  if (notes = character()) return(tibble())
  
  notes_split <- split_notes(notes)
  
  tibble(offset = 1, tag = "CONC", value = notes_split) 
  
}





personal_name_pieces <- function(prefix = character(),
                                 given = character(), 
                                 nickname = character(), 
                                 surname_prefix = character(),
                                 surname = character(),
                                 suffix = character(),
                                 notes = note_structure(),
                                 source_citations = source_citation()) {
  
  bind_rows(
    tibble(offset = 0, tag = "NPFX", value = prefix),
    tibble(offset = 0, tag = "GIVN", value = given),
    tibble(offset = 0, tag = "NICK", value = nickname),
    tibble(offset = 0, tag = "SPFX", value = surname_prefix),
    tibble(offset = 0, tag = "SURN", value = surname),
    tibble(offset = 0, tag = "NSFX", value = suffix),
    notes %>% add_offsets(0),
    source_citations %>% add_offsets(0)
  ) 
  
}


personal_name_structure <- function(name,
                                    type = character(),
                                    name_pieces = personal_name_pieces(), 
                                    phonetic_variation = character(),
                                    phonetic_type = character(),
                                    phonetic_name_pieces = personal_name_pieces(),
                                    romanized_variation = character(),
                                    romanized_type = character(),
                                    romanized_name_pieces = personal_name_pieces()) {
  
  bind_rows(
    tibble(offset = 0, tag = "NAME", value = name),
    tibble(offset = 1, tag = "TYPE", value = type),
    name_pieces %>% add_offsets(1),
    tibble(offset = 1, tag = "FONE", value = phonetic_variation),
    tibble(offset = 2, tag = "TYPE", value = phonetic_type),
    phonetic_name_pieces %>% add_offsets(2),
    tibble(offset = 1, tag = "ROMN", value = romanized_variation),
    tibble(offset = 2, tag = "TYPE", value = romanized_type),
    romanized_name_pieces %>% add_offsets(2)
  ) 
  
}


place_structure <- function(place,
                            place_hierarchy = character(),
                            phonetic_variation = character(),
                            phonetic_type = character(),
                            romanized_variation = character(),
                            romanized_type = character(),
                            latitude = character(),
                            longitude = character(),
                            notes = note_structure()) {

  bind_rows(
    tibble(offset = 0, tag = "PLAC", value = place),
    tibble(offset = 1, tag = "FORM", value = place_hierarchy),
    tibble(offset = 1, tag = "FONE", value = phonetic_variation),
    tibble(offset = 2, tag = "TYPE", value = phonetic_type),
    tibble(offset = 1, tag = "ROMN", value = romanized_variation),
    tibble(offset = 2, tag = "TYPE", value = romanized_type),
    tibble(offset = 1, tag = "MAP", value = ""),
    tibble(offset = 2, tag = "LATI", value = latitude),
    tibble(offset = 2, tag = "LONG", value = longitude),
    notes %>% add_offsets(1)
  )
  
}


source_citation <- function(source_ref,
                            page = character(),
                            event_type = character(),
                            role = character(),
                            entry_recording_date = character(),
                            source_text = character(),
                            certainty_assessment = character(),
                            notes = note_structure()) {
  
  if (1=1) {
    
    bind_rows(
      tibble(offset = 0, tag = "SOUR", value = ref_to_xref(source_ref, "S"))
    )
    
  } else {
    
    bind_rows(
      tibble(offset = 0, tag = "SOUR", value = source_ref)
    )
    
  }
  
  tibble(offset = 1, tag = "", value = character())
}


source_repository_citation <- function(repo_ref,
                                       notes = note_structure(),
                                       call_numbers = character(),
                                       media_type = character()) {
  
  bind_rows(
    tibble(offset = 0, tag = "REPO", value = ref_to_xref(repo_ref, "R")),
    notes %>% add_offsets(1),
    tibble(offset = 1, tag = "CALN", value = call_numbers),
    tibble(offset = 2, tag = "MEDI", value = media_type)
  )
  
}



spouse_to_family_link <- function(family_ref,
                                  notes = note_structure()) {
  
  bind_rows(
    tibble(offset = 0, tag = "FAMS", value = ref_to_xref(family_ref, "F")),
    notes %>% add_offsets(1)
  )
}



