
#' Constructs the ADDRESS_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(address_structure())
#' expect_error(address_structure(letters[1:5]))
#' expect_error(address_structure("address", address_city = 1:2))
#' expect_error(address_structure("address", address_state = 1:2))
#' expect_error(address_structure("address", address_postal_code = 1:2))
#' expect_error(address_structure("address", phone_number = 1:4))
#' expect_error(address_structure("address", address_email = 1:4))
#' expect_error(address_structure("address", address_fax = 1:4))
#' expect_error(address_structure("address", address_web_page = 1:4))
#' expect_error(address_structure(paste0(rep("a", 61), collapse = "")))
#' expect_error(address_structure("address", address_city = paste0(rep("a", 61), collapse = "")))
#' expect_error(address_structure("address", address_state = paste0(rep("a", 61), collapse = "")))
#' expect_error(address_structure("address", address_postal_code = paste0(rep("a", 11), collapse = "")))
#' expect_error(address_structure("address", address_country = paste0(rep("a", 61), collapse = "")))
#' expect_error(address_structure("address", address_web_page = paste0(rep("a", 121), collapse = "")))
#' expect_equal(address_structure("Road name"),
#'              tibble::tribble(
#'                              ~level,   ~tag,      ~value,
#'                              0, "ADDR", "Road name"
#'                              ))
#' expect_equal(address_structure(letters[1:4]),
#'              tibble::tribble(
#'                              ~level,   ~tag, ~value,
#'                              0, "ADDR",    "a",
#'                              1, "CONT",    "b",
#'                              1, "CONT",    "c",
#'                              1, "CONT",    "d",
#'                              1, "ADR1",    "b",
#'                              1, "ADR2",    "c",
#'                              1, "ADR3",    "d"
#'                              ))
#' expect_equal(address_structure(letters[1:2], address_country = "UK"),
#'              tibble::tribble(
#'                              ~level,   ~tag, ~value,
#'                              0, "ADDR",    "a",
#'                              1, "CONT",    "b",
#'                              1, "ADR1",    "b",
#'                              1, "CTRY",   "UK"
#'                              )) 
#' @return
#' @export
address_structure <- function(all_address_lines,
                              address_city = character(),
                              address_state = character(),
                              address_postal_code = character(),
                              address_country = character(),
                              phone_number = character(),
                              address_email = character(),
                              address_fax = character(),
                              address_web_page = character()) {
  
  if (length(all_address_lines) == 0) return(tibble())
  
  address_postal_code <- as.character(address_postal_code)
  phone_number <- as.character(phone_number)
  address_fax <- as.character(address_fax)
  
  validate_address_lines(all_address_lines, 4)
  validate_address_city(address_city, 1)
  validate_address_state(address_state, 1)
  validate_address_postal_code(address_postal_code, 1)
  validate_address_country(address_country, 1)
  validate_phone_number(phone_number, 3)
  validate_address_email(address_email, 3)
  validate_address_fax(address_fax, 3)
  validate_address_web_page(address_web_page, 3)
  
  address_lines_all <- tibble()
  
  # First populate the ADDR and CONT lines (mandatory)
  for (i in seq_along(all_address_lines)) {
      
    if (i == 1) {
      address_lines_all <- bind_rows(
        address_lines_all,
        tibble(level = 0, tag = "ADDR", value = all_address_lines[i])
      )
    } else {
      address_lines_all <- bind_rows(
        address_lines_all,
        tibble(level = 1, tag = "CONT", value = all_address_lines[i])
      )
    }
      
  }
  
  # Now populate ADR1/2/3 lines
  if (length(all_address_lines) > 1) {
    for (i in 2:length(all_address_lines)) {
      
      address_lines_all <- bind_rows(
        address_lines_all,
        tibble(level = 1, tag = paste0("ADR", i-1), value = all_address_lines[i])
      )
    }
  }
  
  bind_rows(
    address_lines_all,
    tibble(level = 1, tag = "CITY", value = address_city),
    tibble(level = 1, tag = "STAE", value = address_state),
    tibble(level = 1, tag = "POST", value = address_postal_code),
    tibble(level = 1, tag = "CTRY", value = address_country),
    tibble(level = 0, tag = "PHON", value = phone_number),
    tibble(level = 0, tag = "EMAIL", value = address_email),
    tibble(level = 0, tag = "FAX", value = address_fax),
    tibble(level = 0, tag = "WWW", value = address_web_page)
  )
  
  
}

#' Constructs the ASSOCIATION_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(association_structure())
#' expect_error(association_structure("@1@"))
#' expect_error(association_structure(c("@1@", "@2@"), "Godfather"))
#' expect_equal(association_structure("@I1@", "Godfather"),
#'              tibble::tribble(
#'                              ~level,   ~tag,      ~value,
#'                              0, "ASSO",      "@I1@",
#'                              1, "RELA", "Godfather"
#'                              ))
#' expect_equal(association_structure("@I1@", "Father", notes = list(note_structure("This is a note"))), 
#'              tibble::tribble(
#'                              ~level,   ~tag,              ~value,
#'                              0, "ASSO",              "@I1@",
#'                              1, "RELA",            "Father",
#'                              1, "NOTE",    "This is a note"
#'                              ))
#' @return
#' @export
association_structure <- function(xref_indi,
                                  relation_is_descriptor,
                                  source_citations = list(),
                                  notes = list()) {
  
  if (length(xref_indi) == 0) return(tibble())
  if (length(relation_is_descriptor) == 0) return(tibble())
  
  validate_xref(xref_indi, 1)
  validate_relation_is_descriptor(relation_is_descriptor, 1)
  
  bind_rows(
    tibble(level = 0, tag = "ASSO", value = xref_indi),
    tibble(level = 1, tag = "RELA", value = relation_is_descriptor),
    source_citations %>% bind_rows() %>% add_levels(1),
    notes %>% bind_rows() %>% add_levels(1)
  )
  
}

#' Constructs the CHANGE_DATE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_equal(change_date(),
#'              tibble::tribble(
#'                              ~level,   ~tag, ~value,
#'                              0, "CHAN", "",
#'                              1, "DATE", toupper(format(Sys.Date(), "%d %b %Y"))
#'                              ))
#' expect_equal(change_date(date_exact(5, 10, 1990)),
#'              tibble::tribble(
#'                              ~level,   ~tag, ~value,
#'                              0, "CHAN", "",
#'                              1, "DATE", "5 OCT 1990"
#'                              ))
#' expect_equal(change_date(date_exact(18, 12, 2008), time_value = "11:00:08.563")
#'              tibble::tribble(
#'                              ~level,   ~tag, ~value,
#'                              0, "CHAN", "",
#'                              1, "DATE", "18 DEC 2008",
#'                              2, "TIME", "11:00:08.563"
#'                              ))
#' expect_equal(change_date(date_exact(5, 10, 1990), "10:34:56", notes = list(note_structure("Note 1"),
#'                                                                            note_structure("Note 2"))),
#'              tibble::tribble(
#'                              ~level,   ~tag, ~value,
#'                              0, "CHAN", "",
#'                              1, "DATE", "5 OCT 1990",
#'                              2, "TIME", "10:34:56",
#'                              1, "NOTE", "Note 1",
#'                              1, "NOTE", "Note 2"
#'                              ))
#' @return
#' @export
change_date <- function(change_date = date_exact(),
                        time_value = character(),
                        notes = list()) {
  
  if (length(change_date) == 0) 
    change_date <- toupper(format(Sys.Date(), "%d %b %Y"))
  
  validate_change_date(change_date, 1)
  validate_time_value(time_value, 1)
  
  bind_rows(
    tibble(level = 0, tag = "CHAN", value = ""),
    tibble(level = 1, tag = "DATE", value = change_date),
    tibble(level = 2, tag = "TIME", value = time_value),
    notes %>% bind_rows() %>% add_levels(1)
  )
  
}

#' Constructs the CHILD_TO_FAMILY_LINK from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
child_to_family_link <- function(xref_fam,
                                pedigree_linkage_type = character(),
                                child_linkage_status = character(),
                                notes = list()) {
  
  if (length(xref_fam) == 0) return(tibble())
  
  validate_xref(xref_fam, 1)
  validate_pedigree_linkage_type(pedigree_linkage_type, 1)
  validate_child_linkage_status(child_linkage_status, 1)
  
  bind_rows(
    tibble(level = 0, tag = "FAMC", value = xref_fam),
    tibble(level = 1, tag = "PEDI", value = pedigree_linkage_type),
    tibble(level = 1, tag = "STAT", value = child_linkage_status),
    notes %>% bind_rows() %>% add_levels(1)
  )
  
}

#' Constructs the EVENT_DETAIL from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
event_detail <- function(event_or_fact_classification = character(),
                         date = date_value(),
                         place = place_structure(character()),
                         address = address_structure(character()),
                         responsible_agency = character(),
                         religious_affiliation = character(),
                         cause_of_event = character(),
                         restriction_notice = character(),
                         notes = list(),
                         source_citations = list(),
                         multimedia_links = list()) {
  
  validate_event_or_fact_classification(event_or_fact_classification, 1)
  validate_date_value(date, 1)
  validate_responsible_agency(responsible_agency, 1)
  validate_religious_affiliation(religious_affiliation, 1)
  validate_cause_of_event(cause_of_event, 1)
  validate_restriction_notice(restriction_notice, 1)
  
  bind_rows(
    tibble(level = 0, tag = "TYPE", value = event_or_fact_classification),
    tibble(level = 0, tag = "DATE", value = date),
    place %>% add_levels(0),
    address %>% add_levels(0),
    tibble(level = 0, tag = "AGNC", value = responsible_agency),
    tibble(level = 0, tag = "RELI", value = religious_affiliation),
    tibble(level = 0, tag = "CAUS", value = cause_of_event),
    tibble(level = 0, tag = "RESN", value = restriction_notice),
    notes %>% bind_rows() %>% add_levels(0),
    source_citations %>% bind_rows() %>% add_levels(0),
    multimedia_links %>% bind_rows() %>% add_levels(0)
  )
  
}

#' Constructs the FAMILY_EVENT_DETAIL from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
family_event_detail <- function(husband_age_at_event = character(),
                                wife_age_at_event = character(),
                                event_details = event_detail()) {
  
  husband_age_at_event <- as.character(husband_age_at_event)
  wife_age_at_event <- as.character(wife_age_at_event)
  
  validate_age_at_event(husband_age_at_event, 1)
  validate_age_at_event(wife_age_at_event, 1)
  
  temp = bind_rows(
    tibble(level = 0, tag = "HUSB", value = ""),
    tibble(level = 1, tag = "HAGE", value = husband_age_at_event),
    tibble(level = 0, tag = "WIFE", value = ""),
    tibble(level = 1, tag = "WAGE", value = wife_age_at_event),
    event_details %>% add_levels(0),
  )
  
  if (sum(temp$tag == "HAGE") == 0) temp <- filter(temp, tag != "HUSB")
  if (sum(temp$tag == "WAGE") == 0) temp <- filter(temp, tag != "WIFE")  
  mutate(temp,
         tag = if_else(tag == "HAGE", "AGE", tag),
         tag = if_else(tag == "WAGE", "AGE", tag))
  
}

#' Constructs the FAMILY_EVENT_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
family_event_structure <- function(event_type_family,
                                   event_descriptor = character(),
                                   family_event_details = family_event_detail()) {
  
  if (length(event_type_family) == 0) return(tibble())
  
  validate_event_type_family(event_type_family, 1)
  if (event_type_family == "RESI") validate_residence_descriptor(event_descriptor, 1)
  if (event_type_family == "EVEN") validate_event_descriptor(event_descriptor, 1)
  
  bind_rows(
    tibble(level = 0, tag = event_type_family, value = ""),
    family_event_details %>% add_levels(1),
  ) %>% 
  mutate(value = ifelse(tag %in% c("EVEN", "RESI") & length(event_descriptor) == 1,
                         event_descriptor,
                         value),
         value = ifelse(tag == "MARR", "Y", value))
  
}

#' Constructs the INDIVIDUAL_ATTRIBUTE_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
individual_attribute_structure <- function(attribute_type,
                                           attribute_description,
                                           individual_event_details = individual_event_detail()) {
  
  if (length(attribute_type) == 0) return(tibble())
  
  validate_attribute_type(attribute_type, 1)
  if (attribute_type %in% c("IDNO", "NCHI", "NMR", "SSN")) 
    attribute_description <- as.character(attribute_description) 
  
  if (attribute_type == "CAST") validate_caste_name(attribute_description, 1)
  if (attribute_type == "DSCR") validate_physical_description(attribute_description, 1)
  if (attribute_type == "EDUC") validate_scholastic_achievement(attribute_description, 1)
  if (attribute_type == "IDNO") validate_national_id_number(attribute_description, 1)
  if (attribute_type == "NATI") validate_national_or_tribal_origin(attribute_description, 1)
  if (attribute_type == "NCHI") validate_count_of_children(attribute_description, 1)
  if (attribute_type == "NMR") validate_count_of_marriages(attribute_description, 1)
  if (attribute_type == "OCCU") validate_occupation(attribute_description, 1)
  if (attribute_type == "PROP") validate_possessions(attribute_description, 1)
  if (attribute_type == "RELI") validate_religious_affiliation(attribute_description, 1)
  if (attribute_type == "RESI") validate_residence_descriptor(attribute_description, 1)
  if (attribute_type == "SSN") validate_social_security_number(attribute_description, 1)
  if (attribute_type == "TITL") validate_nobility_type_title(attribute_description, 1)
  if (attribute_type == "FACT") validate_attribute_descriptor(attribute_description, 1)
  
  if (attribute_type == "DSCR") {
    
    temp <- split_text(start_level = 0, top_tag = "DSCR", text = attribute_description)
    
  } else {
    
    temp <- tibble(level = 0, tag = attribute_type, value = attribute_description)
    
  }
  
  temp <- bind_rows(temp, 
            individual_event_details %>% add_levels(1)
  )
  
  if (sum(temp$tag %in% c("IDNO", "FACT")) == 1 & sum(temp$tag == "TYPE") == 0)
    stop("IDNO and FACT tags require a event_or_fact_classification to be defined in the event detail.")
  
  temp
}

#' Constructs the INDIVIDUAL_EVENT_DETAIL from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
individual_event_detail <- function(event_details = event_detail(),
                                    age_at_event = character()) {
  
  age_at_event <- as.character(age_at_event)
  
  validate_age_at_event(age_at_event, 1)
  
  bind_rows(
    event_details %>% add_levels(0),
    tibble(level = 0, tag = "AGE", value = age_at_event)
  )
  
  
}

#' Constructs the INDIVIDUAL_EVENT_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
individual_event_structure <- function(event_type_individual,
                                       individual_event_details = individual_event_detail(),
                                       xref_fam = character(),
                                       adopted_by_which_parent = character()) {
  
  if (length(event_type_individual) == 0) return(tibble())
  
  validate_event_type_individual(event_type_individual, 1)
  validate_xref(xref_fam, 1)
  validate_adopted_by_which_parent(adopted_by_which_parent, 1)
  
  temp <- bind_rows(
    tibble(level = 0, tag = event_type_individual, value = ""),
    individual_event_details %>% add_levels(1)
  )
    
  if (sum(temp$tag %in% c("BIRT", "CHR", "ADOP")) == 1)
    temp <- bind_rows(
      temp,
      tibble(level = 1, tag = "FAMC", value = xref_fam)
    )
  
  if (sum(temp$tag == "ADOP") == 1)
    temp <- bind_rows(
      temp,
      tibble(level = 2, tag = "ADOP", value = adopted_by_which_parent)
    )
    
  temp %>% 
    mutate(value = if_else(tag %in% c("BIRT", "CHR", "DEAT"), "Y", value))
  
}


lds_individual_ordinance <- function() {
  
  tibble()
}


lds_spouse_sealing <- function() {
  
  tibble()
}

#' Constructs the MULTIMEDIA_LINK from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
multimedia_link <- function(xref_obje = character(),
                            multimedia_file_refn = character(),
                            multimedia_format = character(),
                            source_media_type = character(),
                            descriptive_title = character()) {
  
  if (length(xref_obje) + length(multimedia_file_refn) == 0) return(tibble())
  
  if_else(str_detect(xref_obje, "@"),
          validate_input_size(file_ref, 1, 1, 18),
          validate_input_size(file_ref, 1000, 1, 30))
  # Is it one format per file?
  validate_input_size(media_format, 1)
  validate_input_size(media_type, 1)
  validate_input_size(title, 1, 1, 248)
  check_media_format(media_format)
  check_media_type(media_type)
  
  if (length(xref_obje) > 0) {
  
    tibble(level = 0, tag = "OBJE", value = xref_obje)
  
  } else {
    
    #file and form are needed
    if (length(multimedia_format) == 0) stop("Media format required")
    
    bind_rows(
      tibble(level = 0, tag = "OBJE", value = ""),
      tibble(level = 1, tag = "FILE", value = multimedia_file_refn),
      tibble(level = 2, tag = "FORM", value = multmedia_format),
      tibble(level = 3, tag = "MEDI", value = source_media_type),
      tibble(level = 1, tag = "TITL", value = descriptive_title)
    )
  }
}

#' Constructs the NOTE_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
note_structure <- function(notes) {
  
  if (length(notes) == 0) return(tibble())
  
  if (is.numeric(notes)) {
    
    validate_input_size(notes, 1, 1, 18)
    tibble(level = 0, tag = "NOTE", value = ref_to_xref(notes, "T"))
  
  } else {
  
    validate_input_size(notes, 1)
    split_text(start_level = 0, top_tag = "NOTE", text = notes)  
  }
  
}




#' Constructs the PERSONAL_NAME_PIECES from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
personal_name_pieces <- function(prefix = character(),
                                 given = character(), 
                                 nickname = character(), 
                                 surname_prefix = character(),
                                 surname = character(),
                                 suffix = character(),
                                 notes = list(),
                                 source_citations = list()) {
  
  validate_input_size(prefix, 1, 1, 30)
  validate_input_size(given, 1, 1, 120)
  validate_input_size(nickname, 1, 1, 30)
  validate_input_size(surname_prefix, 1, 1, 30)
  validate_input_size(surname, 1, 1, 120)
  validate_input_size(suffix, 1, 1, 30)
  
  bind_rows(
    tibble(level = 0, tag = "NPFX", value = prefix),
    tibble(level = 0, tag = "GIVN", value = given),
    tibble(level = 0, tag = "NICK", value = nickname),
    tibble(level = 0, tag = "SPFX", value = surname_prefix),
    tibble(level = 0, tag = "SURN", value = surname),
    tibble(level = 0, tag = "NSFX", value = suffix),
    notes %>% bind_rows() %>% add_levels(0),
    source_citations %>% bind_rows() %>% add_levels(0)
  ) 
  
}

#' Constructs the PERSONAL_NAME_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
personal_name_structure <- function(name,
                                    type = character(),
                                    name_pieces = personal_name_pieces(), 
                                    phonetic_variation = character(),
                                    phonetic_type = character(),
                                    phonetic_name_pieces = list(),
                                    romanized_variation = character(),
                                    romanized_type = character(),
                                    romanized_name_pieces = list()) {
  
  if (length(name) == 0) return(tibble())
  
  validate_input_size(name, 1, 1, 120)
  validate_input_size(type, 1, 5, 30)
  validate_input_size(phonetic_variation, 1000, 5, 120)
  validate_input_size(phonetic_type, 1000, 5, 30)
  validate_input_size(romanized_variation, 1000, 5, 120)
  validate_input_size(romanized_type, 1000, 5, 30)
  
  # ARE THESE NEEDED? - SPEC IS UNCLEAR
  if (length(phonetic_variation) != length(phonetic_type))
    stop("Each phonetic variation requires a phonetic type")
  if (length(phonetic_variation) != length(phonetic_name_pieces) &
      length(phonetic_name_pieces) > 0)
    stop("Each phonetic variation requires a set of phonetic name pieces")
  if (length(romanized_variation) != length(romanized_type))
    stop("Each romanized variation requires a romanized type")
  if (length(romanized_variation) != length(romanized_name_pieces) &
      length(romanized_name_pieces) > 0)
    stop("Each romanized variation requires a set of romanized name pieces")
  
  temp <- bind_rows(
    tibble(level = 0, tag = "NAME", value = name),
    tibble(level = 1, tag = "TYPE", value = type),
    name_pieces %>% add_levels(1)
  )
  
  for (i in seq_along(phonetic_variation)) {
    temp <- bind_rows(
      temp,
      tibble(level = 1, tag = "FONE", value = phonetic_variation[i]),
      tibble(level = 2, tag = "TYPE", value = phonetic_type[i])
    )
    if (length(phonetic_name_pieces) > 0)
      temp <- bind_rows(temp, phonetic_name_pieces[[i]] %>% add_levels(2))
  }
  for (i in seq_along(romanized_variation)) {
    temp <- bind_rows(
     temp,
     tibble(level = 1, tag = "ROMN", value = romanized_variation[i]),
     tibble(level = 2, tag = "TYPE", value = romanized_type[i])
    )
    if (length(romanized_name_pieces) > 0)
      temp <- bind_rows(temp, romanized_name_pieces[[i]] %>% add_levels(2))
  }
  
  temp
  
}

#' Constructs the PLACE_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
place_structure <- function(place,
                            place_hierarchy = character(),
                            phonetic_variation = character(),
                            phonetic_type = character(),
                            romanized_variation = character(),
                            romanized_type = character(),
                            latitude = character(),
                            longitude = character(),
                            notes = list()) {

  if (length(place) == 0) return(tibble())
  
  validate_input_size(place, 1, 1, 120)
  validate_input_size(place_hierarchy, 1, 1, 120)
  validate_input_size(phonetic_variation, 1000, 1, 120)
  validate_input_size(phonetic_type, 1000, 5, 30)
  validate_input_size(romanized_variation, 1000, 1, 120)
  validate_input_size(romanized_type, 1000, 5, 30)
  validate_input_size(latitude, 1, 5, 8)
  validate_input_size(longitude, 1, 5, 8)
  
  # ARE THESE NEEDED? - SPEC IS UNCLEAR
  if (length(phonetic_variation) != length(phonetic_type))
    stop("Each phonetic variation requires a phonetic type")
  if (length(romanized_variation) != length(romanized_type))
    stop("Each romanized variation requires a romanized type")
  
  temp <- bind_rows(
    tibble(level = 0, tag = "PLAC", value = place),
    tibble(level = 1, tag = "FORM", value = place_hierarchy)
    )
  
  for (i in seq_along(phonetic_variation)) {
    temp <- bind_rows(
      temp,
      tibble(level = 1, tag = "FONE", value = phonetic_variation[i]),
      tibble(level = 2, tag = "TYPE", value = phonetic_type[i])
    )
  }
  for (i in seq_along(romanized_variation)) {
    temp <- bind_rows(
      temp,
      tibble(level = 1, tag = "ROMN", value = romanized_variation[i]),
      tibble(level = 2, tag = "TYPE", value = romanized_type[i])
    )
  }
  
  temp <- bind_rows(
    temp,
    tibble(level = 1, tag = "MAP", value = ""),
    tibble(level = 2, tag = "LATI", value = latitude),
    tibble(level = 2, tag = "LONG", value = longitude),
    notes %>% bind_rows() %>% add_levels(1)
  )
  
  if (sum(temp$tag == "LATI") == 0) temp <- filter(temp, tag != "MAP")
  if (sum(temp$tag == "LONG") == 0) temp <- filter(temp, tag != "MAP")
  temp
  
}

#' Constructs the SOURCE_CITATION from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
source_citation <- function(source_ref,
                            page = character(),
                            event_type = character(),
                            role = character(),
                            entry_recording_date = character(),
                            source_text = character(),
                            certainty_assessment = character(),
                            multimedia_links = list(),
                            notes = list()) {
  
  if (length(source_ref) == 0) return(tibble())
  
  page <- as.character(page)
  certainty_assessment <- as.character(certainty_assessment)
  
  if_else(is.numeric(source_ref), 
          validate_input_size(source_ref, 1, 1, 18),
          validate_input_size(source_ref, 1))
  validate_input_size(page, 1, 1, 248)
  validate_input_size(event_type, 1, 1, 15)
  validate_input_size(role, 1, 1, 15)
  validate_input_size(entry_recording_date, 1, 1, 90)
  validate_input_size(certainty_assessment, 1)
  check_certainty_assessment(certainty_assessment)
  
  if (is.numeric(source_ref)) {
    
    temp <- bind_rows(
      tibble(level = 0, tag = "SOUR", value = ref_to_xref(source_ref, "S")),
      tibble(level = 1, tag = "PAGE", value = page),
      tibble(level = 1, tag = "EVEN", value = event_type),
      tibble(level = 2, tag = "ROLE", value = role),
      tibble(level = 1, tag = "DATA", value = ""),
      tibble(level = 1, tag = "DATE", value = entry_recording_date),
      split_text(start_level = 2, top_tag = "TEXT", text = source_text),
      multimedia_links %>% bind_rows() %>% add_levels(1),
      notes %>% bind_rows() %>% add_levels(1),
      tibble(level = 1, tag = "QUAY", value = certainty_assessment)
    ) 
    
    if (sum(temp$tag == "EVEN") == 0) temp <- filter(temp, tag != "ROLE")
    if (sum(temp$tag == "DATE") == 0 & sum(temp$tag == "TEXT") == 0) 
      temp <- filter(temp, tag != "DATA")
    
    temp
    
  } else {
    
    bind_rows(
      split_text(start_level = 0, top_tag = "SOUR", text = source_ref),
      split_text(start_level = 1, top_tag = "TEXT", text = source_text),
      multimedia_links %>% bind_rows() %>% add_levels(1),
      notes %>% bind_rows() %>% add_levels(1),
      tibble(level = 1, tag = "QUAY", value = certainty_assessment)
    )
    
  }
  
}

#' Constructs the SOURCE_REPOSITORY_CITATION from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
source_repository_citation <- function(repo_ref,
                                       notes = list(),
                                       call_numbers = character(),
                                       media_type = character()) {
  
  if (length(repo_ref) == 0) return(tibble())
  
  validate_input_size(repo_ref, 1, 1, 18)
  validate_input_size(call_numbers, 1000, 1, 120)
  validate_input_size(media_type, 1)
  check_media_type(media_type)
  # A MEDIA TYPE FOR EVERY CALL NUMBER?
  bind_rows(
    tibble(level = 0, tag = "REPO", value = ref_to_xref(repo_ref, "R")),
    notes %>% bind_rows() %>% add_levels(1),
    tibble(level = 1, tag = "CALN", value = call_numbers),
    tibble(level = 2, tag = "MEDI", value = media_type)
  )
  
}


#' Constructs the SPOUSE_TO_FAMILY_LINK from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' 
#' @return
#' @export
spouse_to_family_link <- function(family_ref,
                                  notes = list()) {
  
  if (length(family_ref) == 0) return(tibble())
  
  validate_input_size(family_ref, 1, 1, 18)
  
  bind_rows(
    tibble(level = 0, tag = "FAMS", value = ref_to_xref(family_ref, "F")),
    notes %>% bind_rows() %>% add_levels(1)
  )
}



