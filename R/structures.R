
#' Constructs the ADDRESS_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @param all_address_lines The address lines usually contain the addressee’s street and city 
#' information so that it forms an address that meets mailing requirements.
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
#' 
#' expect_equal(address_structure("Road name"),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "ADDR", "Road name"
#'              ))
#' 
#' expect_equal(address_structure(letters[1:4]),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "ADDR",    "a",
#'                              1, "CONT",    "b",
#'                              1, "CONT",    "c",
#'                              1, "CONT",    "d",
#'                              1, "ADR1",    "b",
#'                              1, "ADR2",    "c",
#'                              1, "ADR3",    "d"
#'              ))
#' 
#' expect_equal(address_structure(letters[1:2], address_country = "UK"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "ADDR",    "a",
#'                              1, "CONT",    "b",
#'                              1, "ADR1",    "b",
#'                              1, "CTRY",   "UK"
#'              )) 
#' @return A tidy tibble containing the ADDRESS_STRUCTURE part of a GEDCOM file
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
  
  if (length(all_address_lines) == 0) return(tibble::tibble())
  
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
  
  address_lines_all <- tibble::tibble()
  
  # First populate the ADDR and CONT lines (mandatory)
  for (i in seq_along(all_address_lines)) {
      
    if (i == 1) {
      address_lines_all <- dplyr::bind_rows(
        address_lines_all,
        tibble::tibble(level = 0, tag = "ADDR", value = all_address_lines[i])
      )
    } else {
      address_lines_all <- dplyr::bind_rows(
        address_lines_all,
        tibble::tibble(level = 1, tag = "CONT", value = all_address_lines[i])
      )
    }
      
  }
  
  # Now populate ADR1/2/3 lines
  if (length(all_address_lines) > 1) {
    for (i in 2:length(all_address_lines)) {
      
      address_lines_all <- dplyr::bind_rows(
        address_lines_all,
        tibble::tibble(level = 1, tag = paste0("ADR", i-1), value = all_address_lines[i])
      )
    }
  }
  
  dplyr::bind_rows(
    address_lines_all,
    tibble::tibble(level = 1, tag = "CITY", value = address_city),
    tibble::tibble(level = 1, tag = "STAE", value = address_state),
    tibble::tibble(level = 1, tag = "POST", value = address_postal_code),
    tibble::tibble(level = 1, tag = "CTRY", value = address_country),
    tibble::tibble(level = 0, tag = "PHON", value = phone_number),
    tibble::tibble(level = 0, tag = "EMAIL", value = address_email),
    tibble::tibble(level = 0, tag = "FAX", value = address_fax),
    tibble::tibble(level = 0, tag = "WWW", value = address_web_page)
  )
  
  
}

#' Constructs the ASSOCIATION_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(association_structure())
#' expect_error(association_structure("@1@"))
#' expect_error(association_structure(c("@1@", "@2@"), "Godfather"))
#' 
#' expect_equal(association_structure("@I1@", "Godfather"),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "ASSO",      "@I1@",
#'                              1, "RELA", "Godfather"
#'              ))
#' 
#' expect_equal(association_structure("@I1@", "Father", 
#'                                    notes = list(note_structure(submitter_text = "This is a note"))), 
#'              tibble::tribble(~level,   ~tag,              ~value,
#'                              0, "ASSO",              "@I1@",
#'                              1, "RELA",            "Father",
#'                              1, "NOTE",    "This is a note"
#'              ))
#' @return A tidy tibble containing the ASSOCIATION_STRUCTURE part of a GEDCOM file
#' @export
association_structure <- function(xref_indi,
                                  relation_is_descriptor,
                                  source_citations = list(),
                                  notes = list()) {
  
  if (length(xref_indi) == 0) return(tibble::tibble())
  if (length(relation_is_descriptor) == 0) return(tibble::tibble())
  
  validate_xref(xref_indi, 1)
  validate_relation_is_descriptor(relation_is_descriptor, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "ASSO", value = xref_indi),
    tibble::tibble(level = 1, tag = "RELA", value = relation_is_descriptor),
    source_citations %>% dplyr::bind_rows() %>% add_levels(1),
    notes %>% dplyr::bind_rows() %>% add_levels(1)
  )
  
}

#' Constructs the CHANGE_DATE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_equal(change_date(),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "CHAN", "",
#'                              1, "DATE", toupper(format(Sys.Date(), "%d %b %Y"))
#'              ))
#'                              
#' expect_equal(change_date(date_exact(5, 10, 1990)),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "CHAN", "",
#'                              1, "DATE", "5 OCT 1990"
#'              ))
#'                              
#' expect_equal(change_date(date_exact(18, 12, 2008), time_value = "11:00:08.563"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "CHAN", "",
#'                              1, "DATE", "18 DEC 2008",
#'                              2, "TIME", "11:00:08.563"
#'              ))
#'                              
#' expect_equal(change_date(date_exact(5, 10, 1990), "10:34:56", 
#'                          notes = list(note_structure(submitter_text = "Note 1"),
#'                                       note_structure(submitter_text = "Note 2"))),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "CHAN", "",
#'                              1, "DATE", "5 OCT 1990",
#'                              2, "TIME", "10:34:56",
#'                              1, "NOTE", "Note 1",
#'                              1, "NOTE", "Note 2"
#'              ))
#' @return A tidy tibble containing the CHANGE_DATE part of a GEDCOM file
#' @export
change_date <- function(change_date = date_exact(),
                        time_value = character(),
                        notes = list()) {
  
  if (length(change_date) == 0) 
    change_date <- toupper(format(Sys.Date(), "%d %b %Y"))
  
  validate_change_date(change_date, 1)
  validate_time_value(time_value, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "CHAN", value = ""),
    tibble::tibble(level = 1, tag = "DATE", value = change_date),
    tibble::tibble(level = 2, tag = "TIME", value = time_value),
    notes %>% dplyr::bind_rows() %>% add_levels(1)
  )
  
}

#' Constructs the CHILD_TO_FAMILY_LINK from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(child_to_family_link())
#' expect_error(child_to_family_link("@1@", pedigree_linkage_type = "foste"))
#' expect_error(child_to_family_link("@1@", child_linkage_status = "challenge"))
#' 
#' expect_equal(child_to_family_link("@F1@"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "FAMC", "@F1@"
#'              ))
#' 
#' expect_equal(child_to_family_link("@F1@", "birth", "proven"),
#'              tibble::tribble(~level,   ~tag,   ~value,
#'                              0, "FAMC",   "@F1@",
#'                              1, "PEDI",  "birth",
#'                              1, "STAT", "proven"
#'              ))
#' @return A tidy tibble containing the CHILD_TO_FAMILY_LINK part of a GEDCOM file
#' @export
child_to_family_link <- function(xref_fam,
                                 pedigree_linkage_type = character(),
                                 child_linkage_status = character(),
                                 notes = list()) {
  
  if (length(xref_fam) == 0) return(tibble::tibble())
  
  validate_xref(xref_fam, 1)
  validate_pedigree_linkage_type(pedigree_linkage_type, 1)
  validate_child_linkage_status(child_linkage_status, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "FAMC", value = xref_fam),
    tibble::tibble(level = 1, tag = "PEDI", value = pedigree_linkage_type),
    tibble::tibble(level = 1, tag = "STAT", value = child_linkage_status),
    notes %>% dplyr::bind_rows() %>% add_levels(1)
  )
  
}

#' Constructs the EVENT_DETAIL from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @param date A date_value() object giving the date of the event.
#' @param place A place_structure() object giving the location of the event.
#' @param address An address_structure() object giving the address of the event.
#' @tests
#' expect_error(event_detail(restriction_notice = "something"))
#' expect_equal(dim(event_detail()), c(0, 3))
#' 
#' expect_equal(event_detail(event_or_fact_classification = "Woodworking"),
#'              tibble::tribble(~level,   ~tag,        ~value,
#'                              0, "TYPE", "Woodworking"
#'              ))
#' 
#' expect_equal(event_detail(place = place_structure("Somewhere")),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "PLAC", "Somewhere"
#'              ))
#' 
#' expect_equal(event_detail(place = place_structure("Somewhere"), 
#'                           date = date_value(2008, 4, 9, about = TRUE)),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "DATE", "ABT 9 APR 2008",
#'                              0, "PLAC", "Somewhere"
#'              ))
#' 
#' expect_equal(event_detail(address = address_structure(c("House name", "Road"))),
#'              tibble::tribble(~level,   ~tag,       ~value,
#'                              0, "ADDR", "House name",
#'                              1, "CONT",       "Road",
#'                              1, "ADR1",       "Road"
#'              ))
#' @return A tidy tibble containing the EVENT_DETAIL part of a GEDCOM file
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
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "TYPE", value = event_or_fact_classification),
    tibble::tibble(level = 0, tag = "DATE", value = date),
    place %>% add_levels(0),
    address %>% add_levels(0),
    tibble::tibble(level = 0, tag = "AGNC", value = responsible_agency),
    tibble::tibble(level = 0, tag = "RELI", value = religious_affiliation),
    tibble::tibble(level = 0, tag = "CAUS", value = cause_of_event),
    tibble::tibble(level = 0, tag = "RESN", value = restriction_notice),
    notes %>% dplyr::bind_rows() %>% add_levels(0),
    source_citations %>% dplyr::bind_rows() %>% add_levels(0),
    multimedia_links %>% dplyr::bind_rows() %>% add_levels(0)
  )
  
}

#' Constructs the FAMILY_EVENT_DETAIL from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @param event_details An event_detail() object giving details of the event.
#' @tests
#' expect_equal(dim(family_event_detail()), c(0, 3))  
#' 
#' expect_equal(family_event_detail(husband_age_at_event = "42y"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "HUSB",     "",
#'                              1,  "AGE",   "42y"
#'              ))
#' 
#' expect_equal(family_event_detail(wife_age_at_event = "40y"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "WIFE",     "",
#'                              1,  "AGE",   "40y"
#'              ))
#' 
#' expect_equal(family_event_detail(husband_age_at_event = "42y", wife_age_at_event = "40y"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "HUSB",     "",
#'                              1,  "AGE",   "42y",
#'                              0, "WIFE",     "",
#'                              1,  "AGE",   "40y"
#'              ))
#' @return A tidy tibble containing the FAMILY_EVENT_DETAIL part of a GEDCOM file
#' @export
family_event_detail <- function(husband_age_at_event = character(),
                                wife_age_at_event = character(),
                                event_details = event_detail()) {
  
  husband_age_at_event <- as.character(husband_age_at_event)
  wife_age_at_event <- as.character(wife_age_at_event)
  
  validate_age_at_event(husband_age_at_event, 1)
  validate_age_at_event(wife_age_at_event, 1)
  
  temp = dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "HUSB", value = ""),
    tibble::tibble(level = 1, tag = "HAGE", value = husband_age_at_event),
    tibble::tibble(level = 0, tag = "WIFE", value = ""),
    tibble::tibble(level = 1, tag = "WAGE", value = wife_age_at_event),
    event_details %>% add_levels(0),
  )
  
  if (sum(temp$tag == "HAGE") == 0) temp <- dplyr::filter(temp, tag != "HUSB")
  if (sum(temp$tag == "WAGE") == 0) temp <- dplyr::filter(temp, tag != "WIFE")  
  dplyr::mutate(temp,
                tag = if_else(tag == "HAGE", "AGE", tag),
                tag = if_else(tag == "WAGE", "AGE", tag))
  
}

#' Constructs the FAMILY_EVENT_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @param family_event_details A family_event_detail() object giving details of the event.
#' @tests
#' expect_error(family_event_structure())
#' expect_error(family_event_structure("TEST"))
#' 
#' expect_equal(family_event_structure("CENS"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "CENS",     ""
#'              ))
#' 
#' expect_equal(family_event_structure("EVEN"), 
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "EVEN",     ""
#'              ))
#' 
#' expect_equal(family_event_structure("EVEN", "Random event"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "EVEN",     "Random event"
#'              ))
#' 
#' expect_equal(family_event_structure("MARR", 
#'                                     family_event_details = family_event_detail(wife_age_at_event = "20y")),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "MARR",     "Y",
#'                              1, "WIFE",     "",
#'                              2, "AGE",   "20y"
#'              ))
#' @return A tidy tibble containing the FAMILY_EVENT_STRUCTURE part of a GEDCOM file
#' @export
family_event_structure <- function(event_type_family,
                                   event_descriptor = character(),
                                   family_event_details = family_event_detail()) {
  
  if (length(event_type_family) == 0) return(tibble::tibble())
  
  validate_event_type_family(event_type_family, 1)
  if (event_type_family == "RESI") validate_residence_descriptor(event_descriptor, 1)
  if (event_type_family == "EVEN") validate_event_descriptor(event_descriptor, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, tag = event_type_family, value = ""),
    family_event_details %>% add_levels(1),
  ) %>% 
  dplyr::mutate(value = ifelse(tag %in% c("EVEN", "RESI") & length(event_descriptor) == 1,
                         event_descriptor,
                         value),
                value = ifelse(tag == "MARR", "Y", value))
  
}

#' Constructs the INDIVIDUAL_ATTRIBUTE_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @param individual_event_details An individual_event_detail() object giving details of the attribute.
#' @tests
#' expect_error(individual_attribute_structure())
#' expect_error(individual_attribute_structure("TEST"))
#' expect_error(individual_attribute_structure("FACT"))
#' expect_error(individual_attribute_structure(c("FACT", "EDUC"), "This is a fact"))
#' expect_error(individual_attribute_structure("FACT", "This is a fact"))
#' expect_error(individual_attribute_structure("IDNO", 123456))
#' 
#' expect_equal(individual_attribute_structure("NATI", "British"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "NATI",     "British"
#'              ))
#' 
#' expect_equal(individual_attribute_structure("NATI", "British", individual_event_detail(age = "0y")),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "NATI",     "British",
#'                              1,  "AGE",           "0y"
#'              ))
#' @return A tidy tibble containing the INDIVIDUAL_ATTRIBUTE_STRUCTURE part of a GEDCOM file
#' @export
individual_attribute_structure <- function(attribute_type,
                                           attribute_description,
                                           individual_event_details = individual_event_detail()) {
  
  if (length(attribute_type) == 0) return(tibble::tibble())
  if (length(attribute_description) == 0) return(tibble::tibble())
  
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
    
    temp <- tibble::tibble(level = 0, tag = attribute_type, value = attribute_description)
    
  }
  
  temp <- dplyr::bind_rows(temp, 
            individual_event_details %>% add_levels(1)
  )
  
  if (sum(temp$tag %in% c("IDNO", "FACT")) == 1 & sum(temp$tag == "TYPE") == 0)
    stop("IDNO and FACT tags require a event_or_fact_classification to be defined in the event detail.")
  
  temp
}

#' Constructs the INDIVIDUAL_EVENT_DETAIL from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @param event_details An event_detail() object giving details of the event.
#' @tests
#' expect_equal(dim(individual_event_detail()), c(0, 3))  
#' 
#' expect_equal(individual_event_detail(age = "5y"),
#'              tibble::tribble(~level,  ~tag, ~value,
#'                              0, "AGE",    "5y"
#'              ))
#' @return A tidy tibble containing the INDIVIDUAL_EVENT_DETAIL part of a GEDCOM file
#' @export
individual_event_detail <- function(event_details = event_detail(),
                                    age_at_event = character()) {
  
  age_at_event <- as.character(age_at_event)
  
  validate_age_at_event(age_at_event, 1)
  
  dplyr::bind_rows(
    event_details %>% add_levels(0),
    tibble::tibble(level = 0, tag = "AGE", value = age_at_event)
  )
  
  
}

#' Constructs the INDIVIDUAL_EVENT_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @param individual_event_details An individual_event_detail() object giving details of the event.
#' @tests
#' expect_error(individual_event_structure())
#' expect_error(individual_event_structure("BLAH"))
#' expect_error(individual_event_structure("ADOP", adopted_by_which_parent = "WHO"))
#' 
#' expect_equal(individual_event_structure("BIRT"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "BIRT",    "Y"
#'              ))
#' 
#' expect_equal(individual_event_structure("CHRA"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "CHRA",     ""
#'              ))
#' 
#' expect_equal(individual_event_structure("BIRT", xref_fam = "@F4@"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "BIRT",    "Y",
#'                              1, "FAMC", "@F4@"
#'              ))
#' 
#' expect_equal(individual_event_structure("ADOP", xref_fam = "@F4@", adopted_by_which_parent = "BOTH"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "ADOP",     "",
#'                              1, "FAMC", "@F4@",
#'                              2, "ADOP", "BOTH"
#'              ))
#' @return A tidy tibble containing the INDIVIDUAL_EVENT_STRUCTURE part of a GEDCOM file
#' @export
individual_event_structure <- function(event_type_individual,
                                       individual_event_details = individual_event_detail(),
                                       xref_fam = character(),
                                       adopted_by_which_parent = character()) {
  
  if (length(event_type_individual) == 0) return(tibble::tibble())
  
  validate_event_type_individual(event_type_individual, 1)
  validate_xref(xref_fam, 1)
  validate_adopted_by_which_parent(adopted_by_which_parent, 1)
  
  temp <- dplyr::bind_rows(
    tibble::tibble(level = 0, tag = event_type_individual, value = ""),
    individual_event_details %>% add_levels(1)
  )
    
  if (sum(temp$tag %in% c("BIRT", "CHR", "ADOP")) == 1)
    temp <- dplyr::bind_rows(
      temp,
      tibble::tibble(level = 1, tag = "FAMC", value = xref_fam)
    )
  
  if (sum(temp$tag == "ADOP") == 1)
    temp <- dplyr::bind_rows(
      temp,
      tibble::tibble(level = 2, tag = "ADOP", value = adopted_by_which_parent)
    )
    
  temp %>% 
    dplyr::mutate(value = if_else(tag %in% c("BIRT", "CHR", "DEAT"), "Y", value))
  
}


lds_individual_ordinance <- function() {
  
  tibble::tibble()
}


lds_spouse_sealing <- function() {
  
  tibble::tibble()
}

#' Constructs the MULTIMEDIA_LINK from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(multimedia_link("ref"))
#' expect_error(multimedia_link(multimedia_file_reference = "ref", source_media_type = "carrier pigeon"))
#' expect_error(multimedia_link(multimedia_file_reference = "ref", multimedia_format = "jpeg"))
#' 
#' expect_equal(multimedia_link("@M1@"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "OBJE", "@M1@"
#'              ))
#' 
#' expect_equal(multimedia_link(multimedia_file_reference = "ref", multimedia_format = "jpg", 
#'                              source_media_type = "electronic"),
#'              tibble::tribble(~level,   ~tag,       ~value,
#'                              0, "OBJE",           "",
#'                              1, "FILE",        "ref",
#'                              2, "FORM",        "jpg",
#'                              3, "MEDI", "electronic"
#'              ))
#' @return A tidy tibble containing the MULTIMEDIA_LINK part of a GEDCOM file
#' @export
multimedia_link <- function(xref_obje = character(),
                            multimedia_file_reference = character(),
                            multimedia_format = character(),
                            source_media_type = character(),
                            descriptive_title = character()) {
  
  if (length(xref_obje) + length(multimedia_file_reference) == 0) 
    return(tibble::tibble())
  
  if (length(multimedia_format) != length(multimedia_file_reference) &
      length(multimedia_file_reference) > 0)
    stop("Each file reference requires a file format")
  
  if (length(source_media_type) != length(multimedia_file_reference) &
      length(multimedia_file_reference) > 0 & length(source_media_type) > 0)
    stop("Each file reference requires a source media type (even if it's blank)")
  
  validate_xref(xref_obje, 1)
  
  if (length(xref_obje) == 1) {
    
    tibble::tibble(level = 0, tag = "OBJE", value = xref_obje)
  
  } else {
  
    validate_multimedia_file_reference(multimedia_file_reference, 1000)
    validate_multimedia_format(multimedia_format, 1000)
    validate_source_media_type(source_media_type, 1000)
    validate_descriptive_title(descriptive_title, 1)
    
    temp <- tibble::tibble(level = 0, tag = "OBJE", value = "")
    
    for (i in seq_along(multimedia_file_reference)) {
      
      temp <- dplyr::bind_rows(temp,
                        tibble::tibble(level = 1, tag = "FILE", value = multimedia_file_reference[i]),
                        tibble::tibble(level = 2, tag = "FORM", value = multimedia_format[i]),
                        tibble::tibble(level = 3, tag = "MEDI", value = source_media_type[i])
      )
    }
    
    dplyr::bind_rows(temp,
              tibble::tibble(level = 1, tag = "TITL", value = descriptive_title)
    )
  }
}

#' Constructs the NOTE_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(note_structure(submitter_text = c("test1", "test2")))
#' 
#' expect_equal(note_structure("@T1@"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "NOTE", "@T1@"
#'              ))
#' 
#' expect_equal(note_structure(submitter_text = "test text"),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "NOTE", "test text"
#'              ))
#' 
#' expect_equal(note_structure(submitter_text = paste0(rep("a", 248), collapse="")),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "NOTE", paste0(rep("a", 248), collapse="")
#'              ))
#' 
#' expect_equal(note_structure(submitter_text = paste0(rep("a", 249), collapse="")),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "NOTE", paste0(rep("a", 248), collapse=""),
#'                              1, "CONC", "a"
#'              ))
#' 
#' expect_equal(note_structure(submitter_text = paste0(rep("a", 992), collapse="")),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "NOTE", paste0(rep("a", 248), collapse=""),
#'                              1, "CONC", paste0(rep("a", 248), collapse=""),
#'                              1, "CONC", paste0(rep("a", 248), collapse=""),
#'                              1, "CONC", paste0(rep("a", 248), collapse="")
#'              ))
#' @return A tidy tibble containing the NOTE_STRUCTURE part of a GEDCOM file
#' @export
note_structure <- function(xref_note = character(),
                           submitter_text = character()) {
  
  if (length(xref_note) + length(submitter_text) == 0) 
    return(tibble::tibble())
  
  validate_xref(xref_note, 1)
  
  if (length(xref_note) == 1) {
    
    tibble::tibble(level = 0, tag = "NOTE", value = xref_note)
  
  } else {
  
    validate_submitter_text(submitter_text, 1)
    
    split_text(start_level = 0, top_tag = "NOTE", text = submitter_text)  
  }
  
}




#' Constructs the PERSONAL_NAME_PIECES from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_equal(dim(personal_name_pieces()), c(0, 3))
#' 
#' expect_equal(personal_name_pieces(name_piece_prefix = "Mr", name_piece_nickname = "J"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "NPFX",   "Mr",
#'                              0, "NICK",    "J"
#'              ))
#' 
#' expect_equal(personal_name_pieces(name_piece_prefix = "Mr", name_piece_nickname = "J",
#'                                   notes = list(note_structure(submitter_text = paste0(rep("a", 992), collapse="")),
#'                                                note_structure(submitter_text = paste0(rep("b", 992), collapse="")))),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "NPFX",   "Mr",
#'                              0, "NICK",    "J",
#'                              0, "NOTE", paste0(rep("a", 248), collapse=""),
#'                              1, "CONC", paste0(rep("a", 248), collapse=""),
#'                              1, "CONC", paste0(rep("a", 248), collapse=""),
#'                              1, "CONC", paste0(rep("a", 248), collapse=""),
#'                              0, "NOTE", paste0(rep("b", 248), collapse=""),
#'                              1, "CONC", paste0(rep("b", 248), collapse=""),
#'                              1, "CONC", paste0(rep("b", 248), collapse=""),
#'                              1, "CONC", paste0(rep("b", 248), collapse="")
#'              ))
#' @return A tidy tibble containing the PERSONAL_NAME_PIECES part of a GEDCOM file
#' @export
personal_name_pieces <- function(name_piece_prefix = character(),
                                 name_piece_given = character(), 
                                 name_piece_nickname = character(), 
                                 name_piece_surname_prefix = character(),
                                 name_piece_surname = character(),
                                 name_piece_suffix = character(),
                                 notes = list(),
                                 source_citations = list()) {
  
  validate_name_piece_suffix(name_piece_prefix, 1)
  validate_name_piece_given(name_piece_given, 1)
  validate_name_piece_nickname(name_piece_nickname, 1)
  validate_name_piece_surname_prefix(name_piece_surname_prefix, 1)
  validate_name_piece_surname(name_piece_surname, 1)
  validate_name_piece_suffix(name_piece_suffix, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "NPFX", value = name_piece_prefix),
    tibble::tibble(level = 0, tag = "GIVN", value = name_piece_given),
    tibble::tibble(level = 0, tag = "NICK", value = name_piece_nickname),
    tibble::tibble(level = 0, tag = "SPFX", value = name_piece_surname_prefix),
    tibble::tibble(level = 0, tag = "SURN", value = name_piece_surname),
    tibble::tibble(level = 0, tag = "NSFX", value = name_piece_suffix),
    notes %>% dplyr::bind_rows() %>% add_levels(0),
    source_citations %>% dplyr::bind_rows() %>% add_levels(0)
  ) 
  
}

#' Constructs the PERSONAL_NAME_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @param name_pieces A personal_name_pieces() object giving the components of the name.
#' @param phonetic_name_pieces A list of personal_name_pieces() objects giving the components 
#' of the phonetic name variations.
#' @param romanized_name_pieces A list of personal_name_pieces() objects giving the components 
#' of the romanized name variations.
#' @tests
#' expect_error(personal_name_structure())
#' expect_error(
#'   personal_name_structure("Joe Bloggs", 
#'                           name_phonetic_variation = c("Joe Blogs", "Jo Bloggs"))
#' )
#' expect_error(
#'   personal_name_structure("Joe Bloggs", 
#'                           name_phonetic_variation = c("Joe Blogs", "Jo Bloggs"),
#'                           phonetic_type = "Can't spell")
#' )
#' expect_error(
#'   personal_name_structure("Joe Bloggs", 
#'                           name_phonetic_variation = c("Joe Blogs", "Jo Bloggs"),
#'                           phonetic_type = c("Can't spell", "Can't spell"),
#'                           phonetic_name_pieces = list(personal_name_pieces(name_piece_given = "Joe", 
#'                                                                            name_piece_surname = "Blogs")))
#' )
#' 
#' expect_equal(personal_name_structure("Joe /Bloggs/", 
#'                                      name_pieces = personal_name_pieces(name_piece_prefix = "Mr",
#'                                                                         name_piece_surname = "Bloggs")),
#'              tibble::tribble(~level,   ~tag,         ~value,
#'                              0, "NAME", "Joe /Bloggs/",
#'                              1, "NPFX",           "Mr",
#'                              1, "SURN",       "Bloggs"
#'              ))
#' 
#' expect_equal(personal_name_structure("Joe Bloggs", 
#'                                      name_phonetic_variation = c("Joe Blogs", "Jo Bloggs"),
#'                                      phonetic_type = c("Can't spell", "Can't spell")),
#'              tibble::tribble(~level,   ~tag,         ~value,
#'                              0, "NAME",   "Joe Bloggs",
#'                              1, "FONE",    "Joe Blogs",
#'                              2, "TYPE",  "Can't spell",
#'                              1, "FONE",    "Jo Bloggs",
#'                              2, "TYPE",  "Can't spell"
#'              ))
#' 
#' expect_equal(personal_name_structure("Joe Bloggs", 
#'                                      name_phonetic_variation = c("Joe Blogs", "Jo Bloggs"),
#'                                      phonetic_type = c("Can't spell", "Can't spell"),
#'                                      phonetic_name_pieces = 
#'                                        list(personal_name_pieces(name_piece_given = "Joe", 
#'                                                                  name_piece_surname = "Blogs"),
#'                                             personal_name_pieces(name_piece_given = "Jo",
#'                                                                  name_piece_surname = "Bloggs"))),
#'              tibble::tribble(~level,   ~tag,         ~value,
#'                              0, "NAME",   "Joe Bloggs",
#'                              1, "FONE",    "Joe Blogs",
#'                              2, "TYPE",  "Can't spell",
#'                              2, "GIVN",          "Joe",
#'                              2, "SURN",        "Blogs",
#'                              1, "FONE",    "Jo Bloggs",
#'                              2, "TYPE",  "Can't spell",
#'                              2, "GIVN",           "Jo",
#'                              2, "SURN",       "Bloggs"
#'              ))
#' @return A tidy tibble containing the PERSONAL_NAME_STRUCTURE part of a GEDCOM file
#' @export
personal_name_structure <- function(name_personal,
                                    name_type = character(),
                                    name_pieces = personal_name_pieces(), 
                                    name_phonetic_variation = character(),
                                    phonetic_type = character(),
                                    phonetic_name_pieces = list(),
                                    name_romanized_variation = character(),
                                    romanized_type = character(),
                                    romanized_name_pieces = list()) {
  
  if (length(name_personal) == 0) return(tibble::tibble())
  
  validate_name_personal(name_personal, 1)
  validate_name_type(name_type, 1)
  validate_name_phonetic_variation(name_phonetic_variation, 1000)
  validate_phonetic_type(phonetic_type, 1000)
  validate_name_romanized_variation(name_romanized_variation, 1000)
  validate_romanized_type(romanized_type, 1000)
  
  if (length(name_phonetic_variation) != length(phonetic_type))
    stop("Each phonetic variation requires a phonetic type")
  if (length(name_romanized_variation) != length(romanized_type))
    stop("Each romanized variation requires a romanized type")
  
  if (length(name_phonetic_variation) != length(phonetic_name_pieces) &
      length(name_phonetic_variation) > 0 & length(phonetic_name_pieces) > 0)
    stop("Each phonetic variation requires a set of phonetic name pieces (even if empty)")
  
  if (length(name_romanized_variation) != length(romanized_name_pieces) &
      length(name_romanized_variation) > 0 & length(romanized_name_pieces) > 0)
    stop("Each romanized variation requires a set of romanized name pieces (even if empty)")
  
  temp <- dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "NAME", value = name_personal),
    tibble::tibble(level = 1, tag = "TYPE", value = name_type),
    name_pieces %>% add_levels(1)
  )
  
  for (i in seq_along(name_phonetic_variation)) {
    temp <- dplyr::bind_rows(
      temp,
      tibble::tibble(level = 1, tag = "FONE", value = name_phonetic_variation[i]),
      tibble::tibble(level = 2, tag = "TYPE", value = phonetic_type[i])
    )
    if (length(phonetic_name_pieces) > 0)
      temp <- dplyr::bind_rows(temp, phonetic_name_pieces[[i]] %>% add_levels(2))
  }
  for (i in seq_along(name_romanized_variation)) {
    temp <- dplyr::bind_rows(
     temp,
     tibble::tibble(level = 1, tag = "ROMN", value = name_romanized_variation[i]),
     tibble::tibble(level = 2, tag = "TYPE", value = romanized_type[i])
    )
    if (length(romanized_name_pieces) > 0)
      temp <- dplyr::bind_rows(temp, romanized_name_pieces[[i]] %>% add_levels(2))
  }
  
  temp
  
}

#' Constructs the PLACE_STRUCTURE from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(place_structure())
#' expect_error(place_structure("Here", place_latitude = "N51.5", place_longitude = "E0.0"))
#' expect_error(
#'   place_structure("London", 
#'                   place_phonetic_variation = c("Lundon", "Lundun"))
#' )
#' expect_error(
#'   place_structure("London", 
#'                   place_phonetic_variation = c("Lundon", "Lundun"),
#'                   phonetic_type = "English accent")
#' )
#' 
#' expect_equal(place_structure("Greenwich", 
#'                              place_phonetic_variation = c("Grenidge", "Grenich"),
#'                              phonetic_type = c("English accent", "English accent"),
#'                              place_latitude = "N51.5",
#'                              place_longitude = "E0.00"),
#'              tibble::tribble(~level,   ~tag,           ~value,
#'                              0, "PLAC",      "Greenwich",
#'                              1, "FONE",       "Grenidge",
#'                              2, "TYPE", "English accent",
#'                              1, "FONE",        "Grenich",
#'                              2, "TYPE", "English accent",
#'                              1,  "MAP",               "",
#'                              2, "LATI",          "N51.5",
#'                              2, "LONG",           "E0.00"
#'              ))
#' @return A tidy tibble containing the PLACE_STRUCTURE part of a GEDCOM file
#' @export
place_structure <- function(place_name,
                            place_hierarchy = character(),
                            place_phonetic_variation = character(),
                            phonetic_type = character(),
                            place_romanized_variation = character(),
                            romanized_type = character(),
                            place_latitude = character(),
                            place_longitude = character(),
                            notes = list()) {

  if (length(place_name) == 0) return(tibble::tibble())
  
  validate_place_name(place_name, 1)
  validate_place_hierarchy(place_hierarchy, 1)
  validate_place_phonetic_variation(place_phonetic_variation, 1000)
  validate_phonetic_type(phonetic_type, 1000)
  validate_place_romanized_variation(place_romanized_variation, 1000)
  validate_romanized_type(romanized_type, 1000)
  validate_place_latitude(place_latitude, 1)
  validate_place_longitude(place_longitude, 1)
  
  if (length(place_phonetic_variation) != length(phonetic_type))
    stop("Each phonetic variation requires a phonetic type")
  if (length(place_romanized_variation) != length(romanized_type))
    stop("Each romanized variation requires a romanized type")
  
  temp <- dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "PLAC", value = place_name),
    tibble::tibble(level = 1, tag = "FORM", value = place_hierarchy)
    )
  
  for (i in seq_along(place_phonetic_variation)) {
    temp <- dplyr::bind_rows(
      temp,
      tibble::tibble(level = 1, tag = "FONE", value = place_phonetic_variation[i]),
      tibble::tibble(level = 2, tag = "TYPE", value = phonetic_type[i])
    )
  }
  for (i in seq_along(place_romanized_variation)) {
    temp <- dplyr::bind_rows(
      temp,
      tibble::tibble(level = 1, tag = "ROMN", value = place_romanized_variation[i]),
      tibble::tibble(level = 2, tag = "TYPE", value = romanized_type[i])
    )
  }
  
  temp <- dplyr::bind_rows(
    temp,
    tibble::tibble(level = 1, tag = "MAP", value = ""),
    tibble::tibble(level = 2, tag = "LATI", value = place_latitude),
    tibble::tibble(level = 2, tag = "LONG", value = place_longitude),
    notes %>% dplyr::bind_rows() %>% add_levels(1)
  )
  
  if (sum(temp$tag == "LATI") == 0) temp <- dplyr::filter(temp, tag != "MAP")
  if (sum(temp$tag == "LONG") == 0) temp <- dplyr::filter(temp, tag != "MAP")
  temp
  
}

#' Constructs the SOURCE_CITATION from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_equal(source_citation("@S1@"),
#'              tibble::tribble(
#'                ~level,   ~tag, ~value,
#'                0, "SOUR", "@S1@"
#'              ))
#' 
#' expect_equal(source_citation("@S1@", where_within_source = 3, event_type_cited_from = "event"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "SOUR",  "@S1@",
#'                              1, "PAGE",     "3",
#'                              1, "EVEN", "event"
#'              ))
#' 
#' expect_equal(source_citation("@S1@", where_within_source = 3, role_in_event = "a role"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "SOUR",  "@S1@",
#'                              1, "PAGE",     "3"
#'              ))
#' 
#' expect_equal(source_citation("@S1@", where_within_source = 3, 
#'                              event_type_cited_from = "event", 
#'                              role_in_event = "a role"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "SOUR",   "@S1@",
#'                              1, "PAGE",      "3",
#'                              1, "EVEN",  "event",
#'                              2, "ROLE", "a role"
#'              ))
#' @return A tidy tibble containing the SOURCE_CITATION part of a GEDCOM file
#' @export
source_citation <- function(xref_sour = character(),
                            source_description = character(),
                            where_within_source = character(),
                            event_type_cited_from = character(),
                            role_in_event = character(),
                            entry_recording_date = date_value(),
                            text_from_source = character(),
                            certainty_assessment = character(),
                            multimedia_links = list(),
                            notes = list()) {
  
  if (length(xref_sour) + length(source_description) == 0) 
    return(tibble::tibble())
  
  where_within_source <- as.character(where_within_source)
  certainty_assessment <- as.character(certainty_assessment)
  
  validate_xref(xref_sour, 1)
  
  if (length(xref_sour) == 1) {
    
    validate_where_within_source(where_within_source, 1)
    validate_event_type_cited_from(event_type_cited_from, 1)
    validate_role_in_event(role_in_event, 1)
    validate_entry_recording_date(entry_recording_date, 1)
    
    temp <- dplyr::bind_rows(
      tibble::tibble(level = 0, tag = "SOUR", value = xref_sour),
      tibble::tibble(level = 1, tag = "PAGE", value = where_within_source),
      tibble::tibble(level = 1, tag = "EVEN", value = event_type_cited_from),
      tibble::tibble(level = 2, tag = "ROLE", value = role_in_event),
      tibble::tibble(level = 1, tag = "DATA", value = ""),
      tibble::tibble(level = 1, tag = "DATE", value = entry_recording_date),
      split_text(start_level = 2, top_tag = "TEXT", text = text_from_source),
      multimedia_links %>% dplyr::bind_rows() %>% add_levels(1),
      notes %>% dplyr::bind_rows() %>% add_levels(1),
      tibble::tibble(level = 1, tag = "QUAY", value = certainty_assessment)
    ) 
    
    if (sum(temp$tag == "EVEN") == 0) temp <- dplyr::filter(temp, tag != "ROLE")
    if (sum(temp$tag == "DATE") == 0 & sum(temp$tag == "TEXT") == 0) 
      temp <- dplyr::filter(temp, tag != "DATA")
    
    temp
    
  } else {
    
    dplyr::bind_rows(
      split_text(start_level = 0, top_tag = "SOUR", text = source_description),
      split_text(start_level = 1, top_tag = "TEXT", text = text_from_source),
      multimedia_links %>% dplyr::bind_rows() %>% add_levels(1),
      notes %>% dplyr::bind_rows() %>% add_levels(1),
      tibble::tibble(level = 1, tag = "QUAY", value = certainty_assessment)
    )
    
  }
  
}

#' Constructs the SOURCE_REPOSITORY_CITATION from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(source_repository_citation())
#' expect_error(source_repository_citation("@R1@", source_media_type = "carrier pigeon"))
#' 
#' expect_equal(source_repository_citation("@R1@", source_call_number = c("123", "456"), 
#'                                         source_media_type = "map"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "REPO", "@R1@",
#'                              1, "CALN",  "123",
#'                              1, "CALN",  "456",
#'                              2, "MEDI",  "map"
#'              ))
#' @return A tidy tibble containing the SOURCE_REPOSITORY_CITATION part of a GEDCOM file
#' @export
source_repository_citation <- function(xref_repo,
                                       notes = list(),
                                       source_call_number = character(),
                                       source_media_type = character()) {
  
  if (length(xref_repo) == 0) return(tibble())
  
  validate_xref(xref_repo, 1)
  validate_source_call_number(source_call_number, 1000)
  validate_source_media_type(source_media_type, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "REPO", value = xref_repo),
    notes %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "CALN", value = source_call_number),
    tibble::tibble(level = 2, tag = "MEDI", value = source_media_type)
    )
    
}


#' Constructs the SPOUSE_TO_FAMILY_LINK from the GEDCOM specification
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(spouse_to_family_link())
#' @return A tidy tibble containing the SPOUSE_TO_FAMILY_LINK part of a GEDCOM file
#' @export
spouse_to_family_link <- function(xref_fam,
                                  notes = list()) {
  
  if (length(xref_fam) == 0) return(tibble())
  
  validate_xref(xref_fam, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "FAMS", value = xref_fam),
    notes %>% dplyr::bind_rows() %>% add_levels(1)
  )
}



