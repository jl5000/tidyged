
#' Construct the ADDRESS_STRUCTURE tibble
#' 
#' This function constructs a tibble representation of the ADDRESS_STRUCTURE from the GEDCOM 5.5.5
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param local_address_lines These address lines contain the address lines before the town/city.
#' This can be a vector with up to 3 elements.
#' @tests
#' expect_error(ADDRESS_STRUCTURE(letters[1:4]))
#' expect_error(ADDRESS_STRUCTURE("address", address_city = 1:2))
#' expect_error(ADDRESS_STRUCTURE("address", address_state = 1:2))
#' expect_error(ADDRESS_STRUCTURE("address", address_postal_code = 1:2))
#' expect_error(ADDRESS_STRUCTURE("address", phone_number = 1:4))
#' expect_error(ADDRESS_STRUCTURE("address", address_email = 1:4))
#' expect_error(ADDRESS_STRUCTURE("address", address_fax = 1:4))
#' expect_error(ADDRESS_STRUCTURE("address", address_web_page = 1:4))
#' expect_error(ADDRESS_STRUCTURE(paste0(rep("a", 61), collapse = "")))
#' expect_error(ADDRESS_STRUCTURE("address", address_city = paste0(rep("a", 61), collapse = "")))
#' expect_error(ADDRESS_STRUCTURE("address", address_state = paste0(rep("a", 61), collapse = "")))
#' expect_error(ADDRESS_STRUCTURE("address", address_postal_code = paste0(rep("a", 11), collapse = "")))
#' expect_error(ADDRESS_STRUCTURE("address", address_country = paste0(rep("a", 61), collapse = "")))
#' expect_error(ADDRESS_STRUCTURE("address", address_web_page = paste0(rep("a", 121), collapse = "")))
#' expect_equal(ADDRESS_STRUCTURE(), tibble::tibble())
#' expect_equal(ADDRESS_STRUCTURE("Road name"),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "ADDR", "",
#'                              1, "ADR1", "Road name"
#'              ))
#' 
#' expect_equal(ADDRESS_STRUCTURE(letters[1:3]),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "ADDR",    "",
#'                              1, "ADR1",    "a",
#'                              1, "ADR2",    "b",
#'                              1, "ADR3",    "c"
#'              ))
#' 
#' expect_equal(ADDRESS_STRUCTURE(letters[1:2], address_country = "UK"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "ADDR",    "",
#'                              1, "ADR1",    "a",
#'                              1, "ADR2",    "b",
#'                              1, "CTRY",   "UK"
#'              )) 
#' @return A tidy tibble containing the ADDRESS_STRUCTURE part of a GEDCOM file.
ADDRESS_STRUCTURE <- function(local_address_lines = character(),
                              address_city = character(),
                              address_state = character(),
                              address_postal_code = character(),
                              address_country = character(),
                              phone_number = character(),
                              address_email = character(),
                              address_fax = character(),
                              address_web_page = character()) {
  
  address_postal_code <- as.character(address_postal_code)
  phone_number <- as.character(phone_number)
  address_fax <- as.character(address_fax)
  
  validate_address_lines(local_address_lines, 3)
  validate_address_city(address_city, 1)
  validate_address_state(address_state, 1)
  validate_address_postal_code(address_postal_code, 1)
  validate_address_country(address_country, 1)
  validate_phone_number(phone_number, 3)
  validate_address_email(address_email, 3)
  validate_address_fax(address_fax, 3)
  validate_address_web_page(address_web_page, 3)
  
  address_lines <- tibble::tibble(level = 0, tag = "ADDR", value = "")
  
  for (i in seq_along(local_address_lines)) {
    
    address_lines <- dplyr::bind_rows(
      address_lines,
      tibble::tibble(level = 1, tag = paste0("ADR", i), value = local_address_lines[i])
    )
  }
  
  temp <- dplyr::bind_rows(
    address_lines,
    tibble::tibble(level = 1, tag = "CITY", value = address_city),
    tibble::tibble(level = 1, tag = "STAE", value = address_state),
    tibble::tibble(level = 1, tag = "POST", value = address_postal_code),
    tibble::tibble(level = 1, tag = "CTRY", value = address_country),
    tibble::tibble(level = 0, tag = "PHON", value = phone_number),
    tibble::tibble(level = 0, tag = "EMAIL", value = address_email),
    tibble::tibble(level = 0, tag = "FAX", value = address_fax),
    tibble::tibble(level = 0, tag = "WWW", value = address_web_page)
  )
  
  if(nrow(temp) <= 1) {
    return(tibble::tibble())
  } else {
    return(temp)
  }
  
}

#' Construct the ASSOCIATION_STRUCTURE tibble
#' 
#' This function constructs a tibble representation of the ASSOCIATION_STRUCTURE from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(ASSOCIATION_STRUCTURE())
#' expect_error(ASSOCIATION_STRUCTURE("@1@"))
#' expect_error(ASSOCIATION_STRUCTURE(c("@1@", "@2@"), "Godfather"))
#' 
#' expect_equal(ASSOCIATION_STRUCTURE("@I1@", "Godfather"),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "ASSO",      "@I1@",
#'                              1, "RELA", "Godfather"
#'              ))
#' 
#' expect_equal(ASSOCIATION_STRUCTURE("@I1@", "Father", 
#'                                    notes = list(NOTE_STRUCTURE(submitter_text = "This is a note"))), 
#'              tibble::tribble(~level,   ~tag,              ~value,
#'                              0, "ASSO",              "@I1@",
#'                              1, "RELA",            "Father",
#'                              1, "NOTE",    "This is a note"
#'              ))
#' @return A tidy tibble containing the ASSOCIATION_STRUCTURE part of a GEDCOM file.
ASSOCIATION_STRUCTURE <- function(xref_indi,
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

#' Construct the CHANGE_DATE tibble
#'
#' This function constructs a tibble representation of the CHANGE_DATE from the GEDCOM 5.5.1
#' specification.
#' 
#' @inheritParams parameter_definitions
#' @param change_date A date_exact() object giving the date that this data was changed.
#' @tests
#' expect_equal(CHANGE_DATE(),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "CHAN", "",
#'                              1, "DATE", toupper(format(Sys.Date(), "%d %b %Y"))
#'              ))
#'                              
#' expect_equal(CHANGE_DATE(date_exact(5, 10, 1990)),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "CHAN", "",
#'                              1, "DATE", "5 OCT 1990"
#'              ))
#'                              
#' expect_equal(CHANGE_DATE(date_exact(18, 12, 2008), time_value = "11:00:08.56"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "CHAN", "",
#'                              1, "DATE", "18 DEC 2008",
#'                              2, "TIME", "11:00:08.56"
#'              ))
#'                              
#' expect_equal(CHANGE_DATE(date_exact(5, 10, 1990), "10:34:56", 
#'                          notes = list(NOTE_STRUCTURE(submitter_text = "Note 1"),
#'                                       NOTE_STRUCTURE(submitter_text = "Note 2"))),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "CHAN", "",
#'                              1, "DATE", "5 OCT 1990",
#'                              2, "TIME", "10:34:56",
#'                              1, "NOTE", "Note 1",
#'                              1, "NOTE", "Note 2"
#'              ))
#' @return A tidy tibble containing the CHANGE_DATE part of a GEDCOM file.
CHANGE_DATE <- function(change_date = date_exact(),
                        time_value = character(),
                        notes = list()) {
  
  if (length(change_date) == 0) 
    change_date <- current_date()
  
  validate_date_exact(change_date, 1)
  validate_time_value(time_value, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "CHAN", value = ""),
    tibble::tibble(level = 1, tag = "DATE", value = change_date),
    tibble::tibble(level = 2, tag = "TIME", value = time_value),
    notes %>% dplyr::bind_rows() %>% add_levels(1)
  )
  
}

#' Construct the CHILD_TO_FAMILY_LINK tibble
#' 
#' This function constructs a tibble representation of the CHILD_TO_FAMILY_LINK from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(CHILD_TO_FAMILY_LINK())
#' expect_error(CHILD_TO_FAMILY_LINK("@1@", pedigree_linkage_type = "foste"))
#' 
#' expect_equal(CHILD_TO_FAMILY_LINK("@F1@"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "FAMC", "@F1@"
#'              ))
#' 
#' expect_equal(CHILD_TO_FAMILY_LINK("@F1@", "birth"),
#'              tibble::tribble(~level,   ~tag,   ~value,
#'                              0, "FAMC",   "@F1@",
#'                              1, "PEDI",  "birth"
#'              ))
#' @return A tidy tibble containing the CHILD_TO_FAMILY_LINK part of a GEDCOM file.
CHILD_TO_FAMILY_LINK <- function(xref_fam,
                                 pedigree_linkage_type = character(),
                                 notes = list()) {
  
  if (length(xref_fam) == 0) return(tibble::tibble())
  
  validate_xref(xref_fam, 1)
  validate_pedigree_linkage_type(pedigree_linkage_type, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "FAMC", value = xref_fam),
    tibble::tibble(level = 1, tag = "PEDI", value = pedigree_linkage_type),
    notes %>% dplyr::bind_rows() %>% add_levels(1)
  )
  
}

#' Construct the EVENT_DETAIL tibble
#' 
#' This function constructs a tibble representation of the EVENT_DETAIL from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param date A date_value() object giving the date of the event.
#' @param place A PLACE_STRUCTURE() object giving the location of the event.
#' @param address An ADDRESS_STRUCTURE() object giving the address of the event.
#' @tests
#' expect_equal(dim(EVENT_DETAIL()), c(0, 3))
#' 
#' expect_equal(EVENT_DETAIL(event_or_fact_classification = "Woodworking"),
#'              tibble::tribble(~level,   ~tag,        ~value,
#'                              0, "TYPE", "Woodworking"
#'              ))
#' 
#' expect_equal(EVENT_DETAIL(place = PLACE_STRUCTURE("Somewhere")),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "PLAC", "Somewhere"
#'              ))
#' 
#' expect_equal(EVENT_DETAIL(place = PLACE_STRUCTURE("Somewhere"), 
#'                           date = date_value(2008, 4, 9, about = TRUE)),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "DATE", "ABT 9 APR 2008",
#'                              0, "PLAC", "Somewhere"
#'              ))
#' 
#' expect_equal(EVENT_DETAIL(address = ADDRESS_STRUCTURE(c("House name", "Road"))),
#'              tibble::tribble(~level,   ~tag,       ~value,
#'                              0, "ADDR", "House name",
#'                              1, "CONT",       "Road",
#'                              1, "ADR1",       "Road"
#'              ))
#' @return A tidy tibble containing the EVENT_DETAIL part of a GEDCOM file.
EVENT_DETAIL <- function(event_or_fact_classification = character(),
                         date = date_value(),
                         place = PLACE_STRUCTURE(character()),
                         address = ADDRESS_STRUCTURE(),
                         responsible_agency = character(),
                         religious_affiliation = character(),
                         cause_of_event = character(),
                         notes = list(),
                         source_citations = list(),
                         multimedia_links = list()) {
  
  validate_event_or_fact_classification(event_or_fact_classification, 1)
  validate_date_value(date, 1)
  validate_responsible_agency(responsible_agency, 1)
  validate_religious_affiliation(religious_affiliation, 1)
  validate_cause_of_event(cause_of_event, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "TYPE", value = event_or_fact_classification),
    tibble::tibble(level = 0, tag = "DATE", value = date),
    place %>% add_levels(0),
    address %>% add_levels(0),
    tibble::tibble(level = 0, tag = "AGNC", value = responsible_agency),
    tibble::tibble(level = 0, tag = "RELI", value = religious_affiliation),
    tibble::tibble(level = 0, tag = "CAUS", value = cause_of_event),
    notes %>% dplyr::bind_rows() %>% add_levels(0),
    source_citations %>% dplyr::bind_rows() %>% add_levels(0),
    multimedia_links %>% dplyr::bind_rows() %>% add_levels(0)
  )
  
}

#' Construct the FAMILY_EVENT_DETAIL tibble
#' 
#' This function constructs a tibble representation of the FAMILY_EVENT_DETAIL from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param event_details An EVENT_DETAIL() object giving details of the event.
#' @tests
#' expect_equal(dim(FAMILY_EVENT_DETAIL()), c(0, 3))  
#' 
#' expect_equal(FAMILY_EVENT_DETAIL(husband_age_at_event = "42y"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "HUSB",     "",
#'                              1,  "AGE",   "42y"
#'              ))
#' 
#' expect_equal(FAMILY_EVENT_DETAIL(wife_age_at_event = "40y"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "WIFE",     "",
#'                              1,  "AGE",   "40y"
#'              ))
#' 
#' expect_equal(FAMILY_EVENT_DETAIL(husband_age_at_event = "42y", wife_age_at_event = "40y"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "HUSB",     "",
#'                              1,  "AGE",   "42y",
#'                              0, "WIFE",     "",
#'                              1,  "AGE",   "40y"
#'              ))
#' @return A tidy tibble containing the FAMILY_EVENT_DETAIL part of a GEDCOM file.
FAMILY_EVENT_DETAIL <- function(husband_age_at_event = character(),
                                wife_age_at_event = character(),
                                event_details = EVENT_DETAIL()) {
  
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
                tag = dplyr::if_else(tag == "HAGE", "AGE", tag),
                tag = dplyr::if_else(tag == "WAGE", "AGE", tag))
  
}

#' Construct the FAMILY_EVENT_STRUCTURE tibble
#' 
#' This function constructs a tibble representation of the FAMILY_EVENT_STRUCTURE from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param family_event_details A FAMILY_EVENT_DETAIL() object giving details of the event.
#' @tests
#' expect_error(FAMILY_EVENT_STRUCTURE())
#' expect_error(FAMILY_EVENT_STRUCTURE("TEST"))
#' 
#' expect_equal(FAMILY_EVENT_STRUCTURE("CENS"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "CENS",     ""
#'              ))
#' 
#' expect_equal(FAMILY_EVENT_STRUCTURE("EVEN"), 
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "EVEN",     ""
#'              ))
#' 
#' expect_equal(FAMILY_EVENT_STRUCTURE("EVEN", "Random event"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "EVEN",     "Random event"
#'              ))
#' 
#' expect_equal(FAMILY_EVENT_STRUCTURE("MARR", 
#'                                     family_event_details = FAMILY_EVENT_DETAIL(wife_age_at_event = "20y")),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "MARR",     "Y",
#'                              1, "WIFE",     "",
#'                              2, "AGE",   "20y"
#'              ))
#' @return A tidy tibble containing the FAMILY_EVENT_STRUCTURE part of a GEDCOM file.
FAMILY_EVENT_STRUCTURE <- function(event_type_family,
                                   event_descriptor = character(),
                                   family_event_details = FAMILY_EVENT_DETAIL()) {
  
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

#' Construct the INDIVIDUAL_ATTRIBUTE_STRUCTURE tibble
#' 
#' This function constructs a tibble representation of the INDIVIDUAL_ATTRIBUTE_STRUCTURE from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param individual_event_details An INDIVIDUAL_EVENT_DETAIL() object giving details of the attribute.
#' @tests
#' expect_error(INDIVIDUAL_ATTRIBUTE_STRUCTURE())
#' expect_error(INDIVIDUAL_ATTRIBUTE_STRUCTURE("TEST"))
#' expect_error(INDIVIDUAL_ATTRIBUTE_STRUCTURE("FACT"))
#' expect_error(INDIVIDUAL_ATTRIBUTE_STRUCTURE(c("FACT", "EDUC"), "This is a fact"))
#' expect_error(INDIVIDUAL_ATTRIBUTE_STRUCTURE("FACT", "This is a fact"))
#' expect_error(INDIVIDUAL_ATTRIBUTE_STRUCTURE("IDNO", 123456))
#' 
#' expect_equal(INDIVIDUAL_ATTRIBUTE_STRUCTURE("NATI", "British"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "NATI",     "British"
#'              ))
#' 
#' expect_equal(INDIVIDUAL_ATTRIBUTE_STRUCTURE("NATI", "British", INDIVIDUAL_EVENT_DETAIL(age = "0y")),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "NATI",     "British",
#'                              1,  "AGE",           "0y"
#'              ))
#' @return A tidy tibble containing the INDIVIDUAL_ATTRIBUTE_STRUCTURE part of a GEDCOM file.
INDIVIDUAL_ATTRIBUTE_STRUCTURE <- function(attribute_type,
                                           attribute_descriptor,
                                           individual_event_details = INDIVIDUAL_EVENT_DETAIL()) {
  
  if (length(attribute_type) == 0) return(tibble::tibble())
  if (length(attribute_descriptor) == 0) return(tibble::tibble())
  
  validate_attribute_type(attribute_type, 1)
  if (attribute_type %in% c("IDNO", "NCHI", "NMR", "SSN")) 
    attribute_descriptor <- as.character(attribute_descriptor) 
  
  if (attribute_type == "CAST") validate_caste_name(attribute_descriptor, 1)
  if (attribute_type == "DSCR") validate_physical_description(attribute_descriptor, 1)
  if (attribute_type == "EDUC") validate_scholastic_achievement(attribute_descriptor, 1)
  if (attribute_type == "IDNO") validate_national_id_number(attribute_descriptor, 1)
  if (attribute_type == "NATI") validate_national_or_tribal_origin(attribute_descriptor, 1)
  if (attribute_type == "NCHI") validate_count_of_children(attribute_descriptor, 1)
  if (attribute_type == "NMR") validate_count_of_marriages(attribute_descriptor, 1)
  if (attribute_type == "OCCU") validate_occupation(attribute_descriptor, 1)
  if (attribute_type == "PROP") validate_possessions(attribute_descriptor, 1)
  if (attribute_type == "RELI") validate_religious_affiliation(attribute_descriptor, 1)
  if (attribute_type == "RESI") validate_residence_descriptor(attribute_descriptor, 1)
  if (attribute_type == "SSN") validate_social_security_number(attribute_descriptor, 1)
  if (attribute_type == "TITL") validate_nobility_type_title(attribute_descriptor, 1)
  if (attribute_type == "FACT") validate_attribute_descriptor(attribute_descriptor, 1)
  
  if (attribute_type == "DSCR") {
    
    temp <- split_text(start_level = 0, top_tag = "DSCR", text = attribute_descriptor)
    
  } else {
    
    temp <- tibble::tibble(level = 0, tag = attribute_type, value = attribute_descriptor)
    
  }
  
  temp <- dplyr::bind_rows(temp, 
            individual_event_details %>% add_levels(1)
  )
  
  if (sum(temp$tag %in% c("IDNO", "FACT")) == 1 & sum(temp$tag == "TYPE") == 0)
    stop("IDNO and FACT tags require a event_or_fact_classification to be defined in the event detail.")
  
  temp
}

#' Construct the INDIVIDUAL_EVENT_DETAIL tibble
#' 
#' This function constructs a tibble representation of the INDIVIDUAL_EVENT_DETAIL from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param event_details An EVENT_DETAIL() object giving details of the event.
#' @tests
#' expect_equal(dim(INDIVIDUAL_EVENT_DETAIL()), c(0, 3))  
#' 
#' expect_equal(INDIVIDUAL_EVENT_DETAIL(age = "5y"),
#'              tibble::tribble(~level,  ~tag, ~value,
#'                              0, "AGE",    "5y"
#'              ))
#' @return A tidy tibble containing the INDIVIDUAL_EVENT_DETAIL part of a GEDCOM file.
INDIVIDUAL_EVENT_DETAIL <- function(event_details = EVENT_DETAIL(),
                                    age_at_event = character()) {
  
  age_at_event <- as.character(age_at_event)
  
  validate_age_at_event(age_at_event, 1)
  
  dplyr::bind_rows(
    event_details %>% add_levels(0),
    tibble::tibble(level = 0, tag = "AGE", value = age_at_event)
  )
  
  
}

#' Construct the INDIVIDUAL_EVENT_STRUCTURE tibble
#' 
#' This function constructs a tibble representation of the INDIVIDUAL_EVENT_STRUCTURE from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param individual_event_details An INDIVIDUAL_EVENT_DETAIL() object giving details of the event.
#' @tests
#' expect_error(INDIVIDUAL_EVENT_STRUCTURE())
#' expect_error(INDIVIDUAL_EVENT_STRUCTURE("BLAH"))
#' expect_error(INDIVIDUAL_EVENT_STRUCTURE("ADOP", adopted_by_which_parent = "WHO"))
#' 
#' expect_equal(INDIVIDUAL_EVENT_STRUCTURE("BIRT"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "BIRT",    "Y"
#'              ))
#' 
#' expect_equal(INDIVIDUAL_EVENT_STRUCTURE("CHRA"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "CHRA",     ""
#'              ))
#' 
#' expect_equal(INDIVIDUAL_EVENT_STRUCTURE("BIRT", xref_fam = "@F4@"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "BIRT",    "Y",
#'                              1, "FAMC", "@F4@"
#'              ))
#' 
#' expect_equal(INDIVIDUAL_EVENT_STRUCTURE("ADOP", xref_fam = "@F4@", adopted_by_which_parent = "BOTH"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "ADOP",     "",
#'                              1, "FAMC", "@F4@",
#'                              2, "ADOP", "BOTH"
#'              ))
#' @return A tidy tibble containing the INDIVIDUAL_EVENT_STRUCTURE part of a GEDCOM file.
INDIVIDUAL_EVENT_STRUCTURE <- function(event_type_individual,
                                       individual_event_details = INDIVIDUAL_EVENT_DETAIL(),
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
    dplyr::mutate(value = dplyr::if_else(tag %in% c("BIRT", "CHR", "DEAT"), "Y", value))
  
}


LDS_INDIVIDUAL_ORDINANCE <- function() {
  
  tibble::tibble()
}


LDS_SPOUSE_SEALING <- function() {
  
  tibble::tibble()
}

#' Construct the MULTIMEDIA_LINK tibble
#' 
#' This function constructs a tibble representation of the MULTIMEDIA_LINK from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(MULTIMEDIA_LINK("ref"))
#' expect_error(MULTIMEDIA_LINK(multimedia_file_reference = "ref", source_media_type = "carrier pigeon"))
#' expect_error(MULTIMEDIA_LINK(multimedia_file_reference = "ref", multimedia_format = "jpeg"))
#' 
#' expect_equal(MULTIMEDIA_LINK("@M1@"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "OBJE", "@M1@"
#'              ))
#' 
#' expect_equal(MULTIMEDIA_LINK(multimedia_file_reference = "ref", multimedia_format = "jpg", 
#'                              source_media_type = "electronic"),
#'              tibble::tribble(~level,   ~tag,       ~value,
#'                              0, "OBJE",           "",
#'                              1, "FILE",        "ref",
#'                              2, "FORM",        "jpg",
#'                              3, "MEDI", "electronic"
#'              ))
#' @return A tidy tibble containing the MULTIMEDIA_LINK part of a GEDCOM file.
MULTIMEDIA_LINK <- function(xref_obje = character(),
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
                        tibble::tibble(level = 2, tag = "FORM", value = multimedia_format[i]))
      
      if (length(source_media_type) > 0)
        temp <- dplyr::bind_rows(temp,
                                 tibble::tibble(level = 3, tag = "MEDI", value = source_media_type[i]))
    }
    
    dplyr::bind_rows(temp,
              tibble::tibble(level = 1, tag = "TITL", value = descriptive_title)
    )
  }
}

#' Construct the NOTE_STRUCTURE tibble
#' 
#' This function constructs a tibble representation of the NOTE_STRUCTURE from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(NOTE_STRUCTURE(submitter_text = c("test1", "test2")))
#' 
#' expect_equal(NOTE_STRUCTURE("@T1@"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "NOTE", "@T1@"
#'              ))
#' 
#' expect_equal(NOTE_STRUCTURE(submitter_text = "test text"),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "NOTE", "test text"
#'              ))
#' 
#' expect_equal(NOTE_STRUCTURE(submitter_text = paste0(rep("a", 248), collapse="")),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "NOTE", paste0(rep("a", 248), collapse="")
#'              ))
#' 
#' expect_equal(NOTE_STRUCTURE(submitter_text = paste0(rep("a", 249), collapse="")),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "NOTE", paste0(rep("a", 248), collapse=""),
#'                              1, "CONC", "a"
#'              ))
#' 
#' expect_equal(NOTE_STRUCTURE(submitter_text = paste0(rep("a", 992), collapse="")),
#'              tibble::tribble(~level,   ~tag,      ~value,
#'                              0, "NOTE", paste0(rep("a", 248), collapse=""),
#'                              1, "CONC", paste0(rep("a", 248), collapse=""),
#'                              1, "CONC", paste0(rep("a", 248), collapse=""),
#'                              1, "CONC", paste0(rep("a", 248), collapse="")
#'              ))
#' @return A tidy tibble containing the NOTE_STRUCTURE part of a GEDCOM file.
NOTE_STRUCTURE <- function(xref_note = character(),
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




#' Construct the PERSONAL_NAME_PIECES tibble
#' 
#' This function constructs a tibble representation of the PERSONAL_NAME_PIECES from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_equal(dim(PERSONAL_NAME_PIECES()), c(0, 3))
#' 
#' expect_equal(PERSONAL_NAME_PIECES(name_piece_prefix = "Mr", name_piece_nickname = "J"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "NPFX",   "Mr",
#'                              0, "NICK",    "J"
#'              ))
#' 
#' expect_equal(PERSONAL_NAME_PIECES(name_piece_prefix = "Mr", name_piece_nickname = "J",
#'                                   notes = list(NOTE_STRUCTURE(submitter_text = paste0(rep("a", 992), collapse="")),
#'                                                NOTE_STRUCTURE(submitter_text = paste0(rep("b", 992), collapse="")))),
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
#' @return A tidy tibble containing the PERSONAL_NAME_PIECES part of a GEDCOM file.
PERSONAL_NAME_PIECES <- function(name_piece_prefix = character(),
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

#' Construct the PERSONAL_NAME_STRUCTURE tibble
#' 
#' This function constructs a tibble representation of the PERSONAL_NAME_STRUCTURE from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param name_pieces A PERSONAL_NAME_PIECES() object giving the components of the name.
#' @param phonetic_name_pieces A list of PERSONAL_NAME_PIECES() objects giving the components 
#' of the phonetic name variations.
#' @param romanized_name_pieces A list of PERSONAL_NAME_PIECES() objects giving the components 
#' of the romanized name variations.
#' @tests
#' expect_error(PERSONAL_NAME_STRUCTURE())
#' expect_error(
#'   PERSONAL_NAME_STRUCTURE("Joe Bloggs", 
#'                           name_phonetic_variation = c("Joe Blogs", "Jo Bloggs"))
#' )
#' expect_error(
#'   PERSONAL_NAME_STRUCTURE("Joe Bloggs", 
#'                           name_phonetic_variation = c("Joe Blogs", "Jo Bloggs"),
#'                           phonetic_type = "Can't spell")
#' )
#' expect_error(
#'   PERSONAL_NAME_STRUCTURE("Joe Bloggs", 
#'                           name_phonetic_variation = c("Joe Blogs", "Jo Bloggs"),
#'                           phonetic_type = c("Can't spell", "Can't spell"),
#'                           phonetic_name_pieces = list(PERSONAL_NAME_PIECES(name_piece_given = "Joe", 
#'                                                                            name_piece_surname = "Blogs")))
#' )
#' 
#' expect_equal(PERSONAL_NAME_STRUCTURE("Joe /Bloggs/", 
#'                                      name_pieces = PERSONAL_NAME_PIECES(name_piece_prefix = "Mr",
#'                                                                         name_piece_surname = "Bloggs")),
#'              tibble::tribble(~level,   ~tag,         ~value,
#'                              0, "NAME", "Joe /Bloggs/",
#'                              1, "NPFX",           "Mr",
#'                              1, "SURN",       "Bloggs"
#'              ))
#' 
#' expect_equal(PERSONAL_NAME_STRUCTURE("Joe Bloggs", 
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
#' expect_equal(PERSONAL_NAME_STRUCTURE("Joe Bloggs", 
#'                                      name_phonetic_variation = c("Joe Blogs", "Jo Bloggs"),
#'                                      phonetic_type = c("Can't spell", "Can't spell"),
#'                                      phonetic_name_pieces = 
#'                                        list(PERSONAL_NAME_PIECES(name_piece_given = "Joe", 
#'                                                                  name_piece_surname = "Blogs"),
#'                                             PERSONAL_NAME_PIECES(name_piece_given = "Jo",
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
#' @return A tidy tibble containing the PERSONAL_NAME_STRUCTURE part of a GEDCOM file.
PERSONAL_NAME_STRUCTURE <- function(name_personal,
                                    name_type = character(),
                                    name_pieces = PERSONAL_NAME_PIECES(), 
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

#' Construct the PLACE_STRUCTURE tibble
#' 
#' This function constructs a tibble representation of the PLACE_STRUCTURE from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(PLACE_STRUCTURE())
#' expect_error(PLACE_STRUCTURE("Here", place_latitude = "N51.5", place_longitude = "E0.0"))
#' expect_error(
#'   PLACE_STRUCTURE("London", 
#'                   place_phonetic_variation = c("Lundon", "Lundun"))
#' )
#' expect_error(
#'   PLACE_STRUCTURE("London", 
#'                   place_phonetic_variation = c("Lundon", "Lundun"),
#'                   phonetic_type = "English accent")
#' )
#' 
#' expect_equal(PLACE_STRUCTURE("Greenwich", 
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
#' @return A tidy tibble containing the PLACE_STRUCTURE part of a GEDCOM file.
PLACE_STRUCTURE <- function(place_name,
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

#' Construct the SOURCE_CITATION tibble
#' 
#' This function constructs a tibble representation of the SOURCE_CITATION from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_equal(SOURCE_CITATION("@S1@"),
#'              tibble::tribble(
#'                ~level,   ~tag, ~value,
#'                0, "SOUR", "@S1@"
#'              ))
#' 
#' expect_equal(SOURCE_CITATION("@S1@", where_within_source = 3, event_type_cited_from = "event"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "SOUR",  "@S1@",
#'                              1, "PAGE",     "3",
#'                              1, "EVEN", "event"
#'              ))
#' 
#' expect_equal(SOURCE_CITATION("@S1@", where_within_source = 3, role_in_event = "a role"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "SOUR",  "@S1@",
#'                              1, "PAGE",     "3"
#'              ))
#' 
#' expect_equal(SOURCE_CITATION("@S1@", where_within_source = 3, 
#'                              event_type_cited_from = "event", 
#'                              role_in_event = "a role"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "SOUR",   "@S1@",
#'                              1, "PAGE",      "3",
#'                              1, "EVEN",  "event",
#'                              2, "ROLE", "a role"
#'              ))
#' @return A tidy tibble containing the SOURCE_CITATION part of a GEDCOM file.
SOURCE_CITATION <- function(xref_sour = character(),
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
    validate_date_value(entry_recording_date, 1)
    
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

#' Construct the SOURCE_REPOSITORY_CITATION tibble
#' 
#' This function constructs a tibble representation of the SOURCE_REPOSITORY_CITATION from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(SOURCE_REPOSITORY_CITATION())
#' expect_error(SOURCE_REPOSITORY_CITATION("@R1@", source_media_type = "carrier pigeon"))
#' 
#' expect_equal(SOURCE_REPOSITORY_CITATION("@R1@", source_call_number = c("123", "456"), 
#'                                         source_media_type = "map"),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "REPO", "@R1@",
#'                              1, "CALN",  "123",
#'                              1, "CALN",  "456",
#'                              2, "MEDI",  "map"
#'              ))
#' @return A tidy tibble containing the SOURCE_REPOSITORY_CITATION part of a GEDCOM file.
SOURCE_REPOSITORY_CITATION <- function(xref_repo,
                                       notes = list(),
                                       source_call_number = character(),
                                       source_media_type = character()) {
  
  if (length(xref_repo) == 0) return(tibble::tibble())
  
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


#' Construct the SPOUSE_TO_FAMILY_LINK tibble
#' 
#' This function constructs a tibble representation of the SPOUSE_TO_FAMILY_LINK from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(SPOUSE_TO_FAMILY_LINK())
#' 
#' expect_equal(SPOUSE_TO_FAMILY_LINK("@F2@", list(NOTE_STRUCTURE(submitter_text = "test"))),
#'              tibble::tribble(~level,   ~tag, ~value,
#'                              0, "FAMS", "@F2@",
#'                              1, "NOTE",  "test"
#'              ))
#' @return A tidy tibble containing the SPOUSE_TO_FAMILY_LINK part of a GEDCOM file.
SPOUSE_TO_FAMILY_LINK <- function(xref_fam,
                                  notes = list()) {
  
  if (length(xref_fam) == 0) return(tibble::tibble())
  
  validate_xref(xref_fam, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, tag = "FAMS", value = xref_fam),
    notes %>% dplyr::bind_rows() %>% add_levels(1)
  )
}



