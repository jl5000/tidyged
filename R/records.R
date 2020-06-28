# The structure of the functions in this file and the structures file all have a similar pattern
# First any inputs that could be numbers are converted to characters
# Then inputs are then checked to ensure they do not breach size limits
#   Structures are not checked for character length
#   Note values which could be split up over several lines are not checked for character length
# Then the outputs dataframe is constructed:
# Inputs which are not structures or lists are placed straight into tibbles
# Inputs which are lists of dataframes are first binded by row, and then levels pushed down
# Inputs which are structures have their levels pushed down
# Inputs which are long text strings are split up intow several rows using split_text
# Finally, if certain subordinate tags are not used, the parent tags are removed
# For records, an additional finalising step is performed which fills missing ids


#' Construct the HEADER_SECTION tibble
#' 
#' This function constructs a tibble representation of the HEADER_SECTION from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param business_address An ADDRESS_STRUCTURE() object giving the address of the business.
#' @tests
#' expect_error(HEADER_SECTION())
#' expect_error(HEADER_SECTION("@1@"))
#' expect_error(HEADER_SECTION("@1@", approved_system_id = "system"))
#' expect_error(HEADER_SECTION("@1@", approved_system_id = "system", character_set = "red"))
#' 
#' expect_equal(HEADER_SECTION("@1@", approved_system_id = "system", character_set = "UTF-8",
#'                             xref_subn = "@2@", system_version_number = "1.0", name_of_product = "R",
#'                             copyright_gedcom_file = "Do not copy", language = "English",
#'                             gedcom_content_description = "This is a gedcom file"),
#'              tibble::tribble(~level,  ~id,   ~tag,                  ~value,
#'                              0, "HD", "HEAD",                      "",
#'                              1, "HD", "SOUR",                "system",
#'                              2, "HD", "VERS",                   "1.0",
#'                              2, "HD", "NAME",                     "R",
#'                              1, "HD", "SUBM",                   "@1@",
#'                              1, "HD", "SUBN",                   "@2@",
#'                              1, "HD", "COPR",           "Do not copy",
#'                              1, "HD", "GEDC",                      "",
#'                              2, "HD", "VERS",                 "5.5.1",
#'                              2, "HD", "FORM",        "Lineage-Linked",
#'                              1, "HD", "CHAR",                 "UTF-8",
#'                              1, "HD", "LANG",               "English",
#'                              1, "HD", "NOTE", "This is a gedcom file"
#'              ))
#' 
#' expect_equal(HEADER_SECTION("@1@", approved_system_id = "system", character_set = "UTF-8",
#'                             xref_subn = "@2@", system_version_number = "1.0", name_of_product = "R",
#'                             name_of_business = "RStudio", business_address = ADDRESS_STRUCTURE(c("Street", "City", "State")),
#'                             name_of_source_data = "Source text", publication_date_source_data = date_exact(5, 9, 2005),
#'                             copyright_source_data = "Source is protected", receiving_system_name = "Windows",
#'                             transmission_date = date_exact(15, 10, 2020), file_name = "test.ged",
#'                             copyright_gedcom_file = "Do not copy", language = "English", place_hierarchy = "here",
#'                             gedcom_content_description = "This is a gedcom file"),
#'              tibble::tribble(~level,  ~id,   ~tag,                  ~value,
#'                              0, "HD", "HEAD",                      "",
#'                              1, "HD", "SOUR",                "system",
#'                              2, "HD", "VERS",                   "1.0",
#'                              2, "HD", "NAME",                     "R",
#'                              2, "HD", "CORP",               "RStudio",
#'                              3, "HD", "ADDR",                "Street",
#'                              4, "HD", "CONT",                  "City",
#'                              4, "HD", "CONT",                 "State",
#'                              4, "HD", "ADR1",                  "City",
#'                              4, "HD", "ADR2",                 "State",
#'                              2, "HD", "DATA",           "Source text",
#'                              3, "HD", "DATE",            "5 SEP 2005",
#'                              3, "HD", "COPR",   "Source is protected",
#'                              1, "HD", "DEST",               "Windows",
#'                              1, "HD", "DATE",           "15 OCT 2020",
#'                              1, "HD", "SUBM",                   "@1@",
#'                              1, "HD", "SUBN",                   "@2@",
#'                              1, "HD", "FILE",              "test.ged",
#'                              1, "HD", "COPR",           "Do not copy",
#'                              1, "HD", "GEDC",                      "",
#'                              2, "HD", "VERS",                 "5.5.1",
#'                              2, "HD", "FORM",        "Lineage-Linked",
#'                              1, "HD", "CHAR",                 "UTF-8",
#'                              1, "HD", "LANG",               "English",
#'                              1, "HD", "PLAC",                      "",
#'                              2, "HD", "FORM",                  "here",
#'                              1, "HD", "NOTE", "This is a gedcom file"
#'              ))
#' @return A tidy tibble containing the HEADER part of a GEDCOM file.
#' @export
HEADER_SECTION <- function(xref_subm,
                           approved_system_id,
                           character_set,
                           xref_subn = character(),
                           system_version_number = character(),
                           name_of_product = character(),
                           name_of_business = character(),
                           business_address = ADDRESS_STRUCTURE(character()),
                           name_of_source_data = character(),
                           publication_date_source_data = date_exact(),
                           copyright_source_data = character(),
                           receiving_system_name = character(),
                           transmission_date = date_exact(),
                           transmission_time = character(),
                           file_name = character(),
                           copyright_gedcom_file = character(),
                           character_set_version_number = character(),
                           language_of_text = character(),
                           place_hierarchy = character(),
                           gedcom_content_description = character()) {
  
  system_version_number <- as.character(system_version_number)
  character_set_version_number <- as.character(character_set_version_number)
  
  validate_xref(xref_subm, 1)
  validate_approved_system_id(approved_system_id, 1)
  approved_system_id <- stringr::str_replace_all(approved_system_id, " ", "_")
  validate_character_set(character_set, 1)
  validate_xref(xref_subn, 1)
  validate_version_number(system_version_number, 1)
  validate_name_of_product(name_of_product, 1)
  validate_name_of_business(name_of_business, 1)
  validate_name_of_source_data(name_of_source_data, 1)
  validate_date_exact(publication_date_source_data, 1)
  validate_copyright_source_data(copyright_source_data, 1)
  validate_receiving_system_name(receiving_system_name, 1)
  validate_date_exact(transmission_date, 1)
  validate_time_value(transmission_time, 1)
  validate_file_name(file_name, 1)
  validate_copyright_gedcom_file(copyright_gedcom_file, 1)
  validate_version_number(character_set_version_number, 1)
  validate_language_of_text(language_of_text, 1)
  validate_place_hierarchy(place_hierarchy, 1)
  validate_gedcom_content_description(gedcom_content_description, 1)
  
  temp <- dplyr::bind_rows(
    tibble::tibble(level = 0, id = "HD", tag = "HEAD", value = ""),
    tibble::tibble(level = 1, tag = "SOUR", value = approved_system_id),
    tibble::tibble(level = 2, tag = "VERS", value = system_version_number),
    tibble::tibble(level = 2, tag = "NAME", value = name_of_product),
    tibble::tibble(level = 2, tag = "CORP", value = name_of_business),
    business_address %>% add_levels(3),
    tibble::tibble(level = 2, tag = "DATA", value = name_of_source_data),
    tibble::tibble(level = 3, tag = "DATE", value = publication_date_source_data),
    split_text(start_level = 3, top_tag = "COPR", text = copyright_source_data, char_limit = 90),
    tibble::tibble(level = 1, tag = "DEST", value = receiving_system_name),
    tibble::tibble(level = 1, tag = "DATE", value = transmission_date),
    tibble::tibble(level = 2, tag = "TIME", value = transmission_time),
    tibble::tibble(level = 1, tag = "SUBM", value = xref_subm),
    tibble::tibble(level = 1, tag = "SUBN", value = xref_subn),
    tibble::tibble(level = 1, tag = "FILE", value = file_name),
    tibble::tibble(level = 1, tag = "COPR", value = copyright_gedcom_file),
    tibble::tibble(level = 1, tag = "GEDC", value = ""),
    tibble::tibble(level = 2, tag = "VERS", value = "5.5.1"),
    tibble::tibble(level = 2, tag = "FORM", value = "Lineage-Linked"),
    tibble::tibble(level = 1, tag = "CHAR", value = character_set),
    tibble::tibble(level = 2, tag = "VERS", value = character_set_version_number),
    tibble::tibble(level = 1, tag = "LANG", value = language_of_text),
    tibble::tibble(level = 1, tag = "PLAC", value = ""),
    tibble::tibble(level = 2, tag = "FORM", value = place_hierarchy),
    split_text(start_level = 1, top_tag = "NOTE", text = gedcom_content_description)
  ) %>% 
    finalise()
  
  # There should only be one FORM (Lineage linked) if no place_hierarchy defined
  if (sum(temp$tag == "FORM") == 1) {
    dplyr::filter(temp, tag != "PLAC")
  } else {
    temp
  }
  
  
}


#' Construct the FAMILY_RECORD tibble
#' 
#' This function constructs a tibble representation of the FAMILY_RECORD from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param events A list of FAMILY_EVENT_STRUCTURE() objects giving events associated with this family.
#' @param xref_husb An xref ID of the husband.
#' @param xref_wife An xref ID of the wife.
#' @param xrefs_chil A vector of xref IDs of children in this family.
#' @param xrefs_subm A vector of xref IDs of submitters of this record.
#' @param lds_spouse_sealings Not used.
#' @tests
#' expect_error(FAMILY_RECORD("@F1@", user_reference_number = 123:125, user_reference_type = letters[1:2]))
#' expect_equal(FAMILY_RECORD("@F1@"),
#'              tibble::tribble(~level,  ~id,   ~tag,                  ~value,
#'                              0, "@F1@", "FAM",                      "",
#'                              1, "@F1@", "CHAN",                     "",
#'                              2, "@F1@", "DATE", toupper(format(Sys.Date(), "%d %b %Y"))
#'              ))
#' @return A tidy tibble containing a FAMILY_RECORD part of a GEDCOM file.
#' @export
FAMILY_RECORD <- function(xref_fam,
                          restriction_notice = character(),
                          events = list(),
                          xref_husb = character(),
                          xref_wife = character(),
                          xrefs_chil = character(),
                          count_of_children = character(),
                          xrefs_subm = character(),
                          lds_spouse_sealings = list(),
                          user_reference_number = character(),
                          user_reference_type = character(),
                          automated_record_id = character(),
                          date_changed = CHANGE_DATE(),
                          notes = list(),
                          source_citations = list(),
                          multimedia_links = list()){
  
  children_count <- as.character(count_of_children)
  user_reference_number <- as.character(user_reference_number)
  
  if (length(user_reference_type) > 0 & length(user_reference_type) != length(user_reference_number))
    stop("The number of user reference types must be the same as the number of user reference numbers")
  
  validate_xref(xref_fam, 1)
  validate_restriction_notice(restriction_notice, 1)
  validate_xref(xref_husb, 1)
  validate_xref(xref_wife, 1)
  validate_xref(xrefs_chil, 100)
  validate_count_of_children(count_of_children, 1)
  validate_xref(xrefs_subm, 1000)
  validate_user_reference_number(user_reference_number, 1000)
  validate_user_reference_type(user_reference_type, 1000)
  validate_automated_record_id(automated_record_id, 1)
  
  
  temp <- dplyr::bind_rows(
    tibble::tibble(level = 0, id = xref_fam, tag = "FAM", value = ""),
    tibble::tibble(level = 1, tag = "RESN", value = restriction_notice),
    events %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "HUSB", value = xref_husb),
    tibble::tibble(level = 1, tag = "WIFE", value = xref_wife),
    tibble::tibble(level = 1, tag = "CHIL", value = xrefs_chil),
    tibble::tibble(level = 1, tag = "NCHI", value = count_of_children),
    tibble::tibble(level = 1, tag = "SUBM", value = xrefs_subm),
    lds_spouse_sealings %>% dplyr::bind_rows() %>% add_levels(1)
  )
  
  for (i in seq_along(user_reference_number)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "REFN", value = user_reference_number[i]))
    
    if (length(user_reference_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type[i])) 
  }
  
  dplyr::bind_rows(temp,
                   tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
                   date_changed %>% add_levels(1),
                   notes %>% dplyr::bind_rows() %>% add_levels(1),
                   source_citations %>% dplyr::bind_rows() %>% add_levels(1),
                   multimedia_links %>% dplyr::bind_rows() %>% add_levels(1)
  ) %>% 
    finalise()
  
}


#' Construct the INDIVIDUAL_RECORD tibble
#' 
#' This function constructs a tibble representation of the INDIVIDUAL_RECORD from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param names A list of PERSONAL_NAME_STRUCTURE() objects giving the names associated with this individual.
#' @param events A list of INDIVIDUAL_EVENT_STRUCTURE() objects giving the events associated with
#' this individual.
#' @param attributes A list of INDIVIDUAL_ATTRIBUTE_STRUCTURE() objects giving the attributes associated 
#' with this individual.
#' @param ordinance Not used.
#' @param child_to_family_links A list of CHILD_TO_FAMILY_LINK() objects giving the details of families
#' this individual is a child of.
#' @param spouse_to_family_links A list of SPOUSE_TO_FAMILY_LINK() objects giving the details of families
#' this individual is a spouse of.
#' @param xrefs_subm A vector of xref IDs of submitters of this record.
#' @param associations A list of ASSOCIATION_STRUCTURE() objects giving the details of individuals this
#' individual is associated with.
#' @param xrefs_alia A vector of xref IDs of individual aliases of this individual.
#' @param xrefs_subm_interested_in_ancestors A vector of xref IDs of submitters with an interest in
#' ancestors of this individual.
#' @param xrefs_subm_interested_in_descendents A vector of xref IDs of submitters with an interest in
#' ancestors of this individual.
#' @tests
#' expect_error(INDIVIDUAL_RECORD("@I1@", user_reference_number = 123:125, user_reference_type = letters[1:2]))
#' expect_equal(INDIVIDUAL_RECORD("@I1@"),
#'              tibble::tribble(~level,  ~id,   ~tag,                  ~value,
#'                              0, "@I1@", "INDI",                      "",
#'                              1, "@I1@", "CHAN",                      "",
#'                              2, "@I1@", "DATE", toupper(format(Sys.Date(), "%d %b %Y"))
#'              ))
#' @return A tidy tibble containing an INDIVIDUAL_RECORD part of a GEDCOM file.
#' @export
INDIVIDUAL_RECORD <- function(xref_indi,
                              restriction_notice = character(),
                              names = list(),
                              sex_value = character(),
                              events = list(),
                              attributes = list(),
                              ordinance = list(),
                              child_to_family_links = list(),
                              spouse_to_family_links = list(),
                              xrefs_subm = character(),
                              associations = list(),
                              xrefs_alia = character(),
                              xrefs_subm_interested_in_ancestors = character(),
                              xrefs_subm_interested_in_descendents = character(),
                              permanent_record_file_number = character(),
                              ancestral_file_number = character(),
                              user_reference_number = character(),
                              user_reference_type = character(),
                              automated_record_id = character(),
                              date_changed = CHANGE_DATE(),
                              notes = list(),
                              source_citations = list(),
                              multimedia_links = list()) {
  
  permanent_record_file_number <- as.character(permanent_record_file_number)
  ancestral_file_number <- as.character(ancestral_file_number)
  user_reference_number <- as.character(user_reference_number)
  
  if (length(user_reference_type) > 0 & length(user_reference_type) != length(user_reference_number))
    stop("The number of user reference types must be the same as the number of user reference numbers")
  
  validate_xref(xref_indi, 1)
  validate_restriction_notice(restriction_notice, 1)
  validate_sex_value(sex_value, 1)
  validate_xref(xrefs_subm, 1000)
  validate_xref(xrefs_alia, 1000)
  validate_xref(xrefs_subm_interested_in_ancestors, 1000)
  validate_xref(xrefs_subm_interested_in_descendents, 1000)
  validate_permanent_record_file_number(permanent_record_file_number, 1)
  validate_ancestral_file_number(ancestral_file_number, 1)
  validate_user_reference_number(user_reference_number, 1000)
  validate_user_reference_type(user_reference_type, 1000)
  validate_automated_record_id(automated_record_id, 1)
  
  
  temp <- dplyr::bind_rows(
    tibble::tibble(level = 0, id = xref_indi, tag = "INDI", value = ""),
    tibble::tibble(level = 1, tag = "RESN", value = restriction_notice),
    names %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "SEX", value = sex_value),
    events %>% dplyr::bind_rows() %>% add_levels(1),
    attributes %>% dplyr::bind_rows() %>% add_levels(1),
    ordinance %>% dplyr::bind_rows() %>% add_levels(1),
    child_to_family_links %>% dplyr::bind_rows() %>% add_levels(1),
    spouse_to_family_links %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "SUBM", value = xrefs_subm),
    associations %>% dplyr::bind_rows() %>%  add_levels(1),
    tibble::tibble(level = 1, tag = "ALIA", value = xrefs_alia),
    tibble::tibble(level = 1, tag = "ANCI", value = xrefs_subm_interested_in_ancestors),
    tibble::tibble(level = 1, tag = "DESI", value = xrefs_subm_interested_in_descendents),
    tibble::tibble(level = 1, tag = "RFN", value = permanent_record_file_number),
    tibble::tibble(level = 1, tag = "AFN", value = ancestral_file_number)
  )
  
  for (i in seq_along(user_reference_number)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "REFN", value = user_reference_number[i]))
    
    if (length(user_reference_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type[i])) 
  }
  
  dplyr::bind_rows(temp,
                   tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
                   date_changed %>% add_levels(1),
                   notes %>% dplyr::bind_rows() %>% add_levels(1),
                   source_citations %>% dplyr::bind_rows() %>% add_levels(1),
                   multimedia_links %>% dplyr::bind_rows() %>% add_levels(1)
  ) %>% 
    finalise()
  
}


#' Construct the MULTIMEDIA_RECORD tibble
#' 
#' This function constructs a tibble representation of the MULTIMEDIA_RECORD from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(MULTIMEDIA_RECORD("@M1@", "file_ref", "jpg",
#'                                user_reference_number = 123:125, user_reference_type = letters[1:2]))
#' expect_equal(MULTIMEDIA_RECORD("@M1@", "file_ref", "jpg"),
#'              tibble::tribble(~level,  ~id,   ~tag,                  ~value,
#'                              0, "@M1@", "OBJE",                      "",
#'                              1, "@M1@", "FILE",              "file_ref",
#'                              2, "@M1@", "FORM",                   "jpg",
#'                              1, "@M1@", "CHAN",                      "",
#'                              2, "@M1@", "DATE", toupper(format(Sys.Date(), "%d %b %Y"))
#'              ))
#' @return A tidy tibble containing a MULTIMEDIA_RECORD part of a GEDCOM file.
#' @export
MULTIMEDIA_RECORD <- function(xref_obje,
                              multimedia_file_reference,
                              multimedia_format,
                              source_media_type = character(),
                              descriptive_title = character(),
                              user_reference_number = character(),
                              user_reference_type = character(),
                              automated_record_id = character(),
                              notes = list(),
                              source_citations = list(),
                              date_changed = CHANGE_DATE()){
  
  multimedia_file_reference <- as.character(multimedia_file_reference)
  user_reference_number <- as.character(user_reference_number)
  
  if (length(multimedia_file_reference) != length(multimedia_format))
    stop("Each file reference requires a media format")
  
  if (length(source_media_type) > 0 & length(source_media_type) != length(multimedia_file_reference))
    stop("The number of media types must be the same as the number of file references")
  
  if (length(descriptive_title) > 0 & length(descriptive_title) != length(multimedia_file_reference))
    stop("The number of desciptive titles must be the same as the number of file references")
  
  if (length(user_reference_type) > 0 & length(user_reference_type) != length(user_reference_number))
    stop("The number of user reference types must be the same as the number of user reference numbers")
  
  validate_xref(xref_obje, 1)
  validate_multimedia_file_reference(multimedia_file_reference, 1000)
  validate_multimedia_format(multimedia_format, 1000)
  validate_source_media_type(source_media_type, 1000)
  validate_descriptive_title(descriptive_title, 1000)
  validate_user_reference_number(user_reference_number, 1000)
  validate_user_reference_type(user_reference_type, 1000)
  validate_automated_record_id(automated_record_id, 1)
  
  temp <- tibble::tibble(level = 0, id = xref_obje, tag = "OBJE", value = "") 
    
  for (i in seq_along(multimedia_file_reference)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "FILE", value = multimedia_file_reference[i]),
                             tibble::tibble(level = 2, tag = "FORM", value = multimedia_format[i]))
    
    if (length(source_media_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 3, tag = "TYPE", value = source_media_type[i]))
    if (length(descriptive_title) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TITL", value = descriptive_title[i]))
  }
    
  for (i in seq_along(user_reference_number)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "REFN", value = user_reference_number[i]))
    
    if (length(user_reference_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type[i])) 
  }
    
  dplyr::bind_rows(temp,
                   tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
                   notes %>% dplyr::bind_rows() %>% add_levels(1),
                   source_citations %>% dplyr::bind_rows() %>% add_levels(1),
                   date_changed %>% add_levels(1)
  ) %>% 
    finalise()
  
  
}


#' Construct the NOTE_RECORD tibble
#' 
#' This function constructs a tibble representation of the NOTE_RECORD from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_error(NOTE_RECORD("@N1@", "This is a note",
#'                                user_reference_number = 123:125, user_reference_type = letters[1:2]))
#' expect_equal(NOTE_RECORD("@N1@", "This is a note"),
#'              tibble::tribble(~level,  ~id,   ~tag,                  ~value,
#'                              0, "@N1@", "NOTE",        "This is a note",
#'                              1, "@N1@", "CHAN",                      "",
#'                              2, "@N1@", "DATE", toupper(format(Sys.Date(), "%d %b %Y"))
#'              ))
#' @return A tidy tibble containing a NOTE_RECORD part of a GEDCOM file.
#' @export
NOTE_RECORD <- function(xref_note,
                        submitter_text,
                        user_reference_number = character(),
                        user_reference_type = character(),
                        automated_record_id = character(),
                        source_citations = list(),
                        date_changed = CHANGE_DATE()){
  
  user_reference_number <- as.character(user_reference_number)
  
  if (length(user_reference_type) > 0 & length(user_reference_type) != length(user_reference_number))
    stop("The number of user reference types must be the same as the number of user reference numbers")
  
  validate_xref(xref_note, 1)
  validate_submitter_text(submitter_text, 1)
  validate_user_reference_number(user_reference_number, 1000)
  validate_user_reference_type(user_reference_type, 1000)
  validate_automated_record_id(automated_record_id, 1)
  
  temp <- split_text(start_level = 0, top_tag = "NOTE", text = submitter_text)
  
  for (i in seq_along(user_reference_number)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "REFN", value = user_reference_number[i]))
    
    if (length(user_reference_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type[i])) 
  }
  
  dplyr::bind_rows(temp,
                   tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
                   source_citations %>% dplyr::bind_rows() %>% add_levels(1),
                   date_changed %>% add_levels(1)
  ) %>% 
    dplyr::mutate(id = dplyr::if_else(tag == "NOTE", xref_note, NA_character_),
                  .after = level) %>% 
    finalise()
  
  
}


#' Construct the REPOSITORY_RECORD tibble
#' 
#' This function constructs a tibble representation of the REPOSITORY_RECORD from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param address An ADDRESS_STRUCTURE() object giving details of the repository address.
#' @tests
#' expect_error(REPOSITORY_RECORD("@R1@", "Repo name",
#'                                user_reference_number = 123:125, user_reference_type = letters[1:2]))
#' expect_equal(REPOSITORY_RECORD("@R1@", "Repo name"),
#'              tibble::tribble(~level,  ~id,   ~tag,                  ~value,
#'                              0, "@R1@", "REPO",                      "",
#'                              1, "@R1@", "NAME",             "Repo name",
#'                              1, "@R1@", "CHAN",                      "",
#'                              2, "@R1@", "DATE", toupper(format(Sys.Date(), "%d %b %Y"))
#'              ))
#' @return A tidy tibble containing a REPOSITORY_RECORD part of a GEDCOM file.
#' @export
REPOSITORY_RECORD <- function(xref_repo,
                              name_of_repository,
                              address = ADDRESS_STRUCTURE(character()),
                              notes = list(),
                              user_reference_number = character(),
                              user_reference_type = character(),
                              automated_record_id = character(),
                              date_changed = CHANGE_DATE()){

  user_reference_number <- as.character(user_reference_number)
  
  if (length(user_reference_type) > 0 & length(user_reference_type) != length(user_reference_number))
    stop("The number of user reference types must be the same as the number of user reference numbers")
  
  validate_xref(xref_repo, 1)
  validate_name_of_repository(name_of_repository, 1)
  validate_user_reference_number(user_reference_number, 1000)
  validate_user_reference_type(user_reference_type, 1000)
  validate_automated_record_id(automated_record_id, 1)
  
  temp <- dplyr::bind_rows(
    tibble::tibble(level = 0, id = xref_repo, tag = "REPO", value = ""),
    tibble::tibble(level = 1, tag = "NAME", value = name_of_repository),
    address %>% add_levels(1),
    notes %>% dplyr::bind_rows() %>% add_levels(1)
  ) %>% 
    finalise()
  
  for (i in seq_along(user_reference_number)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "REFN", value = user_reference_number[i]))
    
    if (length(user_reference_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type[i])) 
  }
    
  dplyr::bind_rows(temp,
                   tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
                   date_changed %>% add_levels(1)
  ) %>% 
    finalise()
}


#' Construct the SOURCE_RECORD tibble
#' 
#' This function constructs a tibble representation of the SOURCE_RECORD from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param date_period_covered A date_period() object associated with the period covered by the course. 
#' @param data_notes A list of NOTE_STRUCTURE() objects associated with the data in this source.
#' @tests
#' expect_error(SOURCE_RECORD("@S1@",
#'                            user_reference_number = 123:125, user_reference_type = letters[1:2]))
#' expect_equal(SOURCE_RECORD("@S1@"),
#'              tibble::tribble(~level,  ~id,   ~tag,                  ~value,
#'                              0, "@S1@", "SOUR",                      "",
#'                              1, "@S1@", "CHAN",                      "",
#'                              2, "@S1@", "DATE", toupper(format(Sys.Date(), "%d %b %Y"))
#'              ))
#' @return A tidy tibble containing a SOURCE_RECORD part of a GEDCOM file.
#' @export
SOURCE_RECORD <- function(xref_sour,
                          events_recorded = character(),
                          date_period_covered = date_period(),
                          source_jurisdiction_place = character(),
                          responsible_agency = character(),
                          data_notes = list(),
                          source_originator = character(),
                          source_descriptive_title = character(),
                          source_filed_by_entry = character(),
                          source_publication_facts = character(),
                          text_from_source = character(),
                          source_repository_citations = list(),
                          user_reference_number = character(),
                          user_reference_type = character(),
                          automated_record_id = character(),
                          date_changed = CHANGE_DATE(),
                          notes = list(),
                          multimedia_links = list()){
  
  user_reference_number <- as.character(user_reference_number)
  
  if (length(user_reference_type) > 0 & length(user_reference_type) != length(user_reference_number))
    stop("The number of user reference types must be the same as the number of user reference numbers")
  
  validate_xref(xref_sour, 1)
  validate_events_recorded(events_recorded, 1000)
  validate_date_period_covered(date_period_covered, 1)
  validate_source_jurisdiction_place(source_jurisdiction_place, 1)
  validate_responsible_agency(responsible_agency, 1)
  validate_source_originator(source_originator, 1)
  validate_source_descriptive_title(source_descriptive_title, 1)
  validate_source_filed_by_entry(source_filed_by_entry, 1)
  validate_source_publication_facts(source_publication_facts, 1)
  validate_text_from_source(text_from_source, 1)
  validate_user_reference_number(user_reference_number, 1000)
  validate_user_reference_type(user_reference_type, 1000)
  validate_automated_record_id(automated_record_id, 1)
  
  temp <- dplyr::bind_rows(
    tibble::tibble(level = 0, id = xref_sour, tag = "SOUR", value = ""),
    tibble::tibble(level = 1, tag = "DATA", value = ""),
    tibble::tibble(level = 2, tag = "EVEN", value = events_recorded),
    tibble::tibble(level = 3, tag = "DATE", value = date_period_covered),
    tibble::tibble(level = 3, tag = "PLAC", value = source_jurisdiction_place),
    tibble::tibble(level = 2, tag = "AGNC", value = responsible_agency),
    data_notes %>% dplyr::bind_rows() %>% add_levels(2),
    split_text(start_level = 1, top_tag = "AUTH", text = source_originator),
    split_text(start_level = 1, top_tag = "TITL", text = source_descriptive_title),
    tibble::tibble(level = 1, tag = "ABBR", value = source_filed_by_entry),
    split_text(start_level = 1, top_tag = "PUBL", text = source_publication_facts),
    split_text(start_level = 1, top_tag = "TEXT", text = text_from_source),
    source_repository_citations %>% dplyr::bind_rows() %>% add_levels(1)
    )
  
  
  for (i in seq_along(user_reference_number)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "REFN", value = user_reference_number[i]))
    
    if (length(user_reference_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type[i])) 
  }
  
  temp <- dplyr::bind_rows(temp,
                           tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
                           date_changed %>% add_levels(1),
                           notes %>% dplyr::bind_rows() %>% add_levels(1),
                           multimedia_links %>% dplyr::bind_rows() %>% add_levels(1)
  )
  
  if (length(date_period_covered) + length(source_jurisdiction_place) == 0)
    temp <- dplyr::filter(temp, tag != "EVEN")
  
  if (length(events_recorded) + length(responsible_agency) + length(data_notes) == 0)
    temp <- dplyr::filter(temp, tag != "DATA")
  
  finalise(temp)
  
}


#' Construct the SUBMISSION_RECORD tibble
#' 
#' This function constructs a tibble representation of the SUBMISSION_RECORD from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @tests
#' expect_equal(SUBMISSION_RECORD("@S1@"),
#'              tibble::tribble(~level,  ~id,   ~tag,                  ~value,
#'                              0, "@S1@", "SUBN",                      "",
#'                              1, "@S1@", "CHAN",                      "",
#'                              2, "@S1@", "DATE", toupper(format(Sys.Date(), "%d %b %Y"))
#'              ))
#' @return A tidy tibble containing a SUBMISSION_RECORD part of a GEDCOM file.
#' @export
SUBMISSION_RECORD <- function(xref_subn,
                              xref_subm = character(),
                              name_of_family_file = character(),
                              temple_code = character(),
                              generations_of_ancestors = character(),
                              generations_of_descendants = character(),
                              ordinance_process_flag = character(),
                              automated_record_id = character(),
                              notes = list(),
                              date_changed = CHANGE_DATE()){
 
  generations_of_ancestors <- as.character(generations_of_ancestors)
  generations_of_descendants <- as.character(generations_of_descendants)
  
  validate_xref(xref_subn, 1)
  validate_xref(xref_subm, 1)
  validate_name_of_family_file(name_of_family_file, 1)
  validate_temple_code(temple_code, 1)
  validate_generations_of_ancestors(generations_of_ancestors, 1)
  validate_generations_of_descendants(generations_of_descendants, 1)
  validate_ordinance_process_flag(ordinance_process_flag, 1)
  validate_automated_record_id(automated_record_id, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, id = xref_subn, tag = "SUBN", value = ""),
    tibble::tibble(level = 1, tag = "SUBM", value = xref_subm),
    tibble::tibble(level = 1, tag = "FAMF", value = name_of_family_file),
    tibble::tibble(level = 1, tag = "TEMP", value = temple_code),
    tibble::tibble(level = 1, tag = "ANCE", value = generations_of_ancestors),
    tibble::tibble(level = 1, tag = "DESC", value = generations_of_descendants),
    tibble::tibble(level = 1, tag = "ORDI", value = ordinance_process_flag),
    tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
    notes %>% dplyr::bind_rows() %>% add_levels(1),
    date_changed %>% add_levels(1)
  ) %>% 
    finalise()
  
   
}


#' Construct the SUBMITTER_RECORD tibble
#' 
#' This function constructs a tibble representation of the SUBMITTER_RECORD from the GEDCOM 5.5.1
#' specification.
#'
#' @inheritParams parameter_definitions
#' @param address An ADDRESS_STRUCTURE() object giving address details of the submitter.
#' @tests
#' expect_equal(SUBMITTER_RECORD("@S1@", "Joe Bloggs"),
#'              tibble::tribble(~level,  ~id,   ~tag,                  ~value,
#'                              0, "@S1@", "SUBM",                      "",
#'                              1, "@S1@", "NAME",            "Joe Bloggs",
#'                              1, "@S1@", "CHAN",                      "",
#'                              2, "@S1@", "DATE", toupper(format(Sys.Date(), "%d %b %Y"))
#'              ))
#' @return A tidy tibble containing a SUBMITTER_RECORD part of a GEDCOM file.
#' @export
SUBMITTER_RECORD <- function(xref_subm,
                             submitter_name,
                             address = ADDRESS_STRUCTURE(character()),
                             multimedia_links = list(),
                             language_preference = character(),
                             submitter_registered_rfn = character(),
                             automated_record_id = character(),
                             notes = list(),
                             date_changed = CHANGE_DATE()){
  
  submitter_registered_rfn <- as.character(submitter_registered_rfn)
  
  validate_xref(xref_subm, 1)
  validate_submitter_name(submitter_name, 1)
  validate_language_preference(language_preference, 3)
  validate_submitter_registered_rfn(submitter_registered_rfn, 1)
  validate_automated_record_id(automated_record_id, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, id = xref_subm, tag = "SUBM", value = ""),
    tibble::tibble(level = 1, tag = "NAME", value = submitter_name),
    address %>% add_levels(1),
    multimedia_links %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "LANG", value = language_preference),
    tibble::tibble(level = 1, tag = "RFN", value = submitter_registered_rfn),
    tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
    notes %>% dplyr::bind_rows() %>% add_levels(1),
    date_changed %>% add_levels(1)
  ) %>% 
    finalise()
  
  
}

#' Construct the FOOTER_SECTION tibble
#' 
#' This function constructs a tibble representation of the FOOTER_SECTION from the GEDCOM 5.5.1
#' specification.
#'
#' @tests
#' expect_equal(FOOTER_SECTION(),
#'              tibble::tribble(~level,  ~id,   ~tag, ~value,
#'                              0, "TR", "TRLR",     ""
#'              ))
#' @return A tidy tibble containing a FOOTER_SECTION part of a GEDCOM file.
#' @export
FOOTER_SECTION <- function(){
  tibble::tibble(level = 0, id = "TR", tag = "TRLR", value = "") %>% 
    finalise()
}
