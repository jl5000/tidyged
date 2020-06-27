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
#' 
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
  
  children_count <- as.character(children_count)
  user_reference_number <- as.character(user_reference_number)
  #TODO: checks for user ref number/type
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
  
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, id = xref_fam, tag = "FAM"),
    tibble::tibble(level = 1, tag = "RESN", value = restriction_notice),
    events %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "HUSB", value = xref_husb),
    tibble::tibble(level = 1, tag = "WIFE", value = xref_wife),
    tibble::tibble(level = 1, tag = "CHIL", value = xrefs_chil),
    tibble::tibble(level = 1, tag = "NCHI", value = count_of_children),
    tibble::tibble(level = 1, tag = "SUBM", value = xrefs_subm),
    lds_spouse_sealings %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "REFN", value = user_reference_number),
    tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type),
    tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
    date_changed %>% add_levels(1),
    notes %>% dplyr::bind_rows() %>% add_levels(1),
    source_citations %>% dplyr::bind_rows() %>% add_levels(1),
    multimedia_links %>% dplyr::bind_rows() %>% add_levels(1)
  ) %>% 
    finalise()
  
  
}

INDIVIDUAL_RECORD <- function(individual_ref,
                              restriction_notice = character(),
                              names = list(),
                              sex = character(),
                              events = list(),
                              attributes = list(),
                              ordinance = list(),
                              child_to_family_links = list(),
                              spouse_to_family_links = list(),
                              submitter_refs = character(),
                              associations = list(),
                              alias_refs = character(),
                              submitters_interested_in_ancestors = character(),
                              submitters_interested_in_descendents = character(),
                              record_file_number = character(),
                              ancestral_file_number = character(),
                              user_reference_number = character(),
                              user_reference_type = character(),
                              automated_record_id = character(),
                              date_changed = CHANGE_DATE(),
                              notes = list(),
                              source_citations = list(),
                              multimedia_links = list()) {
  
  record_file_number <- as.character(record_file_number)
  ancestral_file_number <- as.character(ancestral_file_number)
  user_reference_number <- as.character(user_reference_number)
  
  validate_input_size(individual_ref, 1, 1, 18)
  validate_input_size(restriction_notice, 1)
  validate_input_size(sex, 1)
  validate_input_size(submitter_refs, 1000, 1, 18)
  validate_input_size(individual_ref, 1, 1, 18)
  validate_input_size(alias_refs, 1000, 1, 18)
  validate_input_size(submitters_interested_in_ancestors, 1000, 1, 18)
  validate_input_size(submitters_interested_in_descendents, 1000, 1, 18)
  validate_input_size(record_file_number, 1, 1, 90)
  validate_input_size(ancestral_file_number, 1, 1, 12)
  validate_input_size(user_reference_number, 1000, 1, 20) # QUERY
  validate_input_size(user_reference_type, 1, 1, 40)
  validate_input_size(automated_record_id, 1, 1, 12)
  validate_input_size(last_modified, 1, 10, 11)
    
  check_restriction_notice(restriction_notice)
  check_sex(sex)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, id = ref_to_xref(individual_ref, "I"), tag = "INDI"),
    tibble::tibble(level = 1, tag = "RESN", value = restriction_notice),
    names %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "SEX", value = sex),
    events %>% dplyr::bind_rows() %>% add_levels(1),
    attributes %>% dplyr::bind_rows() %>% add_levels(1),
    ordinance %>% dplyr::bind_rows() %>% add_levels(1),
    child_to_family_links %>% dplyr::bind_rows() %>% add_levels(1),
    spouse_to_family_links %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "SUBM", value = ref_to_xref(submitter_refs, "U")),
    associations %>% dplyr::bind_rows() %>%  add_levels(1),
    tibble::tibble(level = 1, tag = "ALIA", value = ref_to_xref(alias_refs, "I")),
    tibble::tibble(level = 1, tag = "ANCI", value = ref_to_xref(submitters_interested_in_ancestors, "U")),
    tibble::tibble(level = 1, tag = "DESI", value = ref_to_xref(submitters_interested_in_descendents, "U")),
    tibble::tibble(level = 1, tag = "RFN", value = record_file_number),
    tibble::tibble(level = 1, tag = "AFN", value = ancestral_file_number),
    tibble::tibble(level = 1, tag = "REFN", value = user_reference_number),
    tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type),
    tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
    last_modified %>% add_levels(1),
    notes %>% dplyr::bind_rows() %>% add_levels(1),
    source_citations %>% dplyr::bind_rows() %>% add_levels(1),
    multimedia_links %>% dplyr::bind_rows() %>% add_levels(1)
  ) %>% 
    finalise()
  
}



MULTIMEDIA_RECORD <- function(object_ref,
                              file_reference,
                              media_format,
                              media_type = character(),
                              title = character(),
                              user_ref_number = character(),
                              user_ref_type = character(),
                              automated_record_id = character(),
                              notes = list(),
                              source_citations = list(),
                              date_changed = CHANGE_DATE()){
  
  file_reference <- as.character(file_reference)
  user_ref_number <- as.character(user_ref_number)
  
  if (length(file_reference) != length(media_format))
    stop("Each file reference requires a media format")
  
  validate_input_size(object_ref, 1, 1, 18)
  validate_input_size(file_reference, 1000, 1, 30)
  validate_input_size(media_format, 1)
  validate_input_size(media_type, 1)
  validate_input_size(title, 1, 1, 248)
  validate_input_size(user_ref_number, 1000, 1, 20)
  validate_input_size(user_ref_type, 1000, 1, 40)
  validate_input_size(automated_record_id, 1, 1, 12)
  validate_input_size(last_modified, 1, 10, 11)
  
  check_media_format(media_format)
  check_media_type(media_type)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, id = ref_to_xref(object_ref, "M"), tag = "OBJE"),
    tibble::tibble(level = 1, tag = "FILE", value = file_reference),
    tibble::tibble(level = 2, tag = "FORM", value = media_format),
    tibble::tibble(level = 3, tag = "TYPE", value = media_type),
    tibble::tibble(level = 2, tag = "TITL", value = title),
    tibble::tibble(level = 1, tag = "REFN", value = user_ref_number),
    tibble::tibble(level = 2, tag = "TYPE", value = user_ref_type),
    tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
    notes %>% dplyr::bind_rows() %>% add_levels(1),
    source_citations %>% dplyr::bind_rows() %>% add_levels(1),
    last_modified %>% add_levels(1)
  ) %>% 
    finalise()
  
  
}



NOTE_RECORD <- function(note_ref,
                        submitter_text,
                        user_ref_number = character(),
                        user_ref_type = character(),
                        automated_record_id = character(),
                        source_citations = list(),
                        date_changed = CHANGE_DATE()){
  
  user_ref_number <- as.character(user_ref_number)
  
  validate_input_size(note_ref, 1, 1, 18)
  validate_input_size(submitter_text, 1)
  validate_input_size(user_ref_number, 1000, 1, 20)
  validate_input_size(user_ref_type, 1, 1, 40)
  validate_input_size(automated_record_id, 1, 1, 12)
  validate_input_size(last_modified, 1, 10, 11)
  
  dplyr::bind_rows(
    split_text(start_level = 0, top_tag = "NOTE", text = submitter_text),
    tibble::tibble(level = 1, tag = "REFN", value = user_ref_number),
    tibble::tibble(level = 2, tag = "TYPE", value = user_ref_type),
    tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
    source_citations %>% dplyr::bind_rows() %>% add_levels(1),
    last_modified %>% add_levels(1)
  ) %>% 
    dplyr::mutate(id = if_else(tag == "NOTE", ref_to_xref(note_ref, "T"), NA_character_)) %>% 
    finalise()
  
}



REPOSITORY_RECORD <- function(repo_ref,
                              repo_name,
                              repo_address = address_structure(character()),
                              repo_notes = list(),
                              user_ref_number = character(),
                              user_ref_type = character(),
                              automated_record_id = character(),
                              date_changed = CHANGE_DATE()){

  user_ref_number <- as.character(user_ref_number)
  
  validate_input_size(repo_ref, 1, 1, 18)
  validate_input_size(repo_name, 1, 1, 90)
  validate_input_size(user_ref_number, 1000, 1, 20)
  validate_input_size(user_ref_type, 1, 1, 40)
  validate_input_size(automated_record_id, 1, 1, 12)
  validate_input_size(last_modified, 1, 10, 11)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, id = ref_to_xref(repo_ref, "R"), tag = "REPO"),
    tibble::tibble(level = 1, tag = "NAME", value = repo_name),
    repo_address %>% add_levels(1),
    repo_notes %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "REFN", value = user_ref_number),
    tibble::tibble(level = 2, tag = "TYPE", value = user_ref_type),
    tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
    last_modified %>% add_levels(1)
  ) %>% 
    finalise()
  
    
}



SOURCE_RECORD <- function(source_ref,
                          events_recorded = character(),
                          date_period = character(),
                          source_jurisdiction_place = character(),
                          responsible_agency = character(),
                          data_notes = list(),
                          source_originator = character(),
                          source_title = character(),
                          source_filed_by = character(),
                          source_publ_facts = character(),
                          source_text = character(),
                          source_repo_citations = list(),
                          user_ref_number = character(),
                          user_ref_type = character(),
                          automated_record_id = character(),
                          date_changed = CHANGE_DATE(),
                          notes = list(),
                          multimedia_links = list()){
  
  user_ref_number <- as.character(user_ref_number)
  
  validate_input_size()
  
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, id = ref_to_xref(source_ref, "S"), tag = "SOUR"),
    tibble::tibble(level = 1, tag = "DATA", value = ""),
    tibble::tibble(level = 2, tag = "EVEN", value = events_recorded),
    tibble::tibble(level = 3, tag = "DATE", value = date_period),
    tibble::tibble(level = 3, tag = "PLAC", value = source_jurisdiction_place),
    tibble::tibble(level = 2, tag = "AGNC", value = responsible_agency),
    data_notes %>% dplyr::bind_rows() %>% add_levels(2),
    split_text(start_level = 1, top_tag = "AUTH", text = source_originator),
    split_text(start_level = 1, top_tag = "TITL", text = source_title),
    tibble::tibble(level = 1, tag = "ABBR", value = source_filed_by),
    split_text(start_level = 1, top_tag = "PUBL", text = source_publ_facts),
    split_text(start_level = 1, top_tag = "TEXT", text = source_text),
    source_repo_citations %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "REFN", value = user_ref_number),
    tibble::tibble(level = 2, tag = "TYPE", value = user_ref_type),
    tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
    last_modified %>% add_levels(1),
    notes %>% dplyr::bind_rows() %>% add_levels(1),
    multimedia_links %>% dplyr::bind_rows() %>% add_levels(1)
  ) %>% 
    finalise()
  
}


# Max = 1
SUBMISSION_RECORD <- function(submission_ref,
                              submitter_ref = character(),
                              name_of_family_file = character(),
                              temple_code = character(),
                              num_anc_generations = character(),
                              num_des_generations = character(),
                              ordinance_flag = character(),
                              automated_record_id = character(),
                              notes = list(),
                              date_changed = CHANGE_DATE()){
 
  num_anc_generations <- as.character(num_anc_generations)
  num_des_generations <- as.character(num_des_generations)
  check_ordinance_flag(ordinance_flag)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, id = ref_to_xref(submission_ref, "SN"), tag = "SUBN"),
    tibble::tibble(level = 1, tag = "SUBM", value = ref_to_xref(submitter_ref, "U")),
    tibble::tibble(level = 1, tag = "FAMF", value = name_of_family_file),
    tibble::tibble(level = 1, tag = "TEMP", value = temple_code),
    tibble::tibble(level = 1, tag = "ANCE", value = num_anc_generations),
    tibble::tibble(level = 1, tag = "DESC", value = num_des_generations),
    tibble::tibble(level = 1, tag = "ORDI", value = ordinance_flag),
    tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
    notes %>% dplyr::bind_rows() %>% add_levels(1),
    last_modified %>% add_levels(1)
  ) %>% 
    finalise()
  
   
}


SUBMITTER_RECORD <- function(submitter_ref,
                             submitter_name = character(),
                             submitter_address = address_structure(character()),
                             multimedia_links = list(),
                             language_preference = character(),
                             submitter_registered_rfn = character(),
                             automated_record_id = character(),
                             notes = list(),
                             date_changed = CHANGE_DATE()){
  
  submitter_registered_rfn <- as.character(submitter_registered_rfn)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, id = ref_to_xref(submitter_ref, "U"), tag = "SUBM"),
    tibble::tibble(level = 1, tag = "NAME", value = submitter_name),
    submitter_address %>% add_levels(1),
    multimedia_links %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "LANG", value = language_preference),
    tibble::tibble(level = 1, tag = "RFN", value = submitter_registered_rfn),
    tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
    notes %>% dplyr::bind_rows() %>% add_levels(1),
    last_modified %>% add_levels(1)
  ) %>% 
    finalise()
  
  
}


#' @tests
#' expect_equal(FOOTER_SECTION(),
#'              tibble::tribble(~level,  ~id,   ~tag, ~value,
#'                              0, "TR", "TRLR",     ""
#'              ))
FOOTER_SECTION <- function(){
  tibble::tibble(level = 0, id = "TR", tag = "TRLR", value = "") %>% 
    finalise()
}
