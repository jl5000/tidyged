

#' Import a GEDCOM file
#'
#' Imports a *.ged file and creates a tidyged object.
#'
#' @param filepath The full filepath of the GEDCOM file.
#'
#' @return A tidyged object
#' @export
#'
#' @examples
#' \dontrun{
#' read_gedcom("C:/my_family.ged")
#' }
#' @tests
#' expect_error(read_gedcom("my_family.txt"))
#' expect_snapshot_value(
#'     read_gedcom(system.file("extdata", "555SAMPLE.GED", package = "tidyged")), 
#'     "json2")
#' expect_snapshot_value(
#'     read_gedcom(system.file("extdata", "555SAMPLE16BE.GED", package = "tidyged")), 
#'     "json2")
#' expect_snapshot_value(
#'     read_gedcom(system.file("extdata", "555SAMPLE16LE.GED", package = "tidyged")), 
#'     "json2")
read_gedcom <- function(filepath) {

  if(tolower(stringr::str_sub(filepath, -4, -1)) != ".ged")
    stop("GEDCOM file should have a .ged extension")
  
  gedcom_encoding <- read_gedcom_encoding(filepath)
  
  con <- file(filepath, encoding = gedcom_encoding)
  on.exit(close(con))
  
  ged <- readLines(con) %>% 
    stringr::str_trim(side = "left") %>% 
    check_line_lengths(.pkgenv$gedcom_line_length_limit) %>%
    tibble::tibble(value = .) %>%
    tidyr::extract(value, into = c("level", "record", "tag", "value"), 
                   regex = "^\\w*(\\d) (@.+@)? ?(\\w{3,5}) ?(.*)$") %>%
    dplyr::mutate(record = dplyr::if_else(tag == "HEAD", "HD", record),
                  record = dplyr::if_else(tag == "TRLR", "TR", record),
                  record = dplyr::na_if(record, "")) %>%
    tidyr::fill(record) %>% 
    dplyr::mutate(level = as.numeric(level),
                  value = stringr::str_replace_all(value, "@@", "@")) %>% 
    combine_gedcom_values() %>% 
    set_class_to_tidyged()

  validate_gedcom(ged, gedcom_encoding)
  ged
  
}


#' Read the Byte Order Mark of the GEDCOM file
#' 
#' This function reads the Byte Order Mark of a GEDCOM file in order to determine its encoding.
#' It only checks for UTF-8 or UTF-16 - if neither of these are found it throws an error.
#'
#' @param filepath The full filepath of the GEDCOM file.
#'
#' @return A character string indicating the encoding of the file.
#' @tests
#' expect_equal(
#'   read_gedcom_encoding(system.file("extdata", "555SAMPLE.GED", package = "tidyged")), 
#'   "UTF-8")
#' expect_equal(
#'   read_gedcom_encoding(system.file("extdata", "555SAMPLE16BE.GED", package = "tidyged")), 
#'   "UTF-16BE")
#' expect_equal(
#'   read_gedcom_encoding(system.file("extdata", "555SAMPLE16LE.GED", package = "tidyged")), 
#'   "UTF-16LE")
read_gedcom_encoding <- function(filepath) {
  
  if(identical(as.character(readBin(filepath, 'raw', 3)), .pkgenv$BOM_UTF8)) {
    return("UTF-8")  
  } else if(identical(as.character(readBin(filepath, 'raw', 2)), .pkgenv$BOM_UTF16_BE)) {
    return("UTF-16BE")
  } else if(identical(as.character(readBin(filepath, 'raw', 2)), .pkgenv$BOM_UTF16_LE)) {
    return("UTF-16LE")
  } else {
    stop("Invalid file encoding. Only UTF-8 and UTF-16 Byte Order Marks are supported")
  }
  
}


#' Check the line lengths of a GEDCOM file
#' 
#' @param lines A character vector of GEDCOM lines.
#' @param limit The maximum length of a line allowed.
#'
#' @return The input character vector
#' @tests
#' expect_error(check_line_lengths(c("the", "quick", "brown", "fox"), 4))
#' expect_equal(check_line_lengths(c("the", "quick", "brown", "fox"), 5),
#'                                 c("the", "quick", "brown", "fox"))
check_line_lengths <- function(lines, limit) {
  
  if(any(nchar(lines) > limit)) 
    stop("This is not a GEDCOM 5.5.5 file. The following lines are too long: ", which(lines > limit))
  
  lines
}


set_class_to_tidyged <- function(gedcom) {
  class(gedcom) <- c("tidyged", "tbl_df", "tbl", "data.frame")
  gedcom
}

#' Convert the GEDCOM grammar to the GEDCOM form
#' 
#' This function applies concatenation indicated by CONC/CONT lines.
#' 
#' The function works by collapsing CONC/CONT lines using group-by/summarise.   
#'
#' @param gedcom A tidyged object.
#'
#' @return A tidyged object in the GEDCOM form.
combine_gedcom_values <- function(gedcom) {
  
  tags <- c("CONT", "CONC")
  
  gedcom %>% 
    dplyr::mutate(row = dplyr::row_number()) %>% 
    dplyr::mutate(tag = dplyr::if_else(tag %in% tags, NA_character_, tag),
                  row = dplyr::if_else(tag %in% tags, NA_integer_, row),
                  value = dplyr::if_else(tag == "CONT", paste0("\n", value), value)) %>%
    tidyr::fill(tag, row, .direction = "down") %>%
    dplyr::group_by(record, tag, row) %>% 
    dplyr::summarise(level = min(level),
                     value = paste(value, collapse = ""),
                     .groups = "drop") %>%
    dplyr::ungroup() %>% 
    dplyr::arrange(row) %>%
    dplyr::select(level, record, tag, value)
  
}

#' Save a tidyged object to disk as a GEDCOM file
#' 
#' @details This function prepares the tidyged object and then writes it to the filepath.
#' Steps taken include escaping "@" signs (with another "@") and splitting long lines onto
#' separate lines.
#'
#' @param gedcom A tidyged object.
#' @param filepath The full filepath to write to.
#'
#' @return Nothing
#' @export
#' @tests
#' expect_warning(write_gedcom(gedcom(), "my_family.txt") %>% file.remove("my_family.txt"))
write_gedcom <- function(gedcom, filepath) {

  if(file.exists(filepath)) file.remove(filepath)
  
  con <- file(filepath, encoding = "UTF-8", open = "a")
  suppressWarnings(writeChar("\ufeff", con, eos = NULL))
  on.exit(close(con))
  
  if(tolower(stringr::str_sub(filepath, -4, -1)) != ".ged")
    warning("Output is not being saved as a GEDCOM file (*.ged)")
  
  gedcom %>%
    update_header_with_filename(filename = basename(filepath)) %>% 
    dplyr::mutate(value = dplyr::if_else(stringr::str_detect(value, xref_pattern()),
                                         value,
                                         stringr::str_replace_all(value, "@", "@@"))) %>% 
    split_gedcom_values(char_limit = .pkgenv$gedcom_phys_value_limit) %>% 
    dplyr::mutate(record = dplyr::if_else(dplyr::lag(record) == record, "", record)) %>% 
    dplyr::mutate(record = dplyr::if_else(record == "TR", "", record)) %>% 
    tidyr::replace_na(list(record = "")) %>% 
    dplyr::transmute(value = paste(level, record, tag, value)) %>% 
    dplyr::pull(value) %>% 
    stringr::str_replace_all("  ", " ") %>%
    writeLines(con)
  
}


#' Update GEDCOM header with filename
#'
#' @param gedcom A tidyged object.
#' @param filename The name of the file (with extension).
#'
#' @return An updated tidyged object with the updated filename.
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'                         update_header_with_filename("my_file.ged") %>% 
#'                         remove_dates_for_tests(), "json2")
#' expect_snapshot_value(read_gedcom(system.file("extdata", "555SAMPLE.GED", package = "tidyged")) %>% 
#'                         update_header_with_filename("my_file.ged") %>% 
#'                         remove_dates_for_tests(), "json2")
update_header_with_filename <- function(gedcom, filename) {
  
  if(nrow(dplyr::filter(gedcom, record == "HD", tag == "FILE")) == 0) {
    
    tibble::add_row(gedcom, 
                    tibble::tibble(level = 1, record = "HD", tag = "FILE", value = filename),
                    .before = find_insertion_point(gedcom, "HD", 0, "HEAD"))
    
  } else if(nrow(dplyr::filter(gedcom, record == "HD", tag == "FILE")) == 1) {
    
    dplyr::mutate(gedcom, 
                  value = dplyr::if_else(record == "HD" & tag == "FILE", filename, value))
  }
  
}


#' Convert the GEDCOM form to GEDCOM grammar
#' 
#' This function introduces CONC/CONT lines for line values that exceed the given number of characters.
#' This function only uses the CONC(atenation) tag for splitting values, and does
#' not use the CONT(inuation) tag. This is because it is easier to implement.
#'
#' @param gedcom A tidyged object.
#' @param char_limit Maximum string length of values.
#' @return A tidyged object in the GEDCOM grammar ready to export.
#' @tests
#' expect_snapshot_value(
#'                 gedcom(subm("Me")) %>% 
#'                   add_source(title = paste(rep("a", 4095), collapse = "")) %>%
#'                   remove_dates_for_tests() %>% 
#'                   split_gedcom_values(248), "json2")
split_gedcom_values <- function(gedcom, char_limit) {
  
  unique_delim <- "<>delimiter<>"
  header <- dplyr::filter(gedcom, record == "HD")
  
  gedcom %>% 
    dplyr::filter(record != "HD") %>% #header shouldn't contain CONT/CONC lines
    dplyr::mutate(split = nchar(value) > char_limit, #mark rows to split
                  row = dplyr::row_number()) %>% # mark unique rows
    dplyr::mutate(value = gsub(paste0("(.{", char_limit, "})"), #add delimiters where
                        paste0("\\1", unique_delim), #the splits should occur
                        value)) %>% 
    dplyr::mutate(value = gsub(paste0(unique_delim, "$"), "", value)) %>% #remove last delimiter
    tidyr::separate_rows(value, sep = unique_delim) %>% 
    dplyr::mutate(tag = dplyr::if_else(split & dplyr::lag(split) & row == dplyr::lag(row), "CONC", tag)) %>% # use CONC tags
    dplyr::mutate(level = dplyr::if_else(split & dplyr::lag(split) & row == dplyr::lag(row), level + 1, level)) %>% # increase levels
    dplyr::select(-split, -row) %>%  #remove temporary columns
    dplyr::bind_rows(header, .)
  
  
}





#' Create a base tidyged object
#' 
#' This function creates a minimal tidyged object with header and footer sections and a single submitter record.
#'  
#' @param submitter_details Details of the submitter of the file (you?) using the subm() function. If no submitter
#' name is provided, the username is used.
#' @param gedcom_description A note to describe the contents of the file in terms of "ancestors or descendants of" 
#' so that the person receiving the data knows what genealogical information the file contains.
#' @param gedcom_copyright A copyright statement needed to protect the copyrights of the submitter of this GEDCOM file.
#' @param source_data_name The name of the electronic data source that was used to obtain the data in this file. 
#' @param source_data_date The date this source was created or published. Ensure you create this date with the 
#' date_exact() function.
#' @param source_data_copyright A copyright statement required by the owner of data from which this information was 
#' obtained.  
#' @param receiving_system The name of the system expected to process the GEDCOM-compatible file. 
#' @param language The human language in which the data in the file is normally read or written.
#'
#' @return A minimal tidyged object. 
#' @export
gedcom <- function(submitter_details = subm(),
                   gedcom_description = character(),
                   gedcom_copyright = character(),
                   source_data_name = character(),
                   source_data_date = date_exact(),
                   source_data_copyright = character(),
                   receiving_system = "tidyged",
                   language = "English") {
  
  tidyged.internals::GEDCOM_HEADER(
    header_extension = tidyged.internals::LINEAGE_LINKED_HEADER_EXTENSION(system_id = "tidyged",
                                                                          product_version_number = utils::packageVersion("tidyged"),
                                                                          name_of_product = "The 'tidyged' package for the R language",
                                                                          name_of_business = "Jamie Lendrum",
                                                                          business_address = tidyged.internals::ADDRESS_STRUCTURE(address_email = "jalendrum@gmail.com",
                                                                                                               address_web_page = "https://jl5000.github.io/tidyged/"),
                                                                          name_of_source_data = source_data_name,
                                                                          publication_date = source_data_date,
                                                                          copyright_source_data = source_data_copyright,
                                                                          receiving_system_name = receiving_system,
                                                                          file_creation_date = date_current(),
                                                                          language_of_text = language,
                                                                          xref_subm = assign_xref(.pkgenv$xref_prefix_subm, 1),
                                                                          copyright_gedcom_file = gedcom_copyright,
                                                                          gedcom_content_description = gedcom_description)) %>% 
    dplyr::bind_rows(submitter_details, 
                     tidyged.internals::FOOTER_SECTION()) %>%
    set_class_to_tidyged()
  
}



#' Define a Submitter record for a new tidyged object
#'
#' @details 
#' This function is supposed to be used in the gedcom() function to define a
#' new tidyged object.
#' 
#' This submitter record identifies the individual or organization that contributed 
#' information contained in the GEDCOM file.
#' 
#' @param name The name of the submitter.
#' @param local_address_lines The first lines of the submitter address.
#' @param city The city of the submitter.
#' @param state The state/county of the submitter.
#' @param postal_code The postal code of the submitter.
#' @param country The country of the submitter.
#' @param phone_number A character vector containing up to three phone numbers of the submitter.
#' @param email A character vector containing up to three email addresses of the submitter.
#' @param fax A character vector containing up to three fax numbers of the submitter.
#' @param web_page A character vector containing up to three web pages of the submitter.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. 
#' @param submitter_notes A character vector of notes accompanying this Submitter record.
#' These could be xrefs to existing Note records.
#' @param multimedia_links TODO
#'
#' @return A Submitter record to be incorporated into a new tidyged object.
#' @export
subm <- function(name = unname(Sys.info()["user"]),
                 local_address_lines = character(),
                 city = character(),
                 state = character(),
                 postal_code = character(),
                 country = character(),
                 phone_number = character(),
                 email = character(),
                 fax = character(),
                 web_page = character(),
                 automated_record_id = character(),
                 submitter_notes = character(),
                 multimedia_links = character()) {
  
  if(length(local_address_lines) > 3) local_address_lines <- local_address_lines[1:3]
  
  address <- tidyged.internals::ADDRESS_STRUCTURE(local_address_lines = local_address_lines,
                                                     address_city = city,
                                                     address_state = state,
                                                     address_postal_code = postal_code,
                                                     address_country = country,
                                                     phone_number = phone_number,
                                                     address_email = email,
                                                     address_fax = fax,
                                                     address_web_page = web_page)
  
  subm_notes <- purrr::map(submitter_notes, tidyged.internals::NOTE_STRUCTURE)
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_multimedia(gedcom), tags = "FILE") %>% 
    purrr::map(tidyged.internals::MULTIMEDIA_LINK)
  
  tidyged.internals::SUBMITTER_RECORD(xref_subm = assign_xref(.pkgenv$xref_prefix_subm, 1),
                                         submitter_name = name,
                                         address = address,
                                         automated_record_id = automated_record_id,
                                         notes = subm_notes,
                                         multimedia_links = media_links)
  
}
