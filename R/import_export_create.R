

#' Import a GEDCOM file
#'
#' Imports a *.ged file and creates a tidygedcom object.
#'
#' @param filepath The full filepath of the GEDCOM file
#'
#' @return A tidygedcom object
#' @export
#'
#' @examples
#' \dontrun{
#' import_gedcom("C:/my_family.ged")
#' }
import_gedcom <- function(filepath) {
  # TODO: Byte Order Mark, reject leading whitespace 
  
  gedcom_encoding <- read_gedcom_encoding(filepath)
  
  if(tolower(stringr::str_sub(filepath, -4, -1)) != ".ged")
    stop("GEDCOM file should have a .ged extension")
  
  ged <- readr::read_lines(filepath, locale = readr::locale(encoding = gedcom_encoding)) %>% 
    stringr::str_trim(side = "left") %>% 
    tibble::tibble(value = .) %>%
    tidyr::extract(value, into = c("level", "record", "tag", "value"), 
                   regex = "^(\\d) (@.+@)? ?(\\w{3,5}) ?(.*)$") %>%
    dplyr::mutate(record = dplyr::if_else(tag == "HEAD", "HD", record),
                  record = dplyr::if_else(tag == "TRLR", "TR", record),
                  record = dplyr::na_if(record, "")) %>%
    tidyr::fill(record) %>% 
    dplyr::mutate(level = as.numeric(level),
                  value = stringr::str_replace_all(value, "@@", "@")) %>% 
    set_class_to_tidygedcom()

  validate_gedcom(ged)
  ged
  
}


read_gedcom_encoding <- function(filepath) {
  
  if(all.equal(as.character(readBin(filepath, 'raw', 3)), .pkgenv$BOM_UTF8)) {
    return("UTF-8")  
  } else if(all.equal(as.character(readBin(filepath, 'raw', 2)), .pkgenv$BOM_UTF16_BE)) {
    return("UTF-16BE")
  } else if(all.equal(as.character(readBin(filepath, 'raw', 2)), .pkgenv$BOM_UTF16_LE)) {
    return("UTF-16LE")
  } else {
    stop("Invalid file encoding. Only UTF-8 and UTF-16 are supported")
  }
  
}

#' Save a tidygedcom object to disk as a GEDCOM file
#'
#' @param gedcom A tidygedcom object.
#' @param filepath The full filepath to write to.
#'
#' @return Nothing
#' @export
export_gedcom <- function(gedcom, filepath) {

  if(tolower(stringr::str_sub(filepath, -4, -1)) != ".ged")
    warning("Output is not being saved as a GEDCOM file (*.ged)")
  
  gedcom %>%
    #update_header(file_name = basename(filepath)) %>% 
    purrr::when(
      nrow(dplyr::filter(., record == "HD", tag == "FILE")) == 0 ~
                     tibble::add_row(., tibble::tibble(level = 1, record = "HD", 
                                                       tag = "FILE", value = basename(filepath)),
                                     .before = find_insertion_point(., "HD", 0, "HEAD")),
      nrow(dplyr::filter(., record == "HD", tag == "FILE")) == 1 ~
        dplyr::mutate(., value = dplyr::if_else(record == "HD" & tag == "FILE", basename(filepath), value)),
      ~ .
    ) %>% 
    dplyr::mutate(value = str_replace_all(value, "@", "@@")) %>% 
    split_gedcom_values(char_limit = .pkgenv$gedcom_phys_value_limit) %>% 
    dplyr::mutate(record = dplyr::if_else(dplyr::lag(record) == record, "", record)) %>% 
    dplyr::mutate(record = dplyr::if_else(record == "TR", "", record)) %>% 
    tidyr::replace_na(list(record = "")) %>% 
    dplyr::transmute(value = paste(level, record, tag, value)) %>% 
    dplyr::mutate(value = stringr::str_replace_all(value, "  ", " ")) %>%
    utils::write.table(filepath, na = "", col.names = FALSE, quote = FALSE, row.names = FALSE)
  
}

#' Convert the GEDCOM form to GEDCOM grammar
#' 
#' This function introduces CONC/CONT lines for line values that exceed the given number of characters.
#' This function only uses the CONC(atenation) tag for splitting values, and does
#' not use the CONT(inuation) tag. This is because it is easier to implement.
#'
#' @param gedcom A tidygedcom object.
#' @param char_limit
#' @tests
#' @return A tidygedcom object potentially expanded with CONC/CONT rows.
split_gedcom_values <- function(gedcom, char_limit) {
  
  unique_delim <- "<>delimiter<>"
  
  gedcom %>% 
    dplyr::mutate(split = record != "HD" & nchar(value) > char_limit, #mark rows to split
                  row = dplyr::row_number()) %>% # mark unique rows
    dplyr::mutate(value = gsub(paste0("(.{", char_limit, "})"), #add delimiters where
                        paste0("\\1", unique_delim), #the splits should occur
                        value)) %>% 
    dplyr::mutate(value = gsub(paste0(unique_delim, "$"), "", value)) %>% #remove last delimiter
    tidyr::separate_rows(value, sep = unique_delim) %>% 
    dplyr::mutate(tag = dplyr::if_else(split & dplyr::lag(split) & row == dplyr::lag(row), "CONC", tag)) %>% # use CONC tags
    dplyr::mutate(level = dplyr::if_else(split & dplyr::lag(split) & row == dplyr::lag(row), level + 1, level)) %>% # increase levels
    dplyr::select(-split, -row) #remove temporary columns
  
  
}



combine_gedcom_lines <- function(gedcom) {
  
  
}


#' Create a base tidygedcom object
#' 
#' This function creates a minimal tidygedcom object with header and footer sections and a single submitter record.
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
#' @return A minimal tidygedcom object 
#' @export
gedcom <- function(submitter_details = subm(),
                   gedcom_description = character(),
                   gedcom_copyright = character(),
                   source_data_name = character(),
                   source_data_date = date_exact(),
                   source_data_copyright = character(),
                   receiving_system = character(),
                   language = character()) {
  
  
  GEDCOM_HEADER(header_extension = 
    LINEAGE_LINKED_HEADER_EXTENSION(name_of_source_data = source_data_name,
                                    publication_date_source_data = source_data_date,
                                    copyright_source_data = source_data_copyright,
                                    receiving_system_name = receiving_system,
                                    file_creation_date = current_date(),
                                    language_of_text = language,
                                    xref_subm = assign_xref(.pkgenv$xref_prefix_subm, 1),
                                    copyright_gedcom_file = gedcom_copyright,
                                    gedcom_content_description = gedcom_description)) %>% 
    dplyr::bind_rows(submitter_details, 
                     FOOTER_SECTION()) %>%
    set_class_to_tidygedcom()
  
}


