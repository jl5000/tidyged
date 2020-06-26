
test_that("header_section gives expected values", {
  expect_error(header_section())
  expect_error(header_section(1))
  expect_error(header_section(1, approved_system_id = "system"))
  expect_error(header_section(1, approved_system_id = "system", character_set = "red"))
  
  df1 <- header_section(1, approved_system_id = "system", character_set = "UTF-8",
                        submission_ref = 1, system_version_number = "1.0", name_of_product = "R",
                        gedcom_copyright = "Do not copy", language = "English",
                        gedcom_description = "This is a gedcom file")
  df2 <- tibble::tribble(
    ~level,  ~id,   ~tag,                  ~value,
    0, "HD", "HEAD",                      "",
    1, "HD", "SOUR",                "system",
    2, "HD", "VERS",                   "1.0",
    2, "HD", "NAME",                     "R",
    1, "HD", "SUBM",                  "@U1@",
    1, "HD", "SUBN",                 "@SU1@",
    1, "HD", "COPR",           "Do not copy",
    1, "HD", "GEDC",                      "",
    2, "HD", "VERS",                 "5.5.1",
    2, "HD", "FORM",        "Lineage-Linked",
    1, "HD", "CHAR",                 "UTF-8",
    1, "HD", "LANG",               "English",
    1, "HD", "NOTE", "This is a gedcom file"
  )
  expect_equal(df1, df2)
  
  df1 <- header_section(1, approved_system_id = "system", character_set = "UTF-8",
                        submission_ref = 1, system_version_number = "1.0", name_of_product = "R",
                        name_of_business = "RStudio", business_address = address_structure(c("Street", "City", "State")),
                        source_data_name = "Source text", source_data_publication_date = date_exact(5, 9, 2005),
                        source_data_copyright = "Source is protected", receiving_system = "Windows",
                        transmission_date = date_exact(15, 10, 2020), file_name = "test.ged",
                        gedcom_copyright = "Do not copy", language = "English", place = "here",
                        gedcom_description = "This is a gedcom file")
  
  df2 <- tibble::tribble(
    ~level,  ~id,   ~tag,                  ~value,
    0, "HD", "HEAD",                      "",
    1, "HD", "SOUR",                "system",
    2, "HD", "VERS",                   "1.0",
    2, "HD", "NAME",                     "R",
    2, "HD", "CORP",               "RStudio",
    3, "HD", "ADDR",                "Street",
    4, "HD", "CONT",                  "City",
    4, "HD", "CONT",                 "State",
    4, "HD", "ADR1",                  "City",
    4, "HD", "ADR2",                 "State",
    2, "HD", "DATA",           "Source text",
    3, "HD", "DATE",            "5 SEP 2005",
    3, "HD", "COPR",   "Source is protected",
    1, "HD", "DEST",               "Windows",
    1, "HD", "DATE",           "15 OCT 2020",
    1, "HD", "SUBM",                  "@U1@",
    1, "HD", "SUBN",                 "@SU1@",
    1, "HD", "FILE",              "test.ged",
    1, "HD", "COPR",           "Do not copy",
    1, "HD", "GEDC",                      "",
    2, "HD", "VERS",                 "5.5.1",
    2, "HD", "FORM",        "Lineage-Linked",
    1, "HD", "CHAR",                 "UTF-8",
    1, "HD", "LANG",               "English",
    1, "HD", "PLAC",                      "",
    2, "HD", "FORM",                  "here",
    1, "HD", "NOTE", "This is a gedcom file"
  )
  expect_equal(df1, df2)
})

test_that("family_record gives expected values", {
  
})

test_that("individual_record gives expected values", {
  
})

test_that("multimedia_record gives expected values", {
  
})

test_that("note_record gives expected values", {
  
})

test_that("repository_record gives expected values", {
  
})

test_that("source_record gives expected values", {
  
})

test_that("submission_record gives expected values", {
  
})

test_that("submitter_record gives expected values", {
  
})

test_that("footer_section gives expected values", {
  df1 <- footer_section()
  df2 <- tibble::tribble(
    ~level,  ~id,   ~tag, ~value,
    0, "TR", "TRLR",     ""
  )
  expect_equal(df1, df2)
})
