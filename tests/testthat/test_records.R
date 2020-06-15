
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
