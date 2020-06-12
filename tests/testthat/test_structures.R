
test_that("personal_name_pieces gives expected values", {
  expect_error(personal_name_pieces())
  expect_s3_class(personal_name_pieces(1), "tbl_df")
  expect_s3_class(personal_name_pieces(2, given = "Jamie"), "tbl_df")
  expect_s3_class(personal_name_pieces(3, nickname = "J"), "tbl_df")
})

test_that("personal_name_structure gives expected values", {
  expect_error(personal_name_structure())
  # personal_name_structure(start_level = 1, name = "Joe bloggs", )
})
