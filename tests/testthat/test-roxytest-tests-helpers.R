# Generated by roxytest: Do not edit by hand!

# File R/helpers.R: @tests

test_that("Function remove_records() @ L72", {
  expect_snapshot_value(remove_records(sample555, c("@I1@","@I2@")), "json2")
  expect_snapshot_value(remove_records(sample555, c("@S1@","@R1@")), "json2")
  expect_snapshot_value(remove_records(sample555, c("@F1@","@I3@")), "json2")
})

