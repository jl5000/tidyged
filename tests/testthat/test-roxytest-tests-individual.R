# Generated by roxytest: Do not edit by hand!

# File R/individual.R: @tests

test_that("Function add_indi() @ L32", {
  expect_snapshot_value(add_indi(gedcom(subm("Me")),
                                       sex = "M", user_reference_number = 1234,
                                       user_reference_type = "something",
                                       indi_notes = c("Note1", "Note 2")) %>% 
                         tidyged.internals::remove_dates_for_tests(), "json2")
})


test_that("Function remove_indi() @ L87", {
  expect_equal(gedcom(subm()),
               gedcom(subm()) %>% add_indi() %>% remove_indi())
})

