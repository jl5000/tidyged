# Generated by roxytest: Do not edit by hand!

# File R/repository.R: @tests

test_that("Function remove_repo() @ L51", {
  expect_equal(gedcom(subm()),
               gedcom(subm()) %>% add_repo("text") %>% remove_repo())
})

