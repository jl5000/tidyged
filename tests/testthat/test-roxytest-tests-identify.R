# Generated by roxytest: Do not edit by hand!

# File R/identify.R: @tests

test_that("Function is_famg_birth_child() @ L14", {
  expect_equal(is_famg_birth_child(sample555, "@I3@", "@F1@"), TRUE)
  expect_equal(is_famg_birth_child(sample555, "@I3@", "@F2@"), FALSE)
  expect_equal(is_famg_birth_child(sample555, "@I1@", "@F1@"), FALSE)
})


test_that("Function get_families_as_partner() @ L63", {
  expect_equal(get_families_as_partner(sample555, "@I1@"), c("@F1@", "@F2@"))
  expect_equal(get_families_as_partner(sample555, "@I2@"), "@F1@")
  expect_equal(get_families_as_partner(sample555, "@I3@"), character())
})


test_that("Function get_families_as_child() @ L86", {
  expect_equal(get_families_as_child(sample555, "@I3@"), c("@F1@", "@F2@"))
  expect_equal(get_families_as_child(sample555, "@I1@"), character())
})


test_that("Function get_indi_partners() @ L128", {
  expect_equal(get_indi_partners(sample555, "@I1@"), "@I2@")
  expect_equal(get_indi_partners(sample555, "@I2@", TRUE), "Robert Eugene Williams")
  expect_equal(get_indi_partners(sample555, "@I3@"), character())
})


test_that("Function get_indi_children() @ L168", {
  expect_error(get_indi_children(sample555, "@I4@"))
  expect_equal(get_indi_children(sample555, "@I1@"), "@I3@")
  expect_equal(get_indi_children(sample555, "@I2@", FALSE, TRUE), "Joe Williams")
  expect_equal(get_indi_children(sample555, "@I3@"), character())
})


test_that("Function get_indi_parents() @ L208", {
  expect_equal(get_indi_parents(sample555, "@I3@"), c("@I1@", "@I2@"))
  expect_equal(get_indi_parents(sample555, "@I3@", FALSE, TRUE), c("Robert Eugene Williams", "Mary Ann Wilson"))
  expect_equal(get_indi_parents(sample555, "@I1@"), character())
})


test_that("Function get_indi_siblings() @ L244", {
  expect_equal(get_indi_siblings(sample555, "@I3@"), character())
})


test_that("Function get_famg_partners() @ L336", {
  expect_equal(get_famg_partners(sample555, "@F1@"), c("@I1@", "@I2@"))
})


test_that("Function get_famg_children() @ L366", {
  expect_equal(get_famg_children(sample555, "@F1@"), "@I3@")
  expect_equal(get_famg_children(sample555, "@F2@"), "@I3@")
  expect_equal(get_famg_children(sample555, "@F2@", birth_only = TRUE), character())
  expect_equal(get_famg_children(sample555, "@F2@", return_name = TRUE), "Joe Williams")
})


test_that("Function get_supporting_records() @ L410", {
  expect_equal(get_supporting_records(sample555, "@I1@"), c("@S1@", "@R1@"))
})


test_that("Function get_descendants() @ L461", {
  expect_equal(get_descendants(sample555, "@I3@"), character())
  expect_equal(get_descendants(sample555, "@I1@"), "@I3@")
  expect_equal(get_descendants(sample555, "@I1@", TRUE), c("@I1@","@I3@"))
  expect_equal(get_descendants(sample555, "@I1@", TRUE, TRUE), c("@I2@","@I1@","@I3@"))
  expect_equal(get_descendants(sample555, "@I1@", TRUE, TRUE, TRUE), c("@F1@","@F2@","@I2@","@I1@","@I3@"))
})


test_that("Function get_ancestors() @ L533", {
  expect_equal(get_ancestors(sample555, "@I1@"), character())
})


test_that("Function xrefs_indi() @ L599", {
  expect_equal(xrefs_indi(sample555), paste0("@I", 1:3, "@"))
})


test_that("Function xrefs_famg() @ L605", {
  expect_equal(xrefs_famg(sample555), paste0("@F", 1:2, "@"))
})


test_that("Function xrefs_subm() @ L611", {
  expect_equal(xrefs_subm(sample555), paste0("@U", 1, "@"))
})


test_that("Function xrefs_sour() @ L617", {
  expect_equal(xrefs_sour(sample555), paste0("@S", 1, "@"))
})

