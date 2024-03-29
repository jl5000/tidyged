# Generated by roxytest: Do not edit by hand!

# File R/summarise.R: @tests

test_that("Function summary.tidyged() @ L19", {
  expect_snapshot_value(
                 gedcom(subm("Me"), gedcom_description = "descrip", language = "English",
                        gedcom_copyright = "copyright statement") |> 
                  summary(), "json2")
})


test_that("Function str.tidyged() @ L66", {
  expect_snapshot_value(
   gedcom(subm("Me")) |> 
    add_indi() |> 
    add_indi() |> 
    add_indi() |> 
    add_famg() |> 
    add_famg() |> 
    add_media("ref1", "AAC") |> 
    add_media("ref1", "AAC") |> 
    add_sour() |> 
    add_repo("repo") |> 
    add_note("note1") |> 
    add_note("note2") |> 
    str(), "json2")
})


test_that("Function df_indi() @ L112", {
  expect_snapshot_value(gedcom(subm("Me")) |> 
   add_indi(sex = "M") |> 
   add_indi_names(name_pieces(given = "Joe", surname = "Bloggs")) |> 
   add_indi_fact("bir", date = date_calendar(year = 1950, month = 5, day = 7),
                              fact_place = place("Somewhere")) |> 
   add_indi_fact("dea", date = date_calendar(year = 2000, month = 12, day = 1),
                              fact_place = place("Somewhere else")) |> 
   add_indi(sex = "F") |> 
   add_indi_names(name_pieces(given = "Jess", surname = "Bloggs")) |> 
   add_indi_fact("bir", date = date_calendar(year = 1948, month = 1, day = 15),
                              fact_place = place("Somewhere")) |> 
   add_indi(sex = "F") |> 
   add_indi_names(name_pieces(given = "Jessie", surname = "Bloggs")) |> 
   add_indi_fact("bir", date = date_approximated(date_calendar(year = 1970), about = TRUE),
                              fact_place = place("Elsewhere")) |>
   add_famg(husband = "@I1@", wife = "@I2@", children = "@I3@") |> 
   add_famg_event("rel", date = date_calendar(year = 1969, month = 1, day = 30),
                         event_place = place(name = "Another place")) |> 
   remove_dates_for_tests() |> 
   df_indi(), "json2")
})


test_that("Function df_famg() @ L171", {
  expect_snapshot_value(gedcom(subm("Me")) |> 
   add_indi(sex = "M") |> 
   add_indi_names(name_pieces(given = "Joe", surname = "Bloggs")) |> 
   add_indi(sex = "F") |> 
   add_indi_names(name_pieces(given = "Jess", surname = "Bloggs")) |> 
   add_indi(sex = "F") |> 
   add_indi_names(name_pieces(given = "Jessie", surname = "Bloggs")) |>
   add_famg(husband = "@I1@", wife = "@I2@", children = "@I3@") |> 
   add_famg_event("rel", date = date_calendar(year = 1969, month = 1, day = 30),
                         event_place = place(name = "Another place")) |> 
   remove_dates_for_tests() |> 
   df_famg(), "json2")
})


test_that("Function df_media() @ L208", {
  expect_snapshot_value(gedcom(subm("Me")) |> 
   add_media(file_reference = "ref1", format = "WAV", source_media = "audio", title = "sounds") |> 
   add_media(file_reference = "ref2", format = "JPEG", source_media = "photo", title = "photo1") |> 
   add_media(file_reference = "ref3", format = "PNG", source_media = "photo", title = "photo2") |> 
   remove_dates_for_tests() |> 
   df_media(), "json2")
})


test_that("Function df_sour() @ L236", {
  expect_snapshot_value(gedcom(subm("Me")) |> 
   add_sour(originator = "author1", title = "book1") |> 
   add_sour(originator = "author2", title = "book2") |> 
   add_sour(originator = "author3", title = "book3") |> 
   remove_dates_for_tests() |> 
   df_sour(), "json2")
})


test_that("Function df_repo() @ L260", {
  expect_snapshot_value(gedcom(subm("Me")) |> 
   add_repo(name = "repo1", repo_address = address(city = "Brighton", state = "E. Sussex", country = "UK")) |> 
   add_repo(name = "repo2", repo_address = address(city = "Orlando", state = "Florida", country = "USA")) |> 
   add_repo(name = "repo3", repo_address = address(city = "Yokohama", country = "Japan")) |> 
   remove_dates_for_tests() |> 
   df_repo(), "json2")
})


test_that("Function df_note() @ L287", {
  expect_snapshot_value(gedcom(subm("Me")) |> 
   add_note(text = "This is a note", user_reference_numbers = 1234) |> 
   add_note(text = "This is also a note", user_reference_numbers = 5678) |> 
   add_note(text = "This may be a note too", user_reference_numbers = 987643) |> 
   remove_dates_for_tests() |> 
   df_note(), "json2")
})

