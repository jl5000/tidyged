

devtools::load_all()
devtools::document()
devtools::test()
devtools::check()

test = import_gedcom("../tgdata/Franklins.ged")
test = import_gedcom("../tgdata/royal92.ged")

export_gedcom(test, "../tgdata/test.ged")


gedcom(subm("The Submitter")) %>% 
  add_submission() %>% 
  add_individual(sex = "M") %>% 
  add_individual(sex = "F") %>% 
  add_individual(sex = "F") %>% 
  add_individual(sex = "F") %>% 
  add_family(husband = "@I1@", wife = "@I2@", children = c("@I3@", "@I4@"), submitters = "Submitter") %>%
  add_note("THis is a note") %>% 
  add_source(short_title = "Abbrev", title = "Full title") %>% 
  add_repository("Repository name", city = "London") %>% 
  add_submitter("Submitter 2") %>%
  add_multimedia("Ref: XYZ", format = "jpg") %>% View()

