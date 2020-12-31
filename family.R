

devtools::load_all()
devtools::document()
devtools::test()
devtools::check()

test = read_gedcom("../tgdata/Franklins.ged")
test = read_gedcom("../tgdata/royal92.ged")
test = read_gedcom("../tgdata/555SAMPLE.GED")
write_gedcom(test, "../tgdata/test.ged")

gedcom() %>% 
  add_repository("FreeBMD") %>%
  add_multimedia("filepath", "JPG", "photo", "Someone's birth certificate",
                 multimedia_notes = "Note about the file") %>% 
  add_source(events_recorded = "BIRT", 
             date_period_covered = date_period(start_year = 1950, start_month = 3, start_day = 4,
                                               end_year = start_year, end_month = start_month, end_day = start_day),
             jurisdiction = "UK",
             responsible_agency = "FreeBMD",
             title = "Birth certificate for someone",
             source_text = "...",
             automated_record_id = "5463452-6",
             data_notes = "Signature of registrar is illegible",
             multimedia_links = "filepath") %>% 
  add_individual(sex = "M") %>% 
  add_individual_names("Joe Bloggs") %>%  #citation for pieces
  add_individual_event_birth(event_date = date_value(start_year = 1950, start_month = 2, start_day = 20)) #citation