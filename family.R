

devtools::load_all()
devtools::document()
devtools::test()
devtools::check()

test = import_gedcom("../tgdata/Franklins.ged")
test = import_gedcom("../tgdata/royal92.ged")

export_gedcom(test, "../tgdata/test.ged")


full_ged <- gedcom(subm("Submitter name", 
                        address = ADDRESS_STRUCTURE(c("House", "Street", "City"),
                                                    address_city = "City",
                                                    address_state = "County",
                                                    address_postal_code = "ABC 123",
                                                    address_country = "UK",
                                                    phone_number = "0123456789",
                                                    address_email = "email@domain.com",
                                                    address_fax = "987456",
                                                    address_web_page = "www.website.com"),
                        language_preference = "English",
                        automated_record_id = "Auto ID",
                        submitter_registered_rfn = "1547892",
                        multimedia_links = list(MULTIMEDIA_LINK())),
                   gedcom_description = paste0("This is a", paste(rep(" very", 100), collapse=""), " long description"),
                   source_data_name = "Data source",
                   source_data_copyright = paste0("This is a", paste(rep(" very", 100), collapse=""), " long copyright"),
                   gedcom_copyright = "Gedcom copyright",
                   source_data_date = date_exact(),
                   receiving_system = "Receiving system",
                   language = "English",
                   char_set = "UTF-8",
                   char_set_version = "v1.0")


  
  
  
  
  
 
