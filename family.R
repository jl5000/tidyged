

devtools::load_all()
devtools::document()
devtools::test()
devtools::check()

test = import_gedcom("../tgdata/Franklins.ged")
test = import_gedcom("../tgdata/royal92.ged")

export_gedcom(test, "../tgdata/test.ged")


network_df <- tibble::tribble(
  ~ from, ~ to,
  "family_record", "family_event_structure",
  "family_record", "note_structure",
  "family_record", "source_citation",
  "family_record", "multimedia_link",
  "family_record", "change_date",
  "individual_record", "personal_name_structure",
  "individual_record", "individual_event_structure",
  "individual_record", "individual_attribute_structure",
  "individual_record", "spouse_to_family_link",
  "individual_record", "child_to_family_link",
  "individual_record", "note_structure",
  "individual_record", "source_citation",
  "individual_record", "multimedia_link",
  "individual_record", "change_date",
  "multimedia_record", "note_structure",
  "multimedia_record", "source_citation",
  "multimedia_record", "change_date",
  "note_record", "source_citation",
  "note_record", "change_date",
  "repository_record", "address_structure",
  "repository_record", "note_structure",
  "repository_record", "change_date",
  "source_record", "note_structure",
  "source_record", "multimedia_link",
  "source_record", "change_date",
  "submission_record", "note_structure",
  "submission_record", "change_date",
  "submitter_record", "address_structure",
  "submitter_record", "multimedia_link",
  "submitter_record", "note_structure",
  "submitter_record", "change_date",
  "association_structure", "source_citation",
  "association_structure", "note_structure",
  "change_date", "note_structure",
  "child_to_family_link", "note_structure",
  "event_detail", "place_structure",
  "event_detail", "address_structure",
  "event_detail", "note_structure",
  "event_detail", "source_citation",
  "event_detail", "multimedia_link",
  "family_event_detail", "event_detail",
  "family_event_structure", "family_event_detail",
  "individual_attribute_structure", "individual_event_detail",
  "individual_event_detail", "event_detail",
  "individual_event_structure", "individual_event_detail",
  "personal_name_pieces", "note_structure",
  "personal_name_pieces", "source_citation",
  "personal_name_structure", "personal_name_pieces",
  "place_structure", "note_structure",
  "source_citation", "multimedia_link",
  "source_citation", "note_structure",
  "source_repository_citation", "note_structure",
  "spouse_to_family_link", "note_structure"
) 

igraph::graph_from_data_frame(network_df) %>% plot()



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


  
  
  
  
  
 
