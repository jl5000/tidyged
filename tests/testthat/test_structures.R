# datapasta::tribble_paste(df)


# address_structure -------------------------------------------------------
test_that("address_structure gives expected values", {
  expect_error(address_structure())
  expect_error(address_structure(letters[1:5]))
  expect_error(address_structure("address", city = 1:2))
  expect_error(address_structure("address", state = 1:2))
  expect_error(address_structure("address", postal_code = 1:2))
  expect_error(address_structure("address", phone_numbers = 1:4))
  expect_error(address_structure("address", emails = 1:4))
  expect_error(address_structure("address", fax_numbers = 1:4))
  expect_error(address_structure("address", web_pages = 1:4))
  
  df1 <- address_structure("Road name")
  df2 <- tibble::tribble(
    ~level,   ~tag,      ~value,
    0, "ADDR", "Road name"
  )
  expect_equal(df1, df2)
  
  df1 <- address_structure(letters[1:4])
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "ADDR",    "a",
    1, "CONT",    "b",
    1, "CONT",    "c",
    1, "CONT",    "d",
    1, "ADR1",    "b",
    1, "ADR2",    "c",
    1, "ADR3",    "d"
  )
  expect_equal(df1, df2)
  
  df1 <- address_structure(letters[1:2], country = "UK")
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "ADDR",    "a",
    1, "CONT",    "b",
    1, "ADR1",    "b",
    1, "CTRY",   "UK"
  )
  expect_equal(df1, df2)
})


# association_structure ---------------------------------------------------
test_that("association_structure gives expected values", {
  expect_error(association_structure())
  expect_error(association_structure(1))
  expect_error(association_structure(1:2, "Godfather"))
  
  df1 <- association_structure(1, "Godfather")
  df2 <- tibble::tribble(
    ~level,   ~tag,      ~value,
    0, "ASSO",      "@I1@",
    1, "RELA", "Godfather"
  )
  expect_equal(df1, df2)
  
  df1 <- association_structure(1, "Father", notes = list(note_structure("This is a note")))
  df2 <- tibble::tribble(
    ~level,   ~tag,              ~value,
    0, "ASSO",              "@I1@",
    1, "RELA",            "Father",
    1, "NOTE",    "This is a note"
  )
  expect_equal(df1, df2)
})

# change_date ---------------------------------------------------
test_that("change_date gives expected values", {
  expect_error(change_date())
  
})

# child_to_family_link ---------------------------------------------------
test_that("child_to_family_link gives expected values", {
  expect_error(child_to_family_link())
  expect_error(child_to_family_link(1, pedigree_linkage_type = "foste"))
  expect_error(child_to_family_link(1, child_linkage_status = "challenge"))
  
  df1 <- child_to_family_link(1)
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "FAMC", "@F1@"
  )
  expect_equal(df1, df2)
  
  df1 <- child_to_family_link(1, "birth", "proven")
  df2 <- tibble::tribble(
    ~level,   ~tag,   ~value,
    0, "FAMC",   "@F1@",
    1, "PEDI",  "birth",
    1, "STAT", "proven"
  )
  expect_equal(df1, df2)
})

# event_detail ---------------------------------------------------
test_that("event_detail gives expected values", {
  expect_error(event_detail(restriction_notice = "something"))
  expect_equal(dim(event_detail()), c(0, 3))
  
  df1 <- event_detail(event_classification = "Woodworking")
  df2 <- tibble::tribble(
    ~level,   ~tag,        ~value,
    0, "TYPE", "Woodworking"
  )
  expect_equal(df1, df2)
  
  df1 <- event_detail(place = place_structure("Somewhere"))
  df2 <- tibble::tribble(
    ~level,   ~tag,      ~value,
    0, "PLAC", "Somewhere"
  )
  expect_equal(df1, df2)
  
  
  df1 <- event_detail(address = address_structure(c("House name", "Road")))
  df2 <- tibble::tribble(
    ~level,   ~tag,       ~value,
    0, "ADDR", "House name",
    1, "CONT",       "Road",
    1, "ADR1",       "Road"
  )
  expect_equal(df1, df2)
  
})

# family_event_detail ---------------------------------------------------
test_that("family_event_detail gives expected values", {
  expect_equal(dim(family_event_detail()), c(0, 3))  

  df1 <- family_event_detail(husband_age = 42)
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "HUSB",     "",
    1,  "AGE",   "42"
  )
  expect_equal(df1, df2)
  
  df1 <- family_event_detail(wife_age = 40)
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "WIFE",     "",
    1,  "AGE",   "40"
  )
  expect_equal(df1, df2)
  
  df1 <- family_event_detail(husband_age = 42, wife_age = 40)
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "HUSB",     "",
    1,  "AGE",   "42",
    0, "WIFE",     "",
    1,  "AGE",   "40"
  )
  expect_equal(df1, df2)
  
})

# family_event_structure ---------------------------------------------------
test_that("family_event_structure gives expected values", {
  expect_error(family_event_structure())
  expect_error(family_event_structure("TEST"))
  
  df1 <- family_event_structure("CENS")
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "CENS",     ""
  )
  expect_equal(df1, df2)
  
})

# individual_attribute_structure ---------------------------------------------------
test_that("individual_attribute_structure gives expected values", {
  expect_error(individual_attribute_structure())
  expect_error(individual_attribute_structure("TEST"))
  expect_error(individual_attribute_structure("FACT"))
  expect_error(individual_attribute_structure(c("FACT", "EDUC"), "This is a fact"))
  expect_error(
    individual_attribute_structure(c("NATI", "OCCU"), c("British", "Banker"),
                                   list(individual_event_detail(age = 10))))
  
  df1 <- individual_attribute_structure("NATI", "British")
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "NATI",     "British"
  )
  expect_equal(df1, df2)
  
  df1 <- individual_attribute_structure(c("NATI", "OCCU"), c("British", "Banker"))
  df2 <- tibble::tribble(
    ~level,   ~tag,     ~value,
    0, "NATI", "British",
    0, "OCCU",  "Banker"
  )
  expect_equal(df1, df2)
  
  df1 <- individual_attribute_structure(c("NATI", "OCCU"), c("British", "Banker"),
                                        list(individual_event_detail(age = 0),
                                             individual_event_detail(age = 20)))
  df2 <- tibble::tribble(
    ~level,   ~tag,    ~value,
    0, "NATI", "British",
    1,  "AGE",       "0",
    0, "OCCU",  "Banker",
    1,  "AGE",      "20"
  )
  expect_equal(df1, df2)
  
  df1 <- individual_attribute_structure(c("NATI", "OCCU"), c("British", "Banker"),
                                        list(individual_event_detail(age = 0,
                                                                     event_detail(event_classification = "test1")),
                                             individual_event_detail(age = 20,
                                                                     event_detail(event_classification = "test2"))))
  df2 <- tibble::tribble(
    ~level,   ~tag,    ~value,
    0, "NATI", "British",
    1, "TYPE",   "test1",
    1,  "AGE",       "0",
    0, "OCCU",  "Banker",
    1, "TYPE",   "test2",
    1,  "AGE",      "20"
  )
  expect_equal(df1, df2)
})



# individual_event_detail ---------------------------------------------------
test_that("individual_event_detail gives expected values", {
  expect_equal(dim(individual_event_detail()), c(0, 3))  
  
  df1 <- individual_event_detail(age = 5)
  df2 <- tibble::tribble(
    ~level,  ~tag, ~value,
    0, "AGE",    "5"
  )
  expect_equal(df1, df2)
  
})

# individual_event_structure ---------------------------------------------------
test_that("individual_event_structure gives expected values", {
  expect_error(individual_event_structure())
  expect_error(individual_event_structure("BLAH"))
  expect_error(individual_event_structure("ADOP", adoptive_parent = "WHO"))
  
  df1 <- individual_event_structure("BIRT")
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "BIRT",    "Y"
  )
  expect_equal(df1, df2)
  
  df1 <- individual_event_structure("CHRA")   
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "CHRA",     ""
  )
  expect_equal(df1, df2)
  
  df1 <- individual_event_structure("BIRT", family_ref = 4)
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "BIRT",    "Y",
    1, "FAMC", "@F4@"
  )
  expect_equal(df1, df2)
  
  df1 <- individual_event_structure("ADOP", family_ref = 4, adoptive_parent = "BOTH")
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "ADOP",     "",
    1, "FAMC", "@F4@",
    2, "ADOP", "BOTH"
  )
  expect_equal(df1, df2)
  
})


# lds_individual_ordinance ---------------------------------------------------
test_that("lds_individual_ordinance gives expected values", {
  
  
})

# lds_spouse_sealing ---------------------------------------------------
test_that("lds_spouse_sealing gives expected values", {
  
  
})


# multimedia_link ---------------------------------------------------
test_that("multimedia_link gives expected values", {
  expect_error(multimedia_link())
  expect_error(multimedia_link("ref"))
  expect_error(multimedia_link(1, media_type = "carrier pigeon"))
  expect_error(multimedia_link(1, media_format = "jpeg"))
  
  df1 <- multimedia_link(1)
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "OBJE", "@M1@"
  )
  expect_equal(df1, df2)
  
  df1 <- multimedia_link("ref", media_format = "jpg", media_type = "electronic")
  df2 <- tibble::tribble(
    ~level,   ~tag,       ~value,
    0, "OBJE",           "",
    1, "FILE",        "ref",
    2, "FORM",        "jpg",
    3, "MEDI", "electronic"
  )
  expect_equal(df1, df2)
})

# note_structure ---------------------------------------------------
test_that("note_structure gives expected values", {
  expect_error(note_structure())
  expect_error(note_structure(c("test1", "test2")))
  
  # Basic test with ref number
  df1 <- note_structure(1)
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "NOTE", "@T1@"
  )
  expect_equal(df1, df2)
  
  # Basic test with test text
  df1 <- note_structure("test text")
  df2 <- tibble::tribble(
    ~level,   ~tag,      ~value,
    0, "NOTE", "test text"
  )
  expect_equal(df1, df2)
  
  # Test limits of single note
  df1 <- note_structure(paste0(rep("a", 248), collapse=""))
  df2 <- tibble::tribble(
    ~level,   ~tag,      ~value,
    0, "NOTE", paste0(rep("a", 248), collapse="")
  )
  expect_equal(df1, df2)
  
  # Breach limits of single note by 1
  df1 <- note_structure(paste0(rep("a", 249), collapse=""))
  df2 <- tibble::tribble(
    ~level,   ~tag,      ~value,
    0, "NOTE", paste0(rep("a", 248), collapse=""),
    1, "CONC", "a"
  )
  expect_equal(df1, df2)
  
  # Test with extended note
  df1 <- note_structure(paste0(rep("a", 992), collapse=""))
  df2 <- tibble::tribble(
    ~level,   ~tag,      ~value,
    0, "NOTE", paste0(rep("a", 248), collapse=""),
    1, "CONC", paste0(rep("a", 248), collapse=""),
    1, "CONC", paste0(rep("a", 248), collapse=""),
    1, "CONC", paste0(rep("a", 248), collapse="")
  )
  expect_equal(df1, df2)
  
})

# personal_name_pieces ---------------------------------------------------
test_that("personal_name_pieces gives expected values", {
  expect_equal(dim(personal_name_pieces()), c(0, 3))
  
  df1 <- personal_name_pieces(prefix = "Mr", nick = "J")
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "NPFX",   "Mr",
    0, "NICK",    "J"
  )
  expect_equal(df1, df2)
  
  df1 <- personal_name_pieces(prefix = "Mr", nick = "J",
                              notes = list(note_structure(paste0(rep("a", 992), collapse="")),
                                           note_structure(paste0(rep("b", 992), collapse=""))))
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "NPFX",   "Mr",
    0, "NICK",    "J",
    0, "NOTE", paste0(rep("a", 248), collapse=""),
    1, "CONC", paste0(rep("a", 248), collapse=""),
    1, "CONC", paste0(rep("a", 248), collapse=""),
    1, "CONC", paste0(rep("a", 248), collapse=""),
    0, "NOTE", paste0(rep("b", 248), collapse=""),
    1, "CONC", paste0(rep("b", 248), collapse=""),
    1, "CONC", paste0(rep("b", 248), collapse=""),
    1, "CONC", paste0(rep("b", 248), collapse="")
  )
  expect_equal(df1, df2)
  
})

# personal_name_structure ---------------------------------------------------
test_that("personal_name_structure gives expected values", {
  expect_error(personal_name_structure())
  expect_error(
    personal_name_structure("Joe Bloggs", 
                            phonetic_variation = c("Joe Blogs", "Jo Bloggs"))
  )
  expect_error(
    personal_name_structure("Joe Bloggs", 
                            phonetic_variation = c("Joe Blogs", "Jo Bloggs"),
                            phonetic_type = "Can't spell")
  )
  expect_error(
    personal_name_structure("Joe Bloggs", 
                            phonetic_variation = c("Joe Blogs", "Jo Bloggs"),
                            phonetic_type = c("Can't spell", "Can't spell"),
                            phonetic_name_pieces = list(personal_name_pieces(given = "Joe", 
                                                                             surname = "Blogs")))
  )
    
  
  
  df1 <-  personal_name_structure(name = "Joe /Bloggs/", 
                                  name_pieces = personal_name_pieces(prefix = "Mr",
                                                                     surname = "Bloggs"))
  df2 <- tibble::tribble(
    ~level,   ~tag,         ~value,
    0, "NAME", "Joe /Bloggs/",
    1, "NPFX",           "Mr",
    1, "SURN",       "Bloggs"
  )
  expect_equal(df1, df2)
  
  df1 <- personal_name_structure("Joe Bloggs", 
                                 phonetic_variation = c("Joe Blogs", "Jo Bloggs"),
                                 phonetic_type = c("Can't spell", "Can't spell"))
  df2 <- tibble::tribble(
    ~level,   ~tag,         ~value,
    0, "NAME",   "Joe Bloggs",
    1, "FONE",    "Joe Blogs",
    2, "TYPE",  "Can't spell",
    1, "FONE",    "Jo Bloggs",
    2, "TYPE",  "Can't spell"
  )
  expect_equal(df1, df2)
  
  df1 <- personal_name_structure("Joe Bloggs", 
                                 phonetic_variation = c("Joe Blogs", "Jo Bloggs"),
                                 phonetic_type = c("Can't spell", "Can't spell"),
                                 phonetic_name_pieces = list(personal_name_pieces(given = "Joe", 
                                                                                  surname = "Blogs"),
                                                             personal_name_pieces(given = "Jo",
                                                                                  surname = "Bloggs")))
  df2 <- tibble::tribble(
    ~level,   ~tag,         ~value,
    0, "NAME",   "Joe Bloggs",
    1, "FONE",    "Joe Blogs",
    2, "TYPE",  "Can't spell",
    2, "GIVN",          "Joe",
    2, "SURN",        "Blogs",
    1, "FONE",    "Jo Bloggs",
    2, "TYPE",  "Can't spell",
    2, "GIVN",           "Jo",
    2, "SURN",       "Bloggs"
  )
  expect_equal(df1, df2)
})

# place_structure ---------------------------------------------------
test_that("place_structure gives expected values", {
  expect_error(place_structure())
  expect_error(
    place_structure("London", 
                    phonetic_variation = c("Lundon", "Lundun"))
  )
  expect_error(
    place_structure("London", 
                            phonetic_variation = c("Lundon", "Lundun"),
                            phonetic_type = "English accent")
  )

  df1 <- place_structure("Greenwich", 
                         phonetic_variation = c("Grenidge", "Grenich"),
                         phonetic_type = c("English accent", "English accent"),
                         latitude = "51.5 N",
                         longitude = "0.0 E")
  df2 <- tibble::tribble(
    ~level,   ~tag,           ~value,
    0, "PLAC",      "Greenwich",
    1, "FONE",       "Grenidge",
    2, "TYPE", "English accent",
    1, "FONE",        "Grenich",
    2, "TYPE", "English accent",
    1,  "MAP",               "",
    2, "LATI",         "51.5 N",
    2, "LONG",          "0.0 E"
  )
    
})

# source_citation ---------------------------------------------------
test_that("source_citation gives expected values", {
  expect_error(source_citation())
  
  df1 <- source_citation(1)
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "SOUR", "@S1@"
  )
  expect_equal(df1, df2)
  
  df1 <- source_citation(1, page = 3, event_type = "event")
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "SOUR",  "@S1@",
    1, "PAGE",     "3",
    1, "EVEN", "event"
  )
  expect_equal(df1, df2)
  
  df1 <- source_citation(1, page = 3, role = "a role")
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "SOUR",  "@S1@",
    1, "PAGE",     "3"
  )
  expect_equal(df1, df2)
  
  df1 <- source_citation(1, page = 3, event_type = "event", role = "a role")
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "SOUR",   "@S1@",
    1, "PAGE",      "3",
    1, "EVEN",  "event",
    2, "ROLE", "a role"
  )
  expect_equal(df1, df2)
  
})

# source_repository_citation ---------------------------------------------------
test_that("source_repository_citation gives expected values", {
  expect_error(source_repository_citation())
  expect_error(source_repository_citation(1, media_type = "carrier pigeon"))
  
  df1 <- source_repository_citation(1, call_numbers = c("123", "456"), media_type = "map")
  df2 <- tibble::tribble(
    ~level,   ~tag, ~value,
    0, "REPO", "@R1@",
    1, "CALN",  "123",
    1, "CALN",  "456",
    2, "MEDI",  "map"
  )
  expect_equal(df1, df2)
})

# spouse_to_family_link ---------------------------------------------------
test_that("spouse_to_family_link gives expected values", {
  expect_error(spouse_to_family_link())
  
  
  
})

