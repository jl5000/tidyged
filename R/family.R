
# Need helpers for dates and notes

library(tidyverse)
library(testthat)
source("R/helpers.R")
source("R/params.R")
source("R/structures.R")
source("R/records.R")
source("R/import_export.R")
source("R/validate.R")
source("R/interrogate.R")
source("R/visualise.R")
source("R/edit.R")



test = import_gedcom("Franklins.ged")
royals = import_gedcom("royal92.ged")

export_gedcom(test, "test.txt")

gedcom() %>% 
  # add_individual function will need to bring up a level some of these params
  add_individual(name = "Jamie Lendrum", given = "Jamie", surname = "Lendrum", sex = "M",
                 address_lines = c("6 Widgeon Court", "Fareham", "Hampshire", "PO16 8PW")) %>%
  add_family() %>%
  add_note("Blah de blah")
  update_individual(dob = "fwefe")
  
  
  
  
  
 