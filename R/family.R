
# Need helpers for dates and notes

library(tidyverse)
library(testthat)
library(roxytest)
source("R/helpers.R")
source("R/helpers_dates.R")
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

export_gedcom(test, "test.ged")


  
  
  
  
  
 
