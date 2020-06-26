
# Need helpers for dates and notes

library(tidyverse)
library(testthat)
library(roxytest)

list.files("R", full.names = TRUE, pattern = "[.]R$") %>% 
  walk(source)


test = import_gedcom("Franklins.ged")
royals = import_gedcom("royal92.ged")

export_gedcom(test, "test.ged")


  
  
  
  
  
 
