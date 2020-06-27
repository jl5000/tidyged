

library(magrittr)
library(testthat)
library(roxytest)

purrr::walk(list.files("R", full.names = TRUE, pattern = "[.]R$"), source)


test = import_gedcom("Franklins.ged")
royals = import_gedcom("royal92.ged")

export_gedcom(test, "test.ged")


  
  
  
  
  
 
