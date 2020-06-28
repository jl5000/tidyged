
purrr::walk(list.files("R", full.names = TRUE, pattern = "[.]R$"), source)
devtools::document()
testthat::test_dir("tests/testthat")

test = import_gedcom("Franklins.ged")
royals = import_gedcom("royal92.ged")

export_gedcom(test, "test.ged")


  
  
  
  
  
 
