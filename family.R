

devtools::load_all()
devtools::document()
devtools::test()
devtools::check()

test = import_gedcom("../tgdata/Franklins.ged")
test = import_gedcom("../tgdata/royal92.ged")
test = import_gedcom("../tgdata/555SAMPLE.GED")
export_gedcom(test, "../tgdata/test.ged")

