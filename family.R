

devtools::load_all()
devtools::document()
devtools::test()
devtools::check()

test = read_gedcom("../tgdata/Franklins.ged")
test = read_gedcom("../tgdata/royal92.ged")
test = read_gedcom("../tgdata/555SAMPLE.GED")
write_gedcom(test, "../tgdata/test.ged")

