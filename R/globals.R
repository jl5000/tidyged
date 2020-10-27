# Stop the NOTES from R CMD CHECK when creating columns with mutate()
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("level", "record", "tag", "value", ".", "num_children", "xref",
                           "mother_xref", "father_xref", "num_siblings", "full"))


.pkgenv <- new.env(parent=emptyenv())

.pkgenv$xref_prefix_indi <- "I"
.pkgenv$xref_prefix_fam <- "F"
.pkgenv$xref_prefix_subm <- "U"
.pkgenv$xref_prefix_repo <- "R"
.pkgenv$xref_prefix_obje <- "O"
.pkgenv$xref_prefix_note <- "N"
.pkgenv$xref_prefix_sour <- "S"

.pkgenv$record_tag_indi <- "INDI"
.pkgenv$record_tag_fam <- "FAM"
.pkgenv$record_tag_subm <- "SUBM"
.pkgenv$record_tag_repo <- "REPO"
.pkgenv$record_tag_obje <- "OBJE"
.pkgenv$record_tag_note <- "NOTE"
.pkgenv$record_tag_sour <- "SOUR"

.pkgenv$record_string_indi <- "individual"
.pkgenv$record_string_fam <- "family group"
.pkgenv$record_string_subm <- "submitter"
.pkgenv$record_string_repo <- "repository"
.pkgenv$record_string_obje <- "multimedia"
.pkgenv$record_string_note <- "note"
.pkgenv$record_string_sour <- "source"

.pkgenv$BOM_UTF8 <- c("ef", "bb", "bf")
.pkgenv$BOM_UTF16_LE <- c("ff", "fe")
.pkgenv$BOM_UTF16_BE <- c("fe", "ff")

