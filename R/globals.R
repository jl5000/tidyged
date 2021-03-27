# Stop the NOTES from R CMD CHECK when creating columns with mutate()
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("level", "record", "tag", "value", ".", "num_children", "xref",
                           "mother_xref", "father_xref", "num_siblings", "full", "n"))


.pkgenv <- new.env(parent=emptyenv())

.pkgenv$record_tag_indi <- "INDI"
.pkgenv$record_tag_famg <- "FAM"
.pkgenv$record_tag_subm <- "SUBM"
.pkgenv$record_tag_repo <- "REPO"
.pkgenv$record_tag_obje <- "OBJE"
.pkgenv$record_tag_note <- "NOTE"
.pkgenv$record_tag_sour <- "SOUR"

.pkgenv$record_string_indi <- "Individual"
.pkgenv$record_string_famg <- "Family group"
.pkgenv$record_string_subm <- "Submitter"
.pkgenv$record_string_repo <- "Repository"
.pkgenv$record_string_obje <- "Multimedia"
.pkgenv$record_string_note <- "Note"
.pkgenv$record_string_sour <- "Source"


