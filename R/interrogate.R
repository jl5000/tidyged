
unique_record_count <- function(gedcom, tag) {sum(gedcom$level == 0 & gedcom$tag == tag)}

num_indi <- function(gedcom) {unique_record_count(gedcom, "INDI")}
num_fam <- function(gedcom) {unique_record_count(gedcom, "FAM")}
num_subm <- function(gedcom) {unique_record_count(gedcom, "SUBM")}
num_media <- function(gedcom) {unique_record_count(gedcom, "OBJE")}
num_note <- function(gedcom) {unique_record_count(gedcom, "NOTE")}
num_repo <- function(gedcom) {unique_record_count(gedcom, "REPO")}
num_sour <- function(gedcom) {unique_record_count(gedcom, "SOUR")}