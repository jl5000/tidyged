# I NEED TO:

# 1. IMPORT/EXPORT
# 2. VALIDATE
# 3. INTERROGATE
# 4. VISUALISE
# 5. EDIT
  # add/remove/update individual/family/source/repo/note
# 6. DEFINE STRUCTURES
# 7 TEST

# Need helpers for dates and notes

library(tidyverse)
library(testthat)
source("helpers.R")
source("parms.R")
source("structures.R")
source("records.R")
source("import_export.R")
source("interrogate.R")
source("visualise.R")
source("edit.R")



test = import_gedcom(file.choose())


export_gedcom(test, "test.txt")

