
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidygedcom

<!-- badges: start -->

<!-- badges: end -->

Import, create, and summarise family tree GEDCOM files using tidy
dataframes. This package is still in heavy development with the first
operational version unlikely to be complete before January 2021.

## Installation

You can install development version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("jl5000/tidygedcom")
```

## Example

The intent is to allow the user to import GEDCOM files or create them
from scratch using a `ggplot2` type interface; starting with a base
GEDCOM object (with only a header and trailer section) and adding
records as you would add layers to a ggplot object (but instead using
the pipe operator, %\>%).

An example is below:

``` r
library(tidygedcom)

tg <- gedcom(subm("Jamie Lendrum")) %>%
  add_individual(sex = "M", individual_notes = "The central character in the Star Wars Skywalker Saga") %>%
  add_individual_names("Anakin Skywalker", type = "birth", given = "Anakin", surname = "Skywalker") %>% 
  add_individual_event_death(age_at_event = "45y", event_cause = "Killed by son Luke",
                       place_name = "Second Death Star", place_notes = "Orbiting Endor System") %>% 
  add_individual_attribute_religion(attribute_descriptor = "Jedi") %>% 
  add_individual_attribute_possessions(attribute_descriptor = "Lightsaber") %>%
  add_individual(sex = "F", individual_notes = "Queen of Naboo") %>%
  add_individual_names("Padme Amidala") %>% 
  add_individual(sex = "F") %>% 
  add_individual_names("Leia Organa") %>% 
  add_individual(sex = "M") %>%
  add_individual_names("Luke Skywalker") %>% 
  add_family_group(husband = "Anakin", wife = "Padme", children = c("Luke", "Leia")) %>%
  add_individual(sex = "M") %>% 
  add_individual_names("Obi-Wan Kenobi") %>% 
  activate_individual_record("Anakin") %>% 
  add_individual_association("Obi-Wan", association = "Master") %>% 
  add_note("Based on Star Wars") %>% 
  add_source(short_title = "Star Wars", title = "Star Wars Episode IV: A New Hope") %>% 
  add_repository("The Skywalker Saga") %>% 
  add_multimedia(file_reference = "XYZ", format = "JPG")
#> Family link also added to the Individual record for husband
#> Family link also added to the Individual record for wife
#> Family link also added to the Individual record for child @I4@
#> Family link also added to the Individual record for child @I3@

tg
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["level"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["record"],"name":[2],"type":["chr"],"align":["left"]},{"label":["tag"],"name":[3],"type":["chr"],"align":["left"]},{"label":["value"],"name":[4],"type":["chr"],"align":["left"]}],"data":[{"1":"0","2":"HD","3":"HEAD","4":""},{"1":"1","2":"HD","3":"GEDC","4":""},{"1":"2","2":"HD","3":"VERS","4":"5.5.5"},{"1":"2","2":"HD","3":"FORM","4":"LINEAGE-LINKED"},{"1":"3","2":"HD","3":"VERS","4":"5.5.5"},{"1":"1","2":"HD","3":"CHAR","4":"UTF-8"},{"1":"1","2":"HD","3":"SOUR","4":"tidygedcom"},{"1":"2","2":"HD","3":"VERS","4":"0.0.0.900"},{"1":"2","2":"HD","3":"NAME","4":"tidygedcom"},{"1":"2","2":"HD","3":"CORP","4":"Jamie Lendrum"},{"1":"3","2":"HD","3":"ADDR","4":""},{"1":"3","2":"HD","3":"EMAIL","4":"jalendrum@gmail.com"},{"1":"1","2":"HD","3":"DATE","4":"13 DEC 2020"},{"1":"1","2":"HD","3":"SUBM","4":"@U1@"},{"1":"0","2":"@U1@","3":"SUBM","4":""},{"1":"1","2":"@U1@","3":"NAME","4":"Jamie Lendrum"},{"1":"1","2":"@U1@","3":"CHAN","4":""},{"1":"2","2":"@U1@","3":"DATE","4":"13 DEC 2020"},{"1":"0","2":"@I1@","3":"INDI","4":""},{"1":"1","2":"@I1@","3":"SEX","4":"M"},{"1":"1","2":"@I1@","3":"CHAN","4":""},{"1":"2","2":"@I1@","3":"DATE","4":"13 DEC 2020"},{"1":"1","2":"@I1@","3":"NOTE","4":"The central character in the Star Wars Skywalker Saga"},{"1":"1","2":"@I1@","3":"NAME","4":"Anakin Skywalker"},{"1":"2","2":"@I1@","3":"TYPE","4":"birth"},{"1":"2","2":"@I1@","3":"GIVN","4":"Anakin"},{"1":"2","2":"@I1@","3":"SURN","4":"Skywalker"},{"1":"1","2":"@I1@","3":"DEAT","4":"Y"},{"1":"2","2":"@I1@","3":"PLAC","4":"Second Death Star"},{"1":"3","2":"@I1@","3":"NOTE","4":"Orbiting Endor System"},{"1":"2","2":"@I1@","3":"CAUS","4":"Killed by son Luke"},{"1":"2","2":"@I1@","3":"AGE","4":"45y"},{"1":"1","2":"@I1@","3":"RELI","4":"Jedi"},{"1":"1","2":"@I1@","3":"PROP","4":"Lightsaber"},{"1":"1","2":"@I1@","3":"FAMS","4":"@F1@"},{"1":"1","2":"@I1@","3":"ASSO","4":"@I5@"},{"1":"2","2":"@I1@","3":"RELA","4":"Master"},{"1":"0","2":"@I2@","3":"INDI","4":""},{"1":"1","2":"@I2@","3":"SEX","4":"F"},{"1":"1","2":"@I2@","3":"CHAN","4":""},{"1":"2","2":"@I2@","3":"DATE","4":"13 DEC 2020"},{"1":"1","2":"@I2@","3":"NOTE","4":"Queen of Naboo"},{"1":"1","2":"@I2@","3":"NAME","4":"Padme Amidala"},{"1":"1","2":"@I2@","3":"FAMS","4":"@F1@"},{"1":"0","2":"@I3@","3":"INDI","4":""},{"1":"1","2":"@I3@","3":"SEX","4":"F"},{"1":"1","2":"@I3@","3":"CHAN","4":""},{"1":"2","2":"@I3@","3":"DATE","4":"13 DEC 2020"},{"1":"1","2":"@I3@","3":"NAME","4":"Leia Organa"},{"1":"1","2":"@I3@","3":"FAMC","4":"@F1@"},{"1":"2","2":"@I3@","3":"PEDI","4":"birth"},{"1":"0","2":"@I4@","3":"INDI","4":""},{"1":"1","2":"@I4@","3":"SEX","4":"M"},{"1":"1","2":"@I4@","3":"CHAN","4":""},{"1":"2","2":"@I4@","3":"DATE","4":"13 DEC 2020"},{"1":"1","2":"@I4@","3":"NAME","4":"Luke Skywalker"},{"1":"1","2":"@I4@","3":"FAMC","4":"@F1@"},{"1":"2","2":"@I4@","3":"PEDI","4":"birth"},{"1":"0","2":"@F1@","3":"FAM","4":""},{"1":"1","2":"@F1@","3":"HUSB","4":"@I1@"},{"1":"1","2":"@F1@","3":"WIFE","4":"@I2@"},{"1":"1","2":"@F1@","3":"CHIL","4":"@I4@"},{"1":"1","2":"@F1@","3":"CHIL","4":"@I3@"},{"1":"1","2":"@F1@","3":"CHAN","4":""},{"1":"2","2":"@F1@","3":"DATE","4":"13 DEC 2020"},{"1":"0","2":"@I5@","3":"INDI","4":""},{"1":"1","2":"@I5@","3":"SEX","4":"M"},{"1":"1","2":"@I5@","3":"CHAN","4":""},{"1":"2","2":"@I5@","3":"DATE","4":"13 DEC 2020"},{"1":"1","2":"@I5@","3":"NAME","4":"Obi-Wan Kenobi"},{"1":"0","2":"@N1@","3":"NOTE","4":"Based on Star Wars"},{"1":"1","2":"@N1@","3":"CHAN","4":""},{"1":"2","2":"@N1@","3":"DATE","4":"13 DEC 2020"},{"1":"0","2":"@S1@","3":"SOUR","4":""},{"1":"1","2":"@S1@","3":"TITL","4":"Star Wars Episode IV: A New Hope"},{"1":"1","2":"@S1@","3":"ABBR","4":"Star Wars"},{"1":"1","2":"@S1@","3":"CHAN","4":""},{"1":"2","2":"@S1@","3":"DATE","4":"13 DEC 2020"},{"1":"0","2":"@R1@","3":"REPO","4":""},{"1":"1","2":"@R1@","3":"NAME","4":"The Skywalker Saga"},{"1":"1","2":"@R1@","3":"CHAN","4":""},{"1":"2","2":"@R1@","3":"DATE","4":"13 DEC 2020"},{"1":"0","2":"@O1@","3":"OBJE","4":""},{"1":"1","2":"@O1@","3":"FILE","4":"XYZ"},{"1":"2","2":"@O1@","3":"FORM","4":"JPG"},{"1":"1","2":"@O1@","3":"CHAN","4":""},{"1":"2","2":"@O1@","3":"DATE","4":"13 DEC 2020"},{"1":"0","2":"TR","3":"TRLR","4":""}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

Just like a ggplot object requires aesthetics, a GEDCOM file requires
details of a submitter. If no submitter details are given, the username
is used.

Within the package, GEDCOM files are represented as tidy tibbles (a
sub-class of tibble known as `tidygedcom` objects) allowing easy
manipulation and exploitation of existing `tidyverse` infrastructure.

A number of functions are available to provide summaries of `tidygedcom`
objects:

``` r
num_indi(tg)
#> [1] 0
num_fam(tg)
#> [1] 1

str(tg)
#> GEDCOM version 5.5.5 (LINEAGE-LINKED)
#> 
#> Individuals:     0
#> Families:        1
#> Submitters:      1
#> Multimedia objects:  1
#> Notes:           1
#> Sources:     1
#> Repositories:        1
summary(tg)
#> GEDCOM file summary: 
#>  
#>  Submitter:       Jamie Lendrum 
#>  Description:          
#>  Language:         
#>  Character Set:   UTF-8 
#>  
#>  Copyright:        
#>  
#>  Source system:   tidygedcom 
#>  Source system version:  5.5.5 
#>  Product Name:        tidygedcom 
#>  Product Source:      Jamie Lendrum

df_individuals(tg)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["xref"],"name":[1],"type":["chr"],"align":["left"]},{"label":["name"],"name":[2],"type":["chr"],"align":["left"]},{"label":["sex"],"name":[3],"type":["chr"],"align":["left"]},{"label":["date_of_birth"],"name":[4],"type":["chr"],"align":["left"]},{"label":["place_of_birth"],"name":[5],"type":["chr"],"align":["left"]},{"label":["date_of_death"],"name":[6],"type":["chr"],"align":["left"]},{"label":["place_of_death"],"name":[7],"type":["chr"],"align":["left"]},{"label":["mother"],"name":[8],"type":["chr"],"align":["left"]},{"label":["father"],"name":[9],"type":["chr"],"align":["left"]},{"label":["num_siblings"],"name":[10],"type":["chr"],"align":["left"]},{"label":["num_children"],"name":[11],"type":["int"],"align":["right"]},{"label":["last_modified"],"name":[12],"type":["chr"],"align":["left"]}],"data":[{"1":"@I1@","2":"Anakin Skywalker","3":"M","4":"","5":"","6":"","7":"Second Death Star","8":"","9":"","10":"","11":"2","12":"13 DEC 2020"},{"1":"@I2@","2":"Padme Amidala","3":"F","4":"","5":"","6":"","7":"","8":"","9":"","10":"","11":"2","12":"13 DEC 2020"},{"1":"@I3@","2":"Leia Organa","3":"F","4":"","5":"","6":"","7":"","8":"Padme Amidala","9":"Anakin Skywalker","10":"1","11":"0","12":"13 DEC 2020"},{"1":"@I4@","2":"Luke Skywalker","3":"M","4":"","5":"","6":"","7":"","8":"Padme Amidala","9":"Anakin Skywalker","10":"1","11":"0","12":"13 DEC 2020"},{"1":"@I5@","2":"Obi-Wan Kenobi","3":"M","4":"","5":"","6":"","7":"","8":"","9":"","10":"","11":"0","12":"13 DEC 2020"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
df_families(tg)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["xref"],"name":[1],"type":["chr"],"align":["left"]},{"label":["husband"],"name":[2],"type":["chr"],"align":["left"]},{"label":["wife"],"name":[3],"type":["chr"],"align":["left"]},{"label":["marriage_date"],"name":[4],"type":["chr"],"align":["left"]},{"label":["marriage_place"],"name":[5],"type":["chr"],"align":["left"]},{"label":["num_children"],"name":[6],"type":["chr"],"align":["left"]},{"label":["last_modified"],"name":[7],"type":["chr"],"align":["left"]}],"data":[{"1":"@F1@","2":"Anakin Skywalker","3":"Padme Amidala","4":"","5":"","6":"2","7":"13 DEC 2020"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

This package allows limited editing of `tidygedcom` objects
(adding/removing records, as well as the addition of some record
substructures). Editing of existing records is made possible through
‘activation’ (much like the `tidygraph` package). When a record is
created, it automatically becomes the active record, through an object
attribute. Record editing functions then act on this record. Other
records can be activated through a series of `activate_*_record()`
functions.

Two sister packages are planned:

  - `shinygedcom` will provide a shiny app to edit `tidygedcom` objects;
  - `visgedcom` will allow visualisation of `tidygedcom` objects.

## Current status

  - The GEDCOM 5.5.5 specification has been implemented. This is the
    bulk of the internal data structures required (params.R,
    structures.R, records.R, and validate\_params.R);
  - Some initial functions to summarise tidygedcom objects have been
    completed (interrogate.R);
  - Functions to import, export and create tidygedcom objects have been
    completed (import\_export\_create.R);
  - Current work is on updating code for GEDCOM 5.5.5 and the
    modification of tidygedcom objects;
  - Some simple validation checks have been written
    (validate\_gedcom.R), but there won’t be an extensive functionality
    for this given the number of GEDCOM validators available ([such as
    this one](http://ged-inline.elasticbeanstalk.com/validate)).
  - Work on `shinygedcom` and `visgedcom` will begin once an initial
    version of `tidygedcom` is complete.

## References

  - [The GEDCOM 5.5.5 Specification](https://www.gedcom.org/gedcom.html)
