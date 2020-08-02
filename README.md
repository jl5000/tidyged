
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidygedcom

<!-- badges: start -->

<!-- badges: end -->

Import, create, and summarise family tree GEDCOM files using tidy
dataframes. This package is still in heavy development with the first
operational version unlikely to be complete before October 2020.

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
#> If importing existing GEDCOM files, you should ensure that they are error free.
#> This package assumes imported GEDCOM files are valid and very few validation checks are carried out.
#> Several GEDCOM validators are available, including an online validator at http://ged-inline.elasticbeanstalk.com/

tg <- gedcom(subm("Jamie Lendrum")) %>%
  add_individual(sex = "M", individual_notes = "The central character in the Star Wars Skywalker Saga") %>%
  add_individual_names("Anakin Skywalker", type = "birth", given = "Anakin", surname = "Skywalker") %>% 
  add_individual_event(event_type = "DEAT", age_at_event = "45y", cause_of_event = "Killed by son Luke",
                       place_name = "Second Death Star", place_notes = "Orbiting Endor System") %>% 
  add_individual_attribute(attribute_type = "RELI", attribute_descriptor = "Jedi") %>% 
  add_individual_attribute(attribute_type = "PROP", attribute_descriptor = "Lightsaber") %>%
  add_individual(sex = "F", individual_notes = "Queen of Naboo") %>%
  add_individual_names("Padme Amidala") %>% 
  add_individual(sex = "F") %>% 
  add_individual_names("Leia Organa") %>% 
  add_individual(sex = "M") %>%
  add_individual_names("Luke Skywalker") %>% 
  add_family(husband = "Anakin", wife = "Padme", children = c("Luke", "Leia"), submitters = "Jamie") %>%
  add_individual(sex = "M") %>% 
  add_individual_names("Obi-Wan Kenobi") %>% 
  activate_individual_record("Anakin") %>% 
  add_individual_association("Obi-Wan", association = "Master") %>% 
  add_note("Based on Star Wars") %>% 
  add_source(short_title = "Star Wars", title = "Star Wars Episode IV: A New Hope") %>% 
  add_repository("The Skywalker Saga") %>% 
  add_submitter("George Lucas") %>%
  add_multimedia(file_reference = "XYZ", format = "jpg")
#> Family link also added to the Individual record for husband
#> Family link also added to the Individual record for wife
#> Family link also added to the Individual record for child 1
#> Family link also added to the Individual record for child 2

tg
#> # A tibble: 86 x 4
#>    level record tag   value           
#>    <dbl> <chr>  <chr> <chr>           
#>  1     0 HD     HEAD  ""              
#>  2     1 HD     SOUR  "tidygedcom"    
#>  3     2 HD     VERS  "0.0.0.9000"    
#>  4     1 HD     DATE  "02 AUG 2020"   
#>  5     1 HD     SUBM  "@U1@"          
#>  6     1 HD     GEDC  ""              
#>  7     2 HD     VERS  "5.5.1"         
#>  8     2 HD     FORM  "Lineage-Linked"
#>  9     1 HD     CHAR  "UTF-8"         
#> 10     0 @U1@   SUBM  ""              
#> # … with 76 more rows
```

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
#> [1] 5
num_fam(tg)
#> [1] 1

str(tg)
#> GEDCOM version 5.5.1 (Lineage-Linked)
#> 
#> Individuals:     5
#> Families:        1
#> Submitters:      2
#> Submissions:     0
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
#>  Source system version:  0.0.0.9000 
#>  Product Name:         
#>  Product Source:     

df_individuals(tg)
#> # A tibble: 5 x 12
#>   xref  name  sex   date_of_birth place_of_birth date_of_death place_of_death
#>   <chr> <chr> <chr> <chr>         <chr>          <chr>         <chr>         
#> 1 @I1@  Anak… M     ""            ""             ""            "Second Death…
#> 2 @I2@  Padm… F     ""            ""             ""            ""            
#> 3 @I3@  Leia… F     ""            ""             ""            ""            
#> 4 @I4@  Luke… M     ""            ""             ""            ""            
#> 5 @I5@  Obi-… M     ""            ""             ""            ""            
#> # … with 5 more variables: mother <chr>, father <chr>, num_siblings <chr>,
#> #   num_children <int>, last_modified <chr>
df_families(tg)
#> # A tibble: 1 x 7
#>   xref  husband   wife   marriage_date marriage_place num_children last_modified
#>   <chr> <chr>     <chr>  <chr>         <chr>          <chr>        <chr>        
#> 1 @F1@  Anakin S… Padme… ""            ""             2            02 AUG 2020
```

This package allows limited editing of `tidygedcom` objects
(adding/removing records, as well as the addition of some record
substructures). Editing of existing records is made possible through
‘activation’ (much like the `tidygraph` package). When a record is
created, it automatically becomes the active record, through an object
attribute. Record editing functions then act on this record. Other
records can be activated through a series of `activate_*_record()`
functions.

Two sister packages are planned:

  - `tgvis` will allow visualisation of `tidygedcom` objects;
  - `tgedit` will provide a shiny app to edit `tidygedcom` objects.

## Current status

  - The GEDCOM 5.5.1 specification has been implemented. This is the
    bulk of the internal data structures required (params.R,
    structures.R, records.R, and validate\_params.R);
  - Some initial functions to summarise tidygedcom objects have been
    completed (interrogate.R);
  - Functions to import, export and create tidygedcom objects have been
    completed (import\_export\_create.R);
  - Current work is on the modification of tidygedcom objects;
  - Some simple validation checks have been written
    (validate\_gedcom.R), but there won’t be an extensive functionality
    for this given the number of GEDCOM validators available ([such as
    this one](http://ged-inline.elasticbeanstalk.com/validate)).
  - Work on `tgvis` and `tgedit` will begin once an initial version of
    `tidygedcom` is complete.

## References

  - [The GEDCOM Standard -
    Release 5.5.1](https://edge.fscdn.org/assets/img/documents/ged551-5bac5e57fe88dd37df0e153d9c515335.pdf)
