
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidygedcom

<!-- badges: start -->

[![R-CMD-check](https://github.com/jl5000/tidygedcom/workflows/R-CMD-check/badge.svg)](https://github.com/jl5000/tidygedcom/actions)
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
#> If importing existing GEDCOM files, you should ensure that they are error free.
#> This package assumes imported GEDCOM files are valid and very few validation checks are carried out.
#> Several GEDCOM validators are available, including an online validator at http://ged-inline.elasticbeanstalk.com/

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

print(tg, n = Inf)
#> # A tibble: 88 x 4
#>    level record tag   value                                                  
#>    <dbl> <chr>  <chr> <chr>                                                  
#>  1     0 HD     HEAD  ""                                                     
#>  2     1 HD     GEDC  ""                                                     
#>  3     2 HD     VERS  "5.5.5"                                                
#>  4     2 HD     FORM  "LINEAGE-LINKED"                                       
#>  5     3 HD     VERS  "5.5.5"                                                
#>  6     1 HD     CHAR  "UTF-8"                                                
#>  7     1 HD     SOUR  "tidygedcom"                                           
#>  8     2 HD     VERS  "0.0.0.900"                                            
#>  9     2 HD     NAME  "tidygedcom"                                           
#> 10     2 HD     CORP  "Jamie Lendrum"                                        
#> 11     3 HD     ADDR  ""                                                     
#> 12     3 HD     EMAIL "jalendrum@gmail.com"                                  
#> 13     1 HD     DATE  "19 DEC 2020"                                          
#> 14     1 HD     SUBM  "@U1@"                                                 
#> 15     0 @U1@   SUBM  ""                                                     
#> 16     1 @U1@   NAME  "Jamie Lendrum"                                        
#> 17     1 @U1@   CHAN  ""                                                     
#> 18     2 @U1@   DATE  "19 DEC 2020"                                          
#> 19     0 @I1@   INDI  ""                                                     
#> 20     1 @I1@   SEX   "M"                                                    
#> 21     1 @I1@   CHAN  ""                                                     
#> 22     2 @I1@   DATE  "19 DEC 2020"                                          
#> 23     1 @I1@   NOTE  "The central character in the Star Wars Skywalker Saga"
#> 24     1 @I1@   NAME  "Anakin Skywalker"                                     
#> 25     2 @I1@   TYPE  "birth"                                                
#> 26     2 @I1@   GIVN  "Anakin"                                               
#> 27     2 @I1@   SURN  "Skywalker"                                            
#> 28     1 @I1@   DEAT  "Y"                                                    
#> 29     2 @I1@   PLAC  "Second Death Star"                                    
#> 30     3 @I1@   NOTE  "Orbiting Endor System"                                
#> 31     2 @I1@   CAUS  "Killed by son Luke"                                   
#> 32     2 @I1@   AGE   "45y"                                                  
#> 33     1 @I1@   RELI  "Jedi"                                                 
#> 34     1 @I1@   PROP  "Lightsaber"                                           
#> 35     1 @I1@   FAMS  "@F1@"                                                 
#> 36     1 @I1@   ASSO  "@I5@"                                                 
#> 37     2 @I1@   RELA  "Master"                                               
#> 38     0 @I2@   INDI  ""                                                     
#> 39     1 @I2@   SEX   "F"                                                    
#> 40     1 @I2@   CHAN  ""                                                     
#> 41     2 @I2@   DATE  "19 DEC 2020"                                          
#> 42     1 @I2@   NOTE  "Queen of Naboo"                                       
#> 43     1 @I2@   NAME  "Padme Amidala"                                        
#> 44     1 @I2@   FAMS  "@F1@"                                                 
#> 45     0 @I3@   INDI  ""                                                     
#> 46     1 @I3@   SEX   "F"                                                    
#> 47     1 @I3@   CHAN  ""                                                     
#> 48     2 @I3@   DATE  "19 DEC 2020"                                          
#> 49     1 @I3@   NAME  "Leia Organa"                                          
#> 50     1 @I3@   FAMC  "@F1@"                                                 
#> 51     2 @I3@   PEDI  "birth"                                                
#> 52     0 @I4@   INDI  ""                                                     
#> 53     1 @I4@   SEX   "M"                                                    
#> 54     1 @I4@   CHAN  ""                                                     
#> 55     2 @I4@   DATE  "19 DEC 2020"                                          
#> 56     1 @I4@   NAME  "Luke Skywalker"                                       
#> 57     1 @I4@   FAMC  "@F1@"                                                 
#> 58     2 @I4@   PEDI  "birth"                                                
#> 59     0 @F1@   FAM   ""                                                     
#> 60     1 @F1@   HUSB  "@I1@"                                                 
#> 61     1 @F1@   WIFE  "@I2@"                                                 
#> 62     1 @F1@   CHIL  "@I4@"                                                 
#> 63     1 @F1@   CHIL  "@I3@"                                                 
#> 64     1 @F1@   CHAN  ""                                                     
#> 65     2 @F1@   DATE  "19 DEC 2020"                                          
#> 66     0 @I5@   INDI  ""                                                     
#> 67     1 @I5@   SEX   "M"                                                    
#> 68     1 @I5@   CHAN  ""                                                     
#> 69     2 @I5@   DATE  "19 DEC 2020"                                          
#> 70     1 @I5@   NAME  "Obi-Wan Kenobi"                                       
#> 71     0 @N1@   NOTE  "Based on Star Wars"                                   
#> 72     1 @N1@   CHAN  ""                                                     
#> 73     2 @N1@   DATE  "19 DEC 2020"                                          
#> 74     0 @S1@   SOUR  ""                                                     
#> 75     1 @S1@   TITL  "Star Wars Episode IV: A New Hope"                     
#> 76     1 @S1@   ABBR  "Star Wars"                                            
#> 77     1 @S1@   CHAN  ""                                                     
#> 78     2 @S1@   DATE  "19 DEC 2020"                                          
#> 79     0 @R1@   REPO  ""                                                     
#> 80     1 @R1@   NAME  "The Skywalker Saga"                                   
#> 81     1 @R1@   CHAN  ""                                                     
#> 82     2 @R1@   DATE  "19 DEC 2020"                                          
#> 83     0 @O1@   OBJE  ""                                                     
#> 84     1 @O1@   FILE  "XYZ"                                                  
#> 85     2 @O1@   FORM  "JPG"                                                  
#> 86     1 @O1@   CHAN  ""                                                     
#> 87     2 @O1@   DATE  "19 DEC 2020"                                          
#> 88     0 TR     TRLR  ""
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
#> GEDCOM version 5.5.5 (LINEAGE-LINKED)
#> 
#> Individuals:     5
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
#> 1 @F1@  Anakin S… Padme… ""            ""             2            19 DEC 2020
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
