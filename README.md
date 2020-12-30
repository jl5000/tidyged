
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidygedcom

<!-- badges: start -->

[![R-CMD-check](https://github.com/jl5000/tidygedcom/workflows/R-CMD-check/badge.svg)](https://github.com/jl5000/tidygedcom/actions)
[![](https://codecov.io/gh/jl5000/tidygedcom/branch/master/graph/badge.svg)](https://codecov.io/gh/jl5000/tidygedcom)
[![CodeFactor](https://www.codefactor.io/repository/github/jl5000/tidygedcom/badge)](https://www.codefactor.io/repository/github/jl5000/tidygedcom)
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

tg <- gedcom(subm("Jamie Lendrum"), gedcom_description = "The Skywalker family", language = "English",
             gedcom_copyright = "Disney") %>%
  add_individual(sex = "M", individual_notes = "The central character in the Star Wars Skywalker Saga") %>%
  add_individual_names("Anakin Skywalker", type = "birth", given = "Anakin", surname = "Skywalker") %>%
  add_individual_names("Darth Vader", type = "given", given = "Darth Vader") %>%
  add_individual(sex = "F", individual_notes = "Queen of Naboo") %>%
  add_individual_names("Padme Amidala", given = "Padme", surname = "Amidala") %>% 
  add_individual(sex = "F") %>% 
  add_individual_names("Leia Skywalker", type = "birth", given = "Leia", surname = "Skywalker") %>%
  add_individual_names("Princess Leia Organa", type = "adoptive", 
                       prefix = "Princess", given = "Leia", surname = "Organa") %>% 
  add_individual(sex = "M") %>%
  add_individual_names("Luke Skywalker", type = "birth", given = "Luke", surname = "Skywalker") %>% 
  add_individual(sex = "M") %>% 
  add_individual_names("Obi-Wan Kenobi", type = "birth", given = "Obi-Wan", 
                       nickname = "Ben", surname = "Kenobi") %>% 
  add_family_group(husband = "Anakin", wife = "Padme", children = c("Luke", "Leia")) %>%
  activate_individual_record("Anakin") %>% 
  add_individual_association("Obi-Wan", association = "Master") %>% 
  add_individual_event_death(age_at_event = "45y", event_cause = "Killed by son Luke",
                             place_name = "Second Death Star", place_notes = "Orbiting Endor System") %>% 
  add_individual_attribute_religion(attribute_descriptor = "Jedi") %>% 
  add_individual_attribute_possessions(attribute_descriptor = "Lightsaber") %>%
  add_note("Based on Star Wars") %>% 
  add_source(short_title = "Star Wars", title = "Star Wars Episode IV: A New Hope") %>% 
  add_repository("The Skywalker Saga") %>% 
  add_multimedia(file_reference = "XYZ", format = "JPG")
#> Family link also added to the Individual record for husband
#> Family link also added to the Individual record for wife
#> Family link also added to the Individual record for child @I4@
#> Family link also added to the Individual record for child @I3@

print(tg, n = Inf)
#> # A tibble: 111 x 4
#>     level record tag   value                                                  
#>     <dbl> <chr>  <chr> <chr>                                                  
#>   1     0 HD     HEAD  ""                                                     
#>   2     1 HD     GEDC  ""                                                     
#>   3     2 HD     VERS  "5.5.5"                                                
#>   4     2 HD     FORM  "LINEAGE-LINKED"                                       
#>   5     3 HD     VERS  "5.5.5"                                                
#>   6     1 HD     CHAR  "UTF-8"                                                
#>   7     1 HD     SOUR  "tidygedcom"                                           
#>   8     2 HD     VERS  "0.0.0.900"                                            
#>   9     2 HD     NAME  "tidygedcom"                                           
#>  10     2 HD     CORP  "Jamie Lendrum"                                        
#>  11     3 HD     ADDR  ""                                                     
#>  12     3 HD     EMAIL "jalendrum@gmail.com"                                  
#>  13     1 HD     DATE  "30 DEC 2020"                                          
#>  14     1 HD     LANG  "English"                                              
#>  15     1 HD     SUBM  "@U1@"                                                 
#>  16     1 HD     COPR  "Disney"                                               
#>  17     1 HD     NOTE  "The Skywalker family"                                 
#>  18     0 @U1@   SUBM  ""                                                     
#>  19     1 @U1@   NAME  "Jamie Lendrum"                                        
#>  20     1 @U1@   CHAN  ""                                                     
#>  21     2 @U1@   DATE  "30 DEC 2020"                                          
#>  22     0 @I1@   INDI  ""                                                     
#>  23     1 @I1@   SEX   "M"                                                    
#>  24     1 @I1@   CHAN  ""                                                     
#>  25     2 @I1@   DATE  "30 DEC 2020"                                          
#>  26     1 @I1@   NOTE  "The central character in the Star Wars Skywalker Saga"
#>  27     1 @I1@   NAME  "Anakin Skywalker"                                     
#>  28     2 @I1@   TYPE  "birth"                                                
#>  29     2 @I1@   GIVN  "Anakin"                                               
#>  30     2 @I1@   SURN  "Skywalker"                                            
#>  31     1 @I1@   NAME  "Darth Vader"                                          
#>  32     2 @I1@   TYPE  "given"                                                
#>  33     2 @I1@   GIVN  "Darth Vader"                                          
#>  34     1 @I1@   FAMS  "@F1@"                                                 
#>  35     1 @I1@   ASSO  "@I5@"                                                 
#>  36     2 @I1@   RELA  "Master"                                               
#>  37     1 @I1@   DEAT  "Y"                                                    
#>  38     2 @I1@   PLAC  "Second Death Star"                                    
#>  39     3 @I1@   NOTE  "Orbiting Endor System"                                
#>  40     2 @I1@   CAUS  "Killed by son Luke"                                   
#>  41     2 @I1@   AGE   "45y"                                                  
#>  42     1 @I1@   RELI  "Jedi"                                                 
#>  43     1 @I1@   PROP  "Lightsaber"                                           
#>  44     0 @I2@   INDI  ""                                                     
#>  45     1 @I2@   SEX   "F"                                                    
#>  46     1 @I2@   CHAN  ""                                                     
#>  47     2 @I2@   DATE  "30 DEC 2020"                                          
#>  48     1 @I2@   NOTE  "Queen of Naboo"                                       
#>  49     1 @I2@   NAME  "Padme Amidala"                                        
#>  50     2 @I2@   GIVN  "Padme"                                                
#>  51     2 @I2@   SURN  "Amidala"                                              
#>  52     1 @I2@   FAMS  "@F1@"                                                 
#>  53     0 @I3@   INDI  ""                                                     
#>  54     1 @I3@   SEX   "F"                                                    
#>  55     1 @I3@   CHAN  ""                                                     
#>  56     2 @I3@   DATE  "30 DEC 2020"                                          
#>  57     1 @I3@   NAME  "Leia Skywalker"                                       
#>  58     2 @I3@   TYPE  "birth"                                                
#>  59     2 @I3@   GIVN  "Leia"                                                 
#>  60     2 @I3@   SURN  "Skywalker"                                            
#>  61     1 @I3@   NAME  "Princess Leia Organa"                                 
#>  62     2 @I3@   TYPE  "adoptive"                                             
#>  63     2 @I3@   NPFX  "Princess"                                             
#>  64     2 @I3@   GIVN  "Leia"                                                 
#>  65     2 @I3@   SURN  "Organa"                                               
#>  66     1 @I3@   FAMC  "@F1@"                                                 
#>  67     2 @I3@   PEDI  "birth"                                                
#>  68     0 @I4@   INDI  ""                                                     
#>  69     1 @I4@   SEX   "M"                                                    
#>  70     1 @I4@   CHAN  ""                                                     
#>  71     2 @I4@   DATE  "30 DEC 2020"                                          
#>  72     1 @I4@   NAME  "Luke Skywalker"                                       
#>  73     2 @I4@   TYPE  "birth"                                                
#>  74     2 @I4@   GIVN  "Luke"                                                 
#>  75     2 @I4@   SURN  "Skywalker"                                            
#>  76     1 @I4@   FAMC  "@F1@"                                                 
#>  77     2 @I4@   PEDI  "birth"                                                
#>  78     0 @I5@   INDI  ""                                                     
#>  79     1 @I5@   SEX   "M"                                                    
#>  80     1 @I5@   CHAN  ""                                                     
#>  81     2 @I5@   DATE  "30 DEC 2020"                                          
#>  82     1 @I5@   NAME  "Obi-Wan Kenobi"                                       
#>  83     2 @I5@   TYPE  "birth"                                                
#>  84     2 @I5@   GIVN  "Obi-Wan"                                              
#>  85     2 @I5@   NICK  "Ben"                                                  
#>  86     2 @I5@   SURN  "Kenobi"                                               
#>  87     0 @F1@   FAM   ""                                                     
#>  88     1 @F1@   HUSB  "@I1@"                                                 
#>  89     1 @F1@   WIFE  "@I2@"                                                 
#>  90     1 @F1@   CHIL  "@I4@"                                                 
#>  91     1 @F1@   CHIL  "@I3@"                                                 
#>  92     1 @F1@   CHAN  ""                                                     
#>  93     2 @F1@   DATE  "30 DEC 2020"                                          
#>  94     0 @N1@   NOTE  "Based on Star Wars"                                   
#>  95     1 @N1@   CHAN  ""                                                     
#>  96     2 @N1@   DATE  "30 DEC 2020"                                          
#>  97     0 @S1@   SOUR  ""                                                     
#>  98     1 @S1@   TITL  "Star Wars Episode IV: A New Hope"                     
#>  99     1 @S1@   ABBR  "Star Wars"                                            
#> 100     1 @S1@   CHAN  ""                                                     
#> 101     2 @S1@   DATE  "30 DEC 2020"                                          
#> 102     0 @R1@   REPO  ""                                                     
#> 103     1 @R1@   NAME  "The Skywalker Saga"                                   
#> 104     1 @R1@   CHAN  ""                                                     
#> 105     2 @R1@   DATE  "30 DEC 2020"                                          
#> 106     0 @O1@   OBJE  ""                                                     
#> 107     1 @O1@   FILE  "XYZ"                                                  
#> 108     2 @O1@   FORM  "JPG"                                                  
#> 109     1 @O1@   CHAN  ""                                                     
#> 110     2 @O1@   DATE  "30 DEC 2020"                                          
#> 111     0 TR     TRLR  ""
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
#> Individuals:         5
#> Families:            1
#> Submitters:          1
#> Multimedia objects:  1
#> Notes:               1
#> Sources:             1
#> Repositories:        1
summary(tg)
#> GEDCOM file summary: 
#>  
#>  Submitter:               Jamie Lendrum 
#>  Description:             The Skywalker family 
#>  Language:                English 
#>  Character set:           UTF-8 
#>  
#>  Copyright:               Disney 
#>  
#>  Source system:           tidygedcom 
#>  Source system version:   0.0.0.900 
#>  Product name:            tidygedcom 
#>  Product source:          Jamie Lendrum

df_individuals(tg) %>% print(n = Inf, width = Inf)
#> # A tibble: 5 x 12
#>   xref  name             sex   date_of_birth place_of_birth date_of_death
#>   <chr> <chr>            <chr> <chr>         <chr>          <chr>        
#> 1 @I1@  Anakin Skywalker M     ""            ""             ""           
#> 2 @I2@  Padme Amidala    F     ""            ""             ""           
#> 3 @I3@  Leia Skywalker   F     ""            ""             ""           
#> 4 @I4@  Luke Skywalker   M     ""            ""             ""           
#> 5 @I5@  Obi-Wan Kenobi   M     ""            ""             ""           
#>   place_of_death      mother          father             num_siblings
#>   <chr>               <chr>           <chr>              <chr>       
#> 1 "Second Death Star" ""              ""                 ""          
#> 2 ""                  ""              ""                 ""          
#> 3 ""                  "Padme Amidala" "Anakin Skywalker" "1"         
#> 4 ""                  "Padme Amidala" "Anakin Skywalker" "1"         
#> 5 ""                  ""              ""                 ""          
#>   num_children last_modified
#>          <int> <chr>        
#> 1            2 30 DEC 2020  
#> 2            2 30 DEC 2020  
#> 3            0 30 DEC 2020  
#> 4            0 30 DEC 2020  
#> 5            0 30 DEC 2020
df_families(tg) %>% print(n = Inf, width = Inf)
#> # A tibble: 1 x 7
#>   xref  husband          wife          marriage_date marriage_place num_children
#>   <chr> <chr>            <chr>         <chr>         <chr>          <chr>       
#> 1 @F1@  Anakin Skywalker Padme Amidala ""            ""             2           
#>   last_modified
#>   <chr>        
#> 1 30 DEC 2020
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
