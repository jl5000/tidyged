
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidygedcom

<!-- badges: start -->

[![R-CMD-check](https://github.com/jl5000/tidygedcom/workflows/R-CMD-check/badge.svg)](https://github.com/jl5000/tidygedcom/actions)
[![](https://codecov.io/gh/jl5000/tidygedcom/branch/master/graph/badge.svg)](https://codecov.io/gh/jl5000/tidygedcom)
[![CodeFactor](https://www.codefactor.io/repository/github/jl5000/tidygedcom/badge)](https://www.codefactor.io/repository/github/jl5000/tidygedcom)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

> Families can be difficult - but the data describing them doesn’t need
> to be

Import, create, and summarise family tree GEDCOM files using tidy
dataframes. This package is still in heavy development with the first
operational version likely around March 2021.

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

tg <- gedcom(subm("Jamie Lendrum"), gedcom_description = "The Skywalker family", gedcom_copyright = "None") %>%
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
#> Family link also added to the Individual record for husband: Anakin Skywalker
#> Family link also added to the Individual record for wife: Padme Amidala
#> Family link also added to the Individual record for child: Luke Skywalker
#> Family link also added to the Individual record for child: Leia Skywalker

print(tg, n = Inf)
#> # A tibble: 113 x 4
#>     level record tag   value                                                  
#>     <dbl> <chr>  <chr> <chr>                                                  
#>   1     0 HD     HEAD  ""                                                     
#>   2     1 HD     GEDC  ""                                                     
#>   3     2 HD     VERS  "5.5.5"                                                
#>   4     2 HD     FORM  "LINEAGE-LINKED"                                       
#>   5     3 HD     VERS  "5.5.5"                                                
#>   6     1 HD     CHAR  "UTF-8"                                                
#>   7     1 HD     DEST  "tidygedcom"                                           
#>   8     1 HD     SOUR  "tidygedcom"                                           
#>   9     2 HD     VERS  "0.0.0"                                                
#>  10     2 HD     NAME  "tidygedcom"                                           
#>  11     2 HD     CORP  "Jamie Lendrum"                                        
#>  12     3 HD     ADDR  ""                                                     
#>  13     3 HD     EMAIL "jalendrum@gmail.com"                                  
#>  14     3 HD     WWW   "https://jl5000.github.io/tidygedcom/"                 
#>  15     1 HD     DATE  "10 JAN 2021"                                          
#>  16     1 HD     LANG  "English"                                              
#>  17     1 HD     SUBM  "@U1@"                                                 
#>  18     1 HD     COPR  "None"                                                 
#>  19     1 HD     NOTE  "The Skywalker family"                                 
#>  20     0 @U1@   SUBM  ""                                                     
#>  21     1 @U1@   NAME  "Jamie Lendrum"                                        
#>  22     1 @U1@   CHAN  ""                                                     
#>  23     2 @U1@   DATE  "10 JAN 2021"                                          
#>  24     0 @I1@   INDI  ""                                                     
#>  25     1 @I1@   SEX   "M"                                                    
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
#>  44     1 @I1@   CHAN  ""                                                     
#>  45     2 @I1@   DATE  "10 JAN 2021"                                          
#>  46     0 @I2@   INDI  ""                                                     
#>  47     1 @I2@   SEX   "F"                                                    
#>  48     1 @I2@   NOTE  "Queen of Naboo"                                       
#>  49     1 @I2@   NAME  "Padme Amidala"                                        
#>  50     2 @I2@   GIVN  "Padme"                                                
#>  51     2 @I2@   SURN  "Amidala"                                              
#>  52     1 @I2@   FAMS  "@F1@"                                                 
#>  53     1 @I2@   CHAN  ""                                                     
#>  54     2 @I2@   DATE  "10 JAN 2021"                                          
#>  55     0 @I3@   INDI  ""                                                     
#>  56     1 @I3@   SEX   "F"                                                    
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
#>  68     1 @I3@   CHAN  ""                                                     
#>  69     2 @I3@   DATE  "10 JAN 2021"                                          
#>  70     0 @I4@   INDI  ""                                                     
#>  71     1 @I4@   SEX   "M"                                                    
#>  72     1 @I4@   NAME  "Luke Skywalker"                                       
#>  73     2 @I4@   TYPE  "birth"                                                
#>  74     2 @I4@   GIVN  "Luke"                                                 
#>  75     2 @I4@   SURN  "Skywalker"                                            
#>  76     1 @I4@   FAMC  "@F1@"                                                 
#>  77     2 @I4@   PEDI  "birth"                                                
#>  78     1 @I4@   CHAN  ""                                                     
#>  79     2 @I4@   DATE  "10 JAN 2021"                                          
#>  80     0 @I5@   INDI  ""                                                     
#>  81     1 @I5@   SEX   "M"                                                    
#>  82     1 @I5@   NAME  "Obi-Wan Kenobi"                                       
#>  83     2 @I5@   TYPE  "birth"                                                
#>  84     2 @I5@   GIVN  "Obi-Wan"                                              
#>  85     2 @I5@   NICK  "Ben"                                                  
#>  86     2 @I5@   SURN  "Kenobi"                                               
#>  87     1 @I5@   CHAN  ""                                                     
#>  88     2 @I5@   DATE  "10 JAN 2021"                                          
#>  89     0 @F1@   FAM   ""                                                     
#>  90     1 @F1@   HUSB  "@I1@"                                                 
#>  91     1 @F1@   WIFE  "@I2@"                                                 
#>  92     1 @F1@   CHIL  "@I4@"                                                 
#>  93     1 @F1@   CHIL  "@I3@"                                                 
#>  94     1 @F1@   CHAN  ""                                                     
#>  95     2 @F1@   DATE  "10 JAN 2021"                                          
#>  96     0 @N1@   NOTE  "Based on Star Wars"                                   
#>  97     1 @N1@   CHAN  ""                                                     
#>  98     2 @N1@   DATE  "10 JAN 2021"                                          
#>  99     0 @S1@   SOUR  ""                                                     
#> 100     1 @S1@   TITL  "Star Wars Episode IV: A New Hope"                     
#> 101     1 @S1@   ABBR  "Star Wars"                                            
#> 102     1 @S1@   CHAN  ""                                                     
#> 103     2 @S1@   DATE  "10 JAN 2021"                                          
#> 104     0 @R1@   REPO  ""                                                     
#> 105     1 @R1@   NAME  "The Skywalker Saga"                                   
#> 106     1 @R1@   CHAN  ""                                                     
#> 107     2 @R1@   DATE  "10 JAN 2021"                                          
#> 108     0 @O1@   OBJE  ""                                                     
#> 109     1 @O1@   FILE  "XYZ"                                                  
#> 110     2 @O1@   FORM  "JPG"                                                  
#> 111     1 @O1@   CHAN  ""                                                     
#> 112     2 @O1@   DATE  "10 JAN 2021"                                          
#> 113     0 TR     TRLR  ""
```

Just like a ggplot object requires aesthetics, a GEDCOM file requires
details of a submitter. If no submitter details are given, the username
is used.

Within the package, GEDCOM files are represented as a sub-class of
tibble known as `tidygedcom` objects, allowing easy manipulation and
exploitation of existing `tidyverse` infrastructure.

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
#>  Copyright:               None 
#>  
#>  Source system:           tidygedcom 
#>  Source system version:   0.0.0 
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
#> 1            2 10 JAN 2021  
#> 2            2 10 JAN 2021  
#> 3            0 10 JAN 2021  
#> 4            0 10 JAN 2021  
#> 5            0 10 JAN 2021
df_families(tg) %>% print(n = Inf, width = Inf)
#> # A tibble: 1 x 7
#>   xref  husband          wife          marriage_date marriage_place num_children
#>   <chr> <chr>            <chr>         <chr>         <chr>          <chr>       
#> 1 @F1@  Anakin Skywalker Padme Amidala ""            ""             2           
#>   last_modified
#>   <chr>        
#> 1 10 JAN 2021
```

This package allows limited editing of `tidygedcom` objects
(adding/removing records, as well as the addition of some record
substructures). Editing of existing records is made possible through
‘activation’ (much like the `tidygraph` package). When a record is
created, it automatically becomes the active record, through an object
attribute. Record editing functions then act on this record. Other
records can be activated through a series of `activate_*_record()`
functions.

## Current status

The package and its internal data structures (contained in a separate
`tidygedcom.internals` package) are almost complete and will soon be
submitted to CRAN.

Three further packages are also planned, to provide an ecosystem called
the `gedcompendium`:

  - `shinygedcom` will provide a shiny app to edit `tidygedcom` objects;
  - `visgedcom` will allow visualisation of `tidygedcom` objects;
  - `tidygedcom.sources` will provide functions to automate the
    representation of facts from various source documents (e.g. birth
    certificates, censuses).

## References

  - [The GEDCOM 5.5.5 Specification](https://www.gedcom.org/gedcom.html)
