
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyged <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/jl5000/tidyged/workflows/R-CMD-check/badge.svg)](https://github.com/jl5000/tidyged/actions)
[![](https://codecov.io/gh/jl5000/tidyged/branch/master/graph/badge.svg)](https://codecov.io/gh/jl5000/tidyged)
[![CodeFactor](https://www.codefactor.io/repository/github/jl5000/tidyged/badge)](https://www.codefactor.io/repository/github/jl5000/tidyged)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

> Families can be difficult - but the data describing them doesn’t need
> to be

Create and summarise family tree GEDCOM files using tidy dataframes.

The package is part of the `gedcompendium` ecosystem of packages. This
ecosystem enables the handling of `tidyged` objects (tibble
representations of GEDCOM files), and the main package of this ecosystem
is [`tidyged`](https://jl5000.github.io/tidyged/).

<img src="man/figures/allhex.png" width="65%" style="display: block; margin: auto;" />

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jl5000/tidyged")
```

## Example

The intent is to allow the user to import GEDCOM files or create them
from scratch using a `ggplot2` type interface; starting with a base
GEDCOM object (with only a header and trailer section) and adding
records as you would add layers to a ggplot object (but instead using
the pipe operator, %\>%).

An example is below:

``` r
library(tidyged)

tg <- gedcom(subm("Jamie Lendrum"), gedcom_description = "The Skywalker family", gedcom_copyright = "None") %>%
  add_indi(sex = "M", indi_notes = "The central character in the Star Wars Skywalker Saga") %>%
  add_indi_names(given = "Anakin", surname = "Skywalker", type = "birth") %>%
  add_indi_names(prefix = "Darth", given = "Vader", type = "given") %>%
  add_indi(sex = "F", indi_notes = "Queen of Naboo") %>%
  add_indi_names(given = "Padme", surname = "Amidala", type = "birth") %>% 
  add_indi(sex = "F") %>% 
  add_indi_names(given = "Leia", surname = "Skywalker", type = "birth") %>%
  add_indi_names(prefix = "Princess", given = "Leia", surname = "Organa", type = "adoptive") %>% 
  add_indi(sex = "M") %>%
  add_indi_names(given = "Luke", surname = "Skywalker", type = "birth") %>% 
  add_indi(sex = "M") %>% 
  add_indi_names(given = "Obi-Wan", nickname = "Ben", surname = "Kenobi", type = "birth") %>% 
  add_famg(husband = "Anakin", wife = "Padme", children = c("Luke", "Leia")) %>%
  activate_indi("Anakin") %>% 
  add_indi_association("Obi-Wan", association = "Master") %>% 
  add_indi_event_death(age_at_event = "45y", event_cause = "Killed by son Luke",
                       place_name = "Second Death Star", place_notes = "Orbiting Endor System") %>% 
  add_indi_attr_religion(attribute_descriptor = "Jedi") %>% 
  add_indi_attr_possessions(attribute_descriptor = "Lightsaber") %>%
  add_note("Based on Star Wars") %>% 
  add_sour(short_title = "Star Wars", title = "Star Wars Episode IV: A New Hope") %>% 
  add_repo("The Skywalker Saga") %>% 
  add_media(file_reference = "XYZ", format = "JPG")

print(tg, n = Inf)
#> # A tibble: 115 x 4
#>     level record tag   value                                                  
#>     <dbl> <chr>  <chr> <chr>                                                  
#>   1     0 HD     HEAD  ""                                                     
#>   2     1 HD     GEDC  ""                                                     
#>   3     2 HD     VERS  "5.5.5"                                                
#>   4     2 HD     FORM  "LINEAGE-LINKED"                                       
#>   5     3 HD     VERS  "5.5.5"                                                
#>   6     1 HD     CHAR  "UTF-8"                                                
#>   7     1 HD     DEST  "tidyged"                                              
#>   8     1 HD     SOUR  "tidyged"                                              
#>   9     2 HD     VERS  "0.0.0"                                                
#>  10     2 HD     NAME  "The 'tidyged' package for the R language"             
#>  11     2 HD     CORP  "Jamie Lendrum"                                        
#>  12     3 HD     ADDR  ""                                                     
#>  13     3 HD     EMAIL "jalendrum@gmail.com"                                  
#>  14     3 HD     WWW   "https://jl5000.github.io/tidyged/"                    
#>  15     1 HD     DATE  "20 MAR 2021"                                          
#>  16     1 HD     LANG  "English"                                              
#>  17     1 HD     SUBM  "@U1@"                                                 
#>  18     1 HD     COPR  "None"                                                 
#>  19     1 HD     NOTE  "The Skywalker family"                                 
#>  20     0 @U1@   SUBM  ""                                                     
#>  21     1 @U1@   NAME  "Jamie Lendrum"                                        
#>  22     1 @U1@   CHAN  ""                                                     
#>  23     2 @U1@   DATE  "20 MAR 2021"                                          
#>  24     0 @I1@   INDI  ""                                                     
#>  25     1 @I1@   SEX   "M"                                                    
#>  26     1 @I1@   NOTE  "The central character in the Star Wars Skywalker Saga"
#>  27     1 @I1@   NAME  "Anakin /Skywalker/"                                   
#>  28     2 @I1@   TYPE  "birth"                                                
#>  29     2 @I1@   GIVN  "Anakin"                                               
#>  30     2 @I1@   SURN  "Skywalker"                                            
#>  31     1 @I1@   NAME  "Darth Vader"                                          
#>  32     2 @I1@   TYPE  "given"                                                
#>  33     2 @I1@   NPFX  "Darth"                                                
#>  34     2 @I1@   GIVN  "Vader"                                                
#>  35     1 @I1@   FAMS  "@F1@"                                                 
#>  36     1 @I1@   ASSO  "@I5@"                                                 
#>  37     2 @I1@   RELA  "Master"                                               
#>  38     1 @I1@   DEAT  "Y"                                                    
#>  39     2 @I1@   PLAC  "Second Death Star"                                    
#>  40     3 @I1@   NOTE  "Orbiting Endor System"                                
#>  41     2 @I1@   CAUS  "Killed by son Luke"                                   
#>  42     2 @I1@   AGE   "45y"                                                  
#>  43     1 @I1@   RELI  "Jedi"                                                 
#>  44     1 @I1@   PROP  "Lightsaber"                                           
#>  45     1 @I1@   CHAN  ""                                                     
#>  46     2 @I1@   DATE  "20 MAR 2021"                                          
#>  47     0 @I2@   INDI  ""                                                     
#>  48     1 @I2@   SEX   "F"                                                    
#>  49     1 @I2@   NOTE  "Queen of Naboo"                                       
#>  50     1 @I2@   NAME  "Padme /Amidala/"                                      
#>  51     2 @I2@   TYPE  "birth"                                                
#>  52     2 @I2@   GIVN  "Padme"                                                
#>  53     2 @I2@   SURN  "Amidala"                                              
#>  54     1 @I2@   FAMS  "@F1@"                                                 
#>  55     1 @I2@   CHAN  ""                                                     
#>  56     2 @I2@   DATE  "20 MAR 2021"                                          
#>  57     0 @I3@   INDI  ""                                                     
#>  58     1 @I3@   SEX   "F"                                                    
#>  59     1 @I3@   NAME  "Leia /Skywalker/"                                     
#>  60     2 @I3@   TYPE  "birth"                                                
#>  61     2 @I3@   GIVN  "Leia"                                                 
#>  62     2 @I3@   SURN  "Skywalker"                                            
#>  63     1 @I3@   NAME  "Princess Leia /Organa/"                               
#>  64     2 @I3@   TYPE  "adoptive"                                             
#>  65     2 @I3@   NPFX  "Princess"                                             
#>  66     2 @I3@   GIVN  "Leia"                                                 
#>  67     2 @I3@   SURN  "Organa"                                               
#>  68     1 @I3@   FAMC  "@F1@"                                                 
#>  69     2 @I3@   PEDI  "birth"                                                
#>  70     1 @I3@   CHAN  ""                                                     
#>  71     2 @I3@   DATE  "20 MAR 2021"                                          
#>  72     0 @I4@   INDI  ""                                                     
#>  73     1 @I4@   SEX   "M"                                                    
#>  74     1 @I4@   NAME  "Luke /Skywalker/"                                     
#>  75     2 @I4@   TYPE  "birth"                                                
#>  76     2 @I4@   GIVN  "Luke"                                                 
#>  77     2 @I4@   SURN  "Skywalker"                                            
#>  78     1 @I4@   FAMC  "@F1@"                                                 
#>  79     2 @I4@   PEDI  "birth"                                                
#>  80     1 @I4@   CHAN  ""                                                     
#>  81     2 @I4@   DATE  "20 MAR 2021"                                          
#>  82     0 @I5@   INDI  ""                                                     
#>  83     1 @I5@   SEX   "M"                                                    
#>  84     1 @I5@   NAME  "Obi-Wan 'Ben' /Kenobi/"                               
#>  85     2 @I5@   TYPE  "birth"                                                
#>  86     2 @I5@   GIVN  "Obi-Wan"                                              
#>  87     2 @I5@   NICK  "Ben"                                                  
#>  88     2 @I5@   SURN  "Kenobi"                                               
#>  89     1 @I5@   CHAN  ""                                                     
#>  90     2 @I5@   DATE  "20 MAR 2021"                                          
#>  91     0 @F1@   FAM   ""                                                     
#>  92     1 @F1@   HUSB  "@I1@"                                                 
#>  93     1 @F1@   WIFE  "@I2@"                                                 
#>  94     1 @F1@   CHIL  "@I4@"                                                 
#>  95     1 @F1@   CHIL  "@I3@"                                                 
#>  96     1 @F1@   CHAN  ""                                                     
#>  97     2 @F1@   DATE  "20 MAR 2021"                                          
#>  98     0 @N1@   NOTE  "Based on Star Wars"                                   
#>  99     1 @N1@   CHAN  ""                                                     
#> 100     2 @N1@   DATE  "20 MAR 2021"                                          
#> 101     0 @S1@   SOUR  ""                                                     
#> 102     1 @S1@   TITL  "Star Wars Episode IV: A New Hope"                     
#> 103     1 @S1@   ABBR  "Star Wars"                                            
#> 104     1 @S1@   CHAN  ""                                                     
#> 105     2 @S1@   DATE  "20 MAR 2021"                                          
#> 106     0 @R1@   REPO  ""                                                     
#> 107     1 @R1@   NAME  "The Skywalker Saga"                                   
#> 108     1 @R1@   CHAN  ""                                                     
#> 109     2 @R1@   DATE  "20 MAR 2021"                                          
#> 110     0 @O1@   OBJE  ""                                                     
#> 111     1 @O1@   FILE  "XYZ"                                                  
#> 112     2 @O1@   FORM  "JPG"                                                  
#> 113     1 @O1@   CHAN  ""                                                     
#> 114     2 @O1@   DATE  "20 MAR 2021"                                          
#> 115     0 TR     TRLR  ""
```

Just like a ggplot object requires aesthetics, a GEDCOM file requires
details of a submitter. If no submitter details are given, the username
is used.

Within the package, GEDCOM files are represented as a sub-class of
tibble known as `tidyged` objects, allowing easy manipulation and
exploitation of existing `tidyverse` infrastructure.

A number of functions are available to provide summaries of `tidyged`
objects:

``` r
num_indi(tg)
#> [1] 5
num_famg(tg)
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
#>  Source system:           tidyged 
#>  Source system version:   0.0.0 
#>  Product name:            The 'tidyged' package for the R language 
#>  Product source:          Jamie Lendrum

df_indi(tg) %>% knitr::kable()
```

| xref | name                 | sex | date\_of\_birth | place\_of\_birth | date\_of\_death | place\_of\_death  | mother        | father           | num\_siblings | num\_children | last\_modified |
| :--- | :------------------- | :-- | :-------------- | :--------------- | :-------------- | :---------------- | :------------ | :--------------- | :------------ | ------------: | :------------- |
| @I1@ | Anakin Skywalker     | M   |                 |                  |                 | Second Death Star |               |                  |               |             2 | 20 MAR 2021    |
| @I2@ | Padme Amidala        | F   |                 |                  |                 |                   |               |                  |               |             2 | 20 MAR 2021    |
| @I3@ | Leia Skywalker       | F   |                 |                  |                 |                   | Padme Amidala | Anakin Skywalker | 1             |             0 | 20 MAR 2021    |
| @I4@ | Luke Skywalker       | M   |                 |                  |                 |                   | Padme Amidala | Anakin Skywalker | 1             |             0 | 20 MAR 2021    |
| @I5@ | Obi-Wan ‘Ben’ Kenobi | M   |                 |                  |                 |                   |               |                  |               |             0 | 20 MAR 2021    |

``` r
df_famg(tg) %>% knitr::kable()
```

| xref | husband          | wife          | marriage\_date | marriage\_place | num\_children | last\_modified |
| :--- | :--------------- | :------------ | :------------- | :-------------- | :------------ | :------------- |
| @F1@ | Anakin Skywalker | Padme Amidala |                |                 | 2             | 20 MAR 2021    |

This package allows limited editing of `tidyged` objects
(adding/removing records, as well as the addition of some record
substructures). Editing of existing records is made possible through
‘activation’ (much like the `tidygraph` package). When a record is
created, it automatically becomes the active record, through an object
attribute. Record editing functions then act on this record. Other
records can be activated through a series of `activate_*()` functions.

## References

  - [The GEDCOM 5.5.5 Specification](https://www.gedcom.org/gedcom.html)
