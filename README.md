
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
# install.packages("remotes")
remotes::install_github("jl5000/tidyged")
```

## Example

The intent is to allow the user to import GEDCOM files or create them
from scratch using a `ggplot2` type interface; starting with a base
GEDCOM object (with only a header and trailer section) and adding
records as you would add layers to a ggplot object (but instead using
the pipe operator, \|\>).

An example is below:

``` r
library(tidyged)

tg <- gedcom(subm("Jamie Lendrum"), gedcom_description = "The Skywalker family", gedcom_copyright = "None") |>
  add_indi(sex = "M", indi_notes = "The central character in the Star Wars Skywalker Saga") |>
  add_indi_names(name_pieces(given = "Anakin", surname = "Skywalker"), type = "birth") |>
  add_indi_names(name_pieces(prefix = "Darth", given = "Vader"), 
                 type = "given") |>
  add_indi(sex = "F", indi_notes = "Queen of Naboo") |>
  add_indi_names(name_pieces(given = "Padme", surname = "Amidala"), 
                 type = "birth") |> 
  add_indi(sex = "F") |> 
  add_indi_names(name_pieces(given = "Leia", surname = "Skywalker"), 
                 type = "birth") |>
  add_indi_names(name_pieces(prefix = "Princess", given = "Leia", surname = "Organa"), 
                 type = "adoptive") |> 
  add_indi(sex = "M") |>
  add_indi_names(name_pieces(given = "Luke", surname = "Skywalker"), 
                 type = "birth") |> 
  add_indi(sex = "M") |> 
  add_indi_names(name_pieces(given = "Obi-Wan", nickname = "Ben", surname = "Kenobi"), 
                 type = "birth")

anakin_xref <-find_indi_name(tg, "Anakin")
padme_xref <-find_indi_name(tg, "Padme")
luke_leia_xref <-find_indi_name_all(tg, "Luke|Leia")
obiwan_xref <-find_indi_name(tg, "Obi-Wan")

tg <- tg |>
  add_famg(husband = anakin_xref, 
           wife = padme_xref, 
           children = luke_leia_xref) |>
  activate_indi(anakin_xref) |> 
  add_indi_association(obiwan_xref, association = "Master") |> 
  add_indi_fact("death", age = "45y", cause = "Killed by son Luke",
                fact_place = place(name = "Second Death Star", notes = "Orbiting Endor System")) |> 
  add_indi_fact("religion", descriptor = "Jedi") |> 
  add_indi_fact("possession", descriptor = "Lightsaber") |>
  add_note("Based on Star Wars") |> 
  add_sour(short_title = "Star Wars", title = "Star Wars Episode IV: A New Hope") |> 
  add_repo("The Skywalker Saga") |> 
  add_media(file_reference = "XYZ", format = "JPEG")

print(tg, n = Inf)
#> # A tibble: 114 × 4
#>     level record tag   value                                                    
#>     <dbl> <chr>  <chr> <chr>                                                    
#>   1     0 HD     HEAD  ""                                                       
#>   2     1 HD     GEDC  ""                                                       
#>   3     2 HD     VERS  "5.5.5"                                                  
#>   4     2 HD     FORM  "LINEAGE-LINKED"                                         
#>   5     3 HD     VERS  "5.5.5"                                                  
#>   6     1 HD     CHAR  "UTF-8"                                                  
#>   7     1 HD     DEST  "gedcompendium"                                          
#>   8     1 HD     SOUR  "gedcompendium"                                          
#>   9     2 HD     NAME  "The 'gedcompendium' ecosystem of packages for the R lan…
#>  10     2 HD     CORP  "Jamie Lendrum"                                          
#>  11     3 HD     ADDR  ""                                                       
#>  12     3 HD     EMAIL "jalendrum@gmail.com"                                    
#>  13     3 HD     WWW   "https://jl5000.github.io/tidyged/"                      
#>  14     1 HD     DATE  "1 MAY 2022"                                             
#>  15     1 HD     LANG  "English"                                                
#>  16     1 HD     SUBM  "@U1@"                                                   
#>  17     1 HD     COPR  "None"                                                   
#>  18     1 HD     NOTE  "The Skywalker family"                                   
#>  19     0 @U1@   SUBM  ""                                                       
#>  20     1 @U1@   NAME  "Jamie Lendrum"                                          
#>  21     1 @U1@   CHAN  ""                                                       
#>  22     2 @U1@   DATE  "1 MAY 2022"                                             
#>  23     0 @I1@   INDI  ""                                                       
#>  24     1 @I1@   SEX   "M"                                                      
#>  25     1 @I1@   NOTE  "The central character in the Star Wars Skywalker Saga"  
#>  26     1 @I1@   NAME  "Anakin /Skywalker/"                                     
#>  27     2 @I1@   TYPE  "birth"                                                  
#>  28     2 @I1@   GIVN  "Anakin"                                                 
#>  29     2 @I1@   SURN  "Skywalker"                                              
#>  30     1 @I1@   NAME  "Darth Vader"                                            
#>  31     2 @I1@   TYPE  "given"                                                  
#>  32     2 @I1@   NPFX  "Darth"                                                  
#>  33     2 @I1@   GIVN  "Vader"                                                  
#>  34     1 @I1@   FAMS  "@F1@"                                                   
#>  35     1 @I1@   ASSO  "@I5@"                                                   
#>  36     2 @I1@   RELA  "Master"                                                 
#>  37     1 @I1@   DEAT  ""                                                       
#>  38     2 @I1@   PLAC  "Second Death Star"                                      
#>  39     3 @I1@   NOTE  "Orbiting Endor System"                                  
#>  40     2 @I1@   CAUS  "Killed by son Luke"                                     
#>  41     2 @I1@   AGE   "45y"                                                    
#>  42     1 @I1@   RELI  "Jedi"                                                   
#>  43     1 @I1@   PROP  "Lightsaber"                                             
#>  44     1 @I1@   CHAN  ""                                                       
#>  45     2 @I1@   DATE  "1 MAY 2022"                                             
#>  46     0 @I2@   INDI  ""                                                       
#>  47     1 @I2@   SEX   "F"                                                      
#>  48     1 @I2@   NOTE  "Queen of Naboo"                                         
#>  49     1 @I2@   NAME  "Padme /Amidala/"                                        
#>  50     2 @I2@   TYPE  "birth"                                                  
#>  51     2 @I2@   GIVN  "Padme"                                                  
#>  52     2 @I2@   SURN  "Amidala"                                                
#>  53     1 @I2@   FAMS  "@F1@"                                                   
#>  54     1 @I2@   CHAN  ""                                                       
#>  55     2 @I2@   DATE  "1 MAY 2022"                                             
#>  56     0 @I3@   INDI  ""                                                       
#>  57     1 @I3@   SEX   "F"                                                      
#>  58     1 @I3@   NAME  "Leia /Skywalker/"                                       
#>  59     2 @I3@   TYPE  "birth"                                                  
#>  60     2 @I3@   GIVN  "Leia"                                                   
#>  61     2 @I3@   SURN  "Skywalker"                                              
#>  62     1 @I3@   NAME  "Princess Leia /Organa/"                                 
#>  63     2 @I3@   TYPE  "adoptive"                                               
#>  64     2 @I3@   NPFX  "Princess"                                               
#>  65     2 @I3@   GIVN  "Leia"                                                   
#>  66     2 @I3@   SURN  "Organa"                                                 
#>  67     1 @I3@   FAMC  "@F1@"                                                   
#>  68     2 @I3@   PEDI  "birth"                                                  
#>  69     1 @I3@   CHAN  ""                                                       
#>  70     2 @I3@   DATE  "1 MAY 2022"                                             
#>  71     0 @I4@   INDI  ""                                                       
#>  72     1 @I4@   SEX   "M"                                                      
#>  73     1 @I4@   NAME  "Luke /Skywalker/"                                       
#>  74     2 @I4@   TYPE  "birth"                                                  
#>  75     2 @I4@   GIVN  "Luke"                                                   
#>  76     2 @I4@   SURN  "Skywalker"                                              
#>  77     1 @I4@   FAMC  "@F1@"                                                   
#>  78     2 @I4@   PEDI  "birth"                                                  
#>  79     1 @I4@   CHAN  ""                                                       
#>  80     2 @I4@   DATE  "1 MAY 2022"                                             
#>  81     0 @I5@   INDI  ""                                                       
#>  82     1 @I5@   SEX   "M"                                                      
#>  83     1 @I5@   NAME  "Obi-Wan /Kenobi/"                                       
#>  84     2 @I5@   TYPE  "birth"                                                  
#>  85     2 @I5@   GIVN  "Obi-Wan"                                                
#>  86     2 @I5@   NICK  "Ben"                                                    
#>  87     2 @I5@   SURN  "Kenobi"                                                 
#>  88     1 @I5@   CHAN  ""                                                       
#>  89     2 @I5@   DATE  "1 MAY 2022"                                             
#>  90     0 @F1@   FAM   ""                                                       
#>  91     1 @F1@   HUSB  "@I1@"                                                   
#>  92     1 @F1@   WIFE  "@I2@"                                                   
#>  93     1 @F1@   CHIL  "@I3@"                                                   
#>  94     1 @F1@   CHIL  "@I4@"                                                   
#>  95     1 @F1@   CHAN  ""                                                       
#>  96     2 @F1@   DATE  "1 MAY 2022"                                             
#>  97     0 @N1@   NOTE  "Based on Star Wars"                                     
#>  98     1 @N1@   CHAN  ""                                                       
#>  99     2 @N1@   DATE  "1 MAY 2022"                                             
#> 100     0 @S1@   SOUR  ""                                                       
#> 101     1 @S1@   TITL  "Star Wars Episode IV: A New Hope"                       
#> 102     1 @S1@   ABBR  "Star Wars"                                              
#> 103     1 @S1@   CHAN  ""                                                       
#> 104     2 @S1@   DATE  "1 MAY 2022"                                             
#> 105     0 @R1@   REPO  ""                                                       
#> 106     1 @R1@   NAME  "The Skywalker Saga"                                     
#> 107     1 @R1@   CHAN  ""                                                       
#> 108     2 @R1@   DATE  "1 MAY 2022"                                             
#> 109     0 @M1@   OBJE  ""                                                       
#> 110     1 @M1@   FILE  "XYZ"                                                    
#> 111     2 @M1@   FORM  "JPEG"                                                   
#> 112     1 @M1@   CHAN  ""                                                       
#> 113     2 @M1@   DATE  "1 MAY 2022"                                             
#> 114     0 TR     TRLR  ""
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
#>  Source system:           gedcompendium 
#>  Source system version:    
#>  Product name:            The 'gedcompendium' ecosystem of packages for the R language 
#>  Product source:          Jamie Lendrum

df_indi(tg) |> knitr::kable()
```

| xref | name             | sex | date_of_birth | place_of_birth | date_of_death | place_of_death    | mother        | father           | num_siblings | num_children | last_modified |
|:-----|:-----------------|:----|:--------------|:---------------|:--------------|:------------------|:--------------|:-----------------|:-------------|-------------:|:--------------|
| @I1@ | Anakin Skywalker | M   |               |                |               | Second Death Star |               |                  |              |            2 | 1 MAY 2022    |
| @I2@ | Padme Amidala    | F   |               |                |               |                   |               |                  |              |            2 | 1 MAY 2022    |
| @I3@ | Leia Skywalker   | F   |               |                |               |                   | Padme Amidala | Anakin Skywalker | 1            |            0 | 1 MAY 2022    |
| @I4@ | Luke Skywalker   | M   |               |                |               |                   | Padme Amidala | Anakin Skywalker | 1            |            0 | 1 MAY 2022    |
| @I5@ | Obi-Wan Kenobi   | M   |               |                |               |                   |               |                  |              |            0 | 1 MAY 2022    |

``` r
df_famg(tg) |> knitr::kable()
```

| xref | husband          | wife          | relationship_type | relationship_date | relationship_place | num_children | last_modified |
|:-----|:-----------------|:--------------|:------------------|:------------------|:-------------------|:-------------|:--------------|
| @F1@ | Anakin Skywalker | Padme Amidala |                   |                   |                    | 2            | 1 MAY 2022    |

This package allows limited editing of `tidyged` objects
(adding/removing records, as well as the addition of some record
substructures). Editing of existing records is made possible through
‘activation’ (much like the `tidygraph` package). When a record is
created, it automatically becomes the active record, through an object
attribute. Record editing functions then act on this record. Other
records can be activated through a series of `activate_*()` functions.

## References

-   [The GEDCOM 5.5.5 Specification](https://www.gedcom.org/gedcom.html)
