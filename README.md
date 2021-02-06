
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyged <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/jl5000/tidyged/workflows/R-CMD-check/badge.svg)](https://github.com/jl5000/tidyged/actions)
[![](https://codecov.io/gh/jl5000/tidyged/branch/master/graph/badge.svg)](https://codecov.io/gh/jl5000/tidyged)
[![CodeFactor](https://www.codefactor.io/repository/github/jl5000/tidyged/badge)](https://www.codefactor.io/repository/github/jl5000/tidyged)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

> Families can be difficult - but the data describing them doesn’t need
> to be

Create and summarise family tree GEDCOM files using tidy dataframes.
This package is still in heavy development with the first operational
version likely around March 2021.

This package is the main package of the
[`gedcompendium`](https://github.com/jl5000/gedcompendium) ecosystem.

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

print(tg, n = Inf)
#> [90m# A tibble: 113 x 4[39m
#>     level record tag   value                                                  
#>     [3m[90m<dbl>[39m[23m [3m[90m<chr>[39m[23m  [3m[90m<chr>[39m[23m [3m[90m<chr>[39m[23m                                                  
#> [90m  1[39m     0 HD     HEAD  [90m"[39m[90m"[39m                                                     
#> [90m  2[39m     1 HD     GEDC  [90m"[39m[90m"[39m                                                     
#> [90m  3[39m     2 HD     VERS  [90m"[39m5.5.5[90m"[39m                                                
#> [90m  4[39m     2 HD     FORM  [90m"[39mLINEAGE-LINKED[90m"[39m                                       
#> [90m  5[39m     3 HD     VERS  [90m"[39m5.5.5[90m"[39m                                                
#> [90m  6[39m     1 HD     CHAR  [90m"[39mUTF-8[90m"[39m                                                
#> [90m  7[39m     1 HD     DEST  [90m"[39mtidyged[90m"[39m                                              
#> [90m  8[39m     1 HD     SOUR  [90m"[39mtidyged[90m"[39m                                              
#> [90m  9[39m     2 HD     VERS  [90m"[39m0.0.0[90m"[39m                                                
#> [90m 10[39m     2 HD     NAME  [90m"[39mThe 'tidyged' package for the R language[90m"[39m             
#> [90m 11[39m     2 HD     CORP  [90m"[39mJamie Lendrum[90m"[39m                                        
#> [90m 12[39m     3 HD     ADDR  [90m"[39m[90m"[39m                                                     
#> [90m 13[39m     3 HD     EMAIL [90m"[39mjalendrum@gmail.com[90m"[39m                                  
#> [90m 14[39m     3 HD     WWW   [90m"[39mhttps://jl5000.github.io/tidyged/[90m"[39m                    
#> [90m 15[39m     1 HD     DATE  [90m"[39m6 FEB 2021[90m"[39m                                           
#> [90m 16[39m     1 HD     LANG  [90m"[39mEnglish[90m"[39m                                              
#> [90m 17[39m     1 HD     SUBM  [90m"[39m@U1@[90m"[39m                                                 
#> [90m 18[39m     1 HD     COPR  [90m"[39mNone[90m"[39m                                                 
#> [90m 19[39m     1 HD     NOTE  [90m"[39mThe Skywalker family[90m"[39m                                 
#> [90m 20[39m     0 @U1@   SUBM  [90m"[39m[90m"[39m                                                     
#> [90m 21[39m     1 @U1@   NAME  [90m"[39mJamie Lendrum[90m"[39m                                        
#> [90m 22[39m     1 @U1@   CHAN  [90m"[39m[90m"[39m                                                     
#> [90m 23[39m     2 @U1@   DATE  [90m"[39m6 FEB 2021[90m"[39m                                           
#> [90m 24[39m     0 @I1@   INDI  [90m"[39m[90m"[39m                                                     
#> [90m 25[39m     1 @I1@   SEX   [90m"[39mM[90m"[39m                                                    
#> [90m 26[39m     1 @I1@   NOTE  [90m"[39mThe central character in the Star Wars Skywalker Saga[90m"[39m
#> [90m 27[39m     1 @I1@   NAME  [90m"[39mAnakin Skywalker[90m"[39m                                     
#> [90m 28[39m     2 @I1@   TYPE  [90m"[39mbirth[90m"[39m                                                
#> [90m 29[39m     2 @I1@   GIVN  [90m"[39mAnakin[90m"[39m                                               
#> [90m 30[39m     2 @I1@   SURN  [90m"[39mSkywalker[90m"[39m                                            
#> [90m 31[39m     1 @I1@   NAME  [90m"[39mDarth Vader[90m"[39m                                          
#> [90m 32[39m     2 @I1@   TYPE  [90m"[39mgiven[90m"[39m                                                
#> [90m 33[39m     2 @I1@   GIVN  [90m"[39mDarth Vader[90m"[39m                                          
#> [90m 34[39m     1 @I1@   FAMS  [90m"[39m@F1@[90m"[39m                                                 
#> [90m 35[39m     1 @I1@   ASSO  [90m"[39m@I5@[90m"[39m                                                 
#> [90m 36[39m     2 @I1@   RELA  [90m"[39mMaster[90m"[39m                                               
#> [90m 37[39m     1 @I1@   DEAT  [90m"[39mY[90m"[39m                                                    
#> [90m 38[39m     2 @I1@   PLAC  [90m"[39mSecond Death Star[90m"[39m                                    
#> [90m 39[39m     3 @I1@   NOTE  [90m"[39mOrbiting Endor System[90m"[39m                                
#> [90m 40[39m     2 @I1@   CAUS  [90m"[39mKilled by son Luke[90m"[39m                                   
#> [90m 41[39m     2 @I1@   AGE   [90m"[39m45y[90m"[39m                                                  
#> [90m 42[39m     1 @I1@   RELI  [90m"[39mJedi[90m"[39m                                                 
#> [90m 43[39m     1 @I1@   PROP  [90m"[39mLightsaber[90m"[39m                                           
#> [90m 44[39m     1 @I1@   CHAN  [90m"[39m[90m"[39m                                                     
#> [90m 45[39m     2 @I1@   DATE  [90m"[39m6 FEB 2021[90m"[39m                                           
#> [90m 46[39m     0 @I2@   INDI  [90m"[39m[90m"[39m                                                     
#> [90m 47[39m     1 @I2@   SEX   [90m"[39mF[90m"[39m                                                    
#> [90m 48[39m     1 @I2@   NOTE  [90m"[39mQueen of Naboo[90m"[39m                                       
#> [90m 49[39m     1 @I2@   NAME  [90m"[39mPadme Amidala[90m"[39m                                        
#> [90m 50[39m     2 @I2@   GIVN  [90m"[39mPadme[90m"[39m                                                
#> [90m 51[39m     2 @I2@   SURN  [90m"[39mAmidala[90m"[39m                                              
#> [90m 52[39m     1 @I2@   FAMS  [90m"[39m@F1@[90m"[39m                                                 
#> [90m 53[39m     1 @I2@   CHAN  [90m"[39m[90m"[39m                                                     
#> [90m 54[39m     2 @I2@   DATE  [90m"[39m6 FEB 2021[90m"[39m                                           
#> [90m 55[39m     0 @I3@   INDI  [90m"[39m[90m"[39m                                                     
#> [90m 56[39m     1 @I3@   SEX   [90m"[39mF[90m"[39m                                                    
#> [90m 57[39m     1 @I3@   NAME  [90m"[39mLeia Skywalker[90m"[39m                                       
#> [90m 58[39m     2 @I3@   TYPE  [90m"[39mbirth[90m"[39m                                                
#> [90m 59[39m     2 @I3@   GIVN  [90m"[39mLeia[90m"[39m                                                 
#> [90m 60[39m     2 @I3@   SURN  [90m"[39mSkywalker[90m"[39m                                            
#> [90m 61[39m     1 @I3@   NAME  [90m"[39mPrincess Leia Organa[90m"[39m                                 
#> [90m 62[39m     2 @I3@   TYPE  [90m"[39madoptive[90m"[39m                                             
#> [90m 63[39m     2 @I3@   NPFX  [90m"[39mPrincess[90m"[39m                                             
#> [90m 64[39m     2 @I3@   GIVN  [90m"[39mLeia[90m"[39m                                                 
#> [90m 65[39m     2 @I3@   SURN  [90m"[39mOrgana[90m"[39m                                               
#> [90m 66[39m     1 @I3@   FAMC  [90m"[39m@F1@[90m"[39m                                                 
#> [90m 67[39m     2 @I3@   PEDI  [90m"[39mbirth[90m"[39m                                                
#> [90m 68[39m     1 @I3@   CHAN  [90m"[39m[90m"[39m                                                     
#> [90m 69[39m     2 @I3@   DATE  [90m"[39m6 FEB 2021[90m"[39m                                           
#> [90m 70[39m     0 @I4@   INDI  [90m"[39m[90m"[39m                                                     
#> [90m 71[39m     1 @I4@   SEX   [90m"[39mM[90m"[39m                                                    
#> [90m 72[39m     1 @I4@   NAME  [90m"[39mLuke Skywalker[90m"[39m                                       
#> [90m 73[39m     2 @I4@   TYPE  [90m"[39mbirth[90m"[39m                                                
#> [90m 74[39m     2 @I4@   GIVN  [90m"[39mLuke[90m"[39m                                                 
#> [90m 75[39m     2 @I4@   SURN  [90m"[39mSkywalker[90m"[39m                                            
#> [90m 76[39m     1 @I4@   FAMC  [90m"[39m@F1@[90m"[39m                                                 
#> [90m 77[39m     2 @I4@   PEDI  [90m"[39mbirth[90m"[39m                                                
#> [90m 78[39m     1 @I4@   CHAN  [90m"[39m[90m"[39m                                                     
#> [90m 79[39m     2 @I4@   DATE  [90m"[39m6 FEB 2021[90m"[39m                                           
#> [90m 80[39m     0 @I5@   INDI  [90m"[39m[90m"[39m                                                     
#> [90m 81[39m     1 @I5@   SEX   [90m"[39mM[90m"[39m                                                    
#> [90m 82[39m     1 @I5@   NAME  [90m"[39mObi-Wan Kenobi[90m"[39m                                       
#> [90m 83[39m     2 @I5@   TYPE  [90m"[39mbirth[90m"[39m                                                
#> [90m 84[39m     2 @I5@   GIVN  [90m"[39mObi-Wan[90m"[39m                                              
#> [90m 85[39m     2 @I5@   NICK  [90m"[39mBen[90m"[39m                                                  
#> [90m 86[39m     2 @I5@   SURN  [90m"[39mKenobi[90m"[39m                                               
#> [90m 87[39m     1 @I5@   CHAN  [90m"[39m[90m"[39m                                                     
#> [90m 88[39m     2 @I5@   DATE  [90m"[39m6 FEB 2021[90m"[39m                                           
#> [90m 89[39m     0 @F1@   FAM   [90m"[39m[90m"[39m                                                     
#> [90m 90[39m     1 @F1@   HUSB  [90m"[39m@I1@[90m"[39m                                                 
#> [90m 91[39m     1 @F1@   WIFE  [90m"[39m@I2@[90m"[39m                                                 
#> [90m 92[39m     1 @F1@   CHIL  [90m"[39m@I4@[90m"[39m                                                 
#> [90m 93[39m     1 @F1@   CHIL  [90m"[39m@I3@[90m"[39m                                                 
#> [90m 94[39m     1 @F1@   CHAN  [90m"[39m[90m"[39m                                                     
#> [90m 95[39m     2 @F1@   DATE  [90m"[39m6 FEB 2021[90m"[39m                                           
#> [90m 96[39m     0 @N1@   NOTE  [90m"[39mBased on Star Wars[90m"[39m                                   
#> [90m 97[39m     1 @N1@   CHAN  [90m"[39m[90m"[39m                                                     
#> [90m 98[39m     2 @N1@   DATE  [90m"[39m6 FEB 2021[90m"[39m                                           
#> [90m 99[39m     0 @S1@   SOUR  [90m"[39m[90m"[39m                                                     
#> [90m100[39m     1 @S1@   TITL  [90m"[39mStar Wars Episode IV: A New Hope[90m"[39m                     
#> [90m101[39m     1 @S1@   ABBR  [90m"[39mStar Wars[90m"[39m                                            
#> [90m102[39m     1 @S1@   CHAN  [90m"[39m[90m"[39m                                                     
#> [90m103[39m     2 @S1@   DATE  [90m"[39m6 FEB 2021[90m"[39m                                           
#> [90m104[39m     0 @R1@   REPO  [90m"[39m[90m"[39m                                                     
#> [90m105[39m     1 @R1@   NAME  [90m"[39mThe Skywalker Saga[90m"[39m                                   
#> [90m106[39m     1 @R1@   CHAN  [90m"[39m[90m"[39m                                                     
#> [90m107[39m     2 @R1@   DATE  [90m"[39m6 FEB 2021[90m"[39m                                           
#> [90m108[39m     0 @O1@   OBJE  [90m"[39m[90m"[39m                                                     
#> [90m109[39m     1 @O1@   FILE  [90m"[39mXYZ[90m"[39m                                                  
#> [90m110[39m     2 @O1@   FORM  [90m"[39mJPG[90m"[39m                                                  
#> [90m111[39m     1 @O1@   CHAN  [90m"[39m[90m"[39m                                                     
#> [90m112[39m     2 @O1@   DATE  [90m"[39m6 FEB 2021[90m"[39m                                           
#> [90m113[39m     0 TR     TRLR  [90m"[39m[90m"[39m
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
#>  Source system:           tidyged 
#>  Source system version:   0.0.0 
#>  Product name:            The 'tidyged' package for the R language 
#>  Product source:          Jamie Lendrum

df_individuals(tg) %>% knitr::kable()
```

| xref | name             | sex | date\_of\_birth | place\_of\_birth | date\_of\_death | place\_of\_death  | mother        | father           | num\_siblings | num\_children | last\_modified |
| :--- | :--------------- | :-- | :-------------- | :--------------- | :-------------- | :---------------- | :------------ | :--------------- | :------------ | ------------: | :------------- |
| @I1@ | Anakin Skywalker | M   |                 |                  |                 | Second Death Star |               |                  |               |             2 | 6 FEB 2021     |
| @I2@ | Padme Amidala    | F   |                 |                  |                 |                   |               |                  |               |             2 | 6 FEB 2021     |
| @I3@ | Leia Skywalker   | F   |                 |                  |                 |                   | Padme Amidala | Anakin Skywalker | 1             |             0 | 6 FEB 2021     |
| @I4@ | Luke Skywalker   | M   |                 |                  |                 |                   | Padme Amidala | Anakin Skywalker | 1             |             0 | 6 FEB 2021     |
| @I5@ | Obi-Wan Kenobi   | M   |                 |                  |                 |                   |               |                  |               |             0 | 6 FEB 2021     |

``` r
df_families(tg) %>% knitr::kable()
```

| xref | husband          | wife          | marriage\_date | marriage\_place | num\_children | last\_modified |
| :--- | :--------------- | :------------ | :------------- | :-------------- | :------------ | :------------- |
| @F1@ | Anakin Skywalker | Padme Amidala |                |                 | 2             | 6 FEB 2021     |

This package allows limited editing of `tidyged` objects
(adding/removing records, as well as the addition of some record
substructures). Editing of existing records is made possible through
‘activation’ (much like the `tidygraph` package). When a record is
created, it automatically becomes the active record, through an object
attribute. Record editing functions then act on this record. Other
records can be activated through a series of `activate_*_record()`
functions.

## References

  - [The GEDCOM 5.5.5 Specification](https://www.gedcom.org/gedcom.html)
