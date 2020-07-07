# tidygedcom

Import, check, manipulate, visualise, and edit family tree GEDCOM files using tidy dataframes. This package is still in heavy development with the first operational version unlikely to be complete before October 2020.

The intent is to allow the user to import GEDCOM files or create them from scratch using a `ggplot2` type interface; starting with a base GEDCOM object (with only a header and trailer section) and adding records as you would add layers to a ggplot object (but instead using the pipe operator, %>%).

For example:

``` r
gedcom(subm(name = "Jamie Lendrum")) %>% 
  add_individual(<params>) %>% 
  add_individual(<params>) %>% 
  add_family(<params>) %>% 
  add_source(<params>)
```

Within the package, GEDCOM files are represented as tidy tibbles (a sub-class of tibble known as tidygedcom objects) allowing easy manipulation and exploitation of existing `tidyverse` infrastructure.

Functionality will also exist to allow the removal and modification of existing records, as well as interrogation and visualisation of tidygedcom objects.

# Current status

* The GEDCOM 5.5.1 specification has been implemented within the params.R, structures.R, and records.R files. This is the bulk of the internal data structures required;
* Current work is on the creation and interrogation of tidygedcom objects;
* Work on editing and visualisation of tidygedcom objects has yet to begin;
* It is still undecided how extensive any validation checks should be, given the number of GEDCOM validators available.