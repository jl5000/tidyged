# tidygedcom

Import, create, summarise, and edit family tree GEDCOM files using tidy dataframes. This package is still in heavy development with the first operational version unlikely to be complete before October 2020.

The intent is to allow the user to import GEDCOM files or create them from scratch using a `ggplot2` type interface; starting with a base GEDCOM object (with only a header and trailer section) and adding records as you would add layers to a ggplot object (but instead using the pipe operator, %>%).

For example:

``` r
gedcom(subm(name = "Jamie Lendrum")) %>% 
  add_individual(<params>) %>% 
  add_individual(<params>) %>% 
  add_family(<params>) %>% 
  add_source(<params>)
```

Just like a ggplot object requires aesthetics, a GEDCOM file requires details of a submitter. If no submitter details are given, the username is used.

Within the package, GEDCOM files are represented as tidy tibbles (a sub-class of tibble known as `tidygedcom` objects) allowing easy manipulation and exploitation of existing `tidyverse` infrastructure.

Functionality will also exist to allow the removal and modification of existing records, as well as summaries. 

Editing of existing records is made possible through 'activation' (much like the `tidygraph` package). When a record is created, it automatically becomes the active record, through an object attribute. Record editing functions then act on this record. Other records can be activated through a series of `activate_*_record()` functions.

A sister package named `tgvis` will allow visualisation of `tidygedcom` objects.

# Current status

* The GEDCOM 5.5.1 specification has been implemented within the params.R, structures.R, records.R, and validate_params.R files. This is the bulk of the internal data structures required;
* Current work is on the creation, interrogation, and editing of tidygedcom objects;
* It is still undecided how extensive any validation checks should be, given the number of GEDCOM validators available.
* Work on `tgvis` will begin once an initial version of `tidygedcom` is complete.

# References

* [The GEDCOM Standard - Release 5.5.1](https://edge.fscdn.org/assets/img/documents/ged551-5bac5e57fe88dd37df0e153d9c515335.pdf)
