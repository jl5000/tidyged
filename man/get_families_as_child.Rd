% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify.R
\name{get_families_as_child}
\alias{get_families_as_child}
\title{Identify all families for an individual where they are a child}
\usage{
get_families_as_child(gedcom, indi_xref = character(), birth_only = FALSE)
}
\arguments{
\item{gedcom}{A tidyged object.}

\item{indi_xref}{The xref of an Individual record to act on if one
is not activated (will override active record).}

\item{birth_only}{Whether to only return the family containing the biological parents.}
}
\value{
A character vector of family xrefs.
}
\description{
Identify all families for an individual where they are a child
}
\examples{
get_families_as_child(sample555, "@I3@")
}
