% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{get_valid_xref}
\alias{get_valid_xref}
\title{Derive a valid cross-reference identifier}
\usage{
get_valid_xref(gedcom, xref_or_descriptor, record_type, record_type_fn)
}
\arguments{
\item{gedcom}{A tidygedcom object.}

\item{xref_or_descriptor}{An xref or descriptor uniquely identifying the record.}

\item{record_type}{A character string describing the record type. Generally one of
the global record_string_* values.}

\item{record_type_fn}{A function to check the record type. Generally one of the is_*
functions.}
}
\value{
A valid xref identifier.
}
\description{
Get a valid xref provided explicitly or implicitly (through an identifying attribute or
active record).
}
\details{
This helper function is designed to derive and run validation checks on an xref
provided explicitly or implicitly. An xref is provided implicitly either through the active
record of the tidygedcom object, or through a descriptor identifying a unique record.

The descriptors used for each record are: name (individual, repository, and submitter),
title (source), file reference (multimedia), excerpt (note).

Once found, the xref is checked to ensure it is of the appropriate type.
}