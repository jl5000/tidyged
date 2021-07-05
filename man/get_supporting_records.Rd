% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify.R
\name{get_supporting_records}
\alias{get_supporting_records}
\title{Identify all supporting records for a set of records}
\usage{
get_supporting_records(
  gedcom,
  xrefs,
  include_note = TRUE,
  include_media = TRUE,
  include_sour = TRUE,
  include_repo = TRUE
)
}
\arguments{
\item{gedcom}{A tidyged object.}

\item{xrefs}{The xrefs of records to get supporting records for.}

\item{include_note}{Whether to include Note records.}

\item{include_media}{Whether to include Multimedia records.}

\item{include_sour}{Whether to include Source records.}

\item{include_repo}{Whether to include Repository records.}
}
\value{
A character vector of supporting record xrefs.
}
\description{
This function gets all supporting records (and onwards dependencies) for a set of records. Supporting records
are note, multimedia, source, and repository records, i.e. those providing supporting evidence and comments.
}