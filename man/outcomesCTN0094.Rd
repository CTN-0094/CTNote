% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_outcomesCTN0094.R
\docType{data}
\name{outcomesCTN0094}
\alias{outcomesCTN0094}
\title{All Treatment Outcomes for CTN-0094 Participants}
\format{
A tibble with 3,560 rows and
64 columns. These columns include
\describe{
\item{who}{Patient ID}
\item{usePatternUDS}{A character string containing the "use pattern
word", which represents the weekly opioid use for each participant
after their day of randomization. For more information, see
\code{link[ctn0094DataExtra]{derived_weeklyOpioidPattern}}}
\item{...}{The calculated treatment outcomes for 53 endpoints. Some
endpoints are composites of "time to event" and an "event" indicator,
but these are included as two separate columns and are named \verb{*_time}
and \verb{*_event}, respectively.}
}
}
\usage{
data(outcomesCTN0094)
}
\description{
This data set is a table of 53 treatment outcomes calculated on
3560 participants from three clinical trials to assess the efficacy of
medication-assisted treatment for opioid use disorder.
}
\details{
These outcomes are based on a harmonized set of data from three
clinical trials. The harmonized data from these trials are contained in
the packages \code{ctn0094data} and \code{ctn0094DataExtra}. These outcomes are
calculated in the three abstinence, relapse, and reduction "library"
vignettes of this package. The data dictionary is currently stored as an
Excel spreadsheet in \code{inst/suppl_docs/definitions_20221123.xlsx}.
}
\keyword{datasets}
