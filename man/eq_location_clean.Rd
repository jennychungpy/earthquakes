% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_location_clean.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{Cleaning country names}
\usage{
eq_location_clean(data, colName)
}
\arguments{
\item{data}{- A delimited file that is read in by the 'read_delim' function}

\item{colName}{- The 'colName' parameter is a character and represents the column with the country names}
}
\value{
A data frame in the global environment
}
\description{
The 'eq_location_clean(data, countryCol)' function will change the given column into title case.
}
\details{
The column with the country names of the given data file will be changed into title case.
}
\note{
error - An error is given when colName is not given.

error - An error is given when colName is not a character
}
\examples{
\dontrun{
file = "data/earthquakes.tsv.gz"
file <- read_delim(file, delim = "\\t")
eq_location_clean(file, COUNTRY)
}

}
