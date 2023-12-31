% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h_season.R
\name{h_season}
\alias{h_season}
\title{Seasonal selection}
\usage{
h_season(file, monthstart)
}
\arguments{
\item{file}{Full file name to proceed}

\item{monthstart}{List of 2 to 4 integers (between 1 and 12) giving the starting
month of each season.}
}
\value{
list of file names for each seasonal time-series.
}
\description{
The function provides seasonal time-series.
}
\details{
2 to 4 seasons can be selected. For each season, the prefix sx_ where x is the season
is added to the file name.
}
\examples{
\dontrun{

files <- h_season("foo.hts", monthstart=c(3,6,9,12))
}

}
\author{
P. Chevallier - Oct 2017 - Mar 2020
}
