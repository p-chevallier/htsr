% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_csv_multivar.R
\name{f_csv_multivar}
\alias{f_csv_multivar}
\title{Build a multivariable table file in csv format}
\usage{
f_csv_multivar(files, daily = TRUE, fileo = "fileo")
}
\arguments{
\item{files}{list of hts files}

\item{daily}{default = TRUE}

\item{fileo}{name of the output file (without extension)}
}
\value{
A csv table, where the first field is a date and the next fields values
}
\description{
Build a multivariable table file in csv format
}
\details{
The function build a cvs file with values extracted from several hts files at the same date. 
So, it's better to run `h_common` before to apply `f_csv_multivar`
 
If daily is TRUE, only the date is taking into account, not the time.
}
\author{
P. Chevallier - Jan-Feb 2022
}
