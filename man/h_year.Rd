% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h_year.R
\name{h_year}
\alias{h_year}
\title{Annual time series}
\usage{
h_year(file, mhy = 1, op = "M", dig = 1)
}
\arguments{
\item{file}{File name to proceed}

\item{mhy}{Starting month of the hydrological year (default = 1)}

\item{op}{Sum (S) or Mean (M) (default = "M")}

\item{dig}{Number of significant digits for Value (default = 1)}
}
\value{
The function returns a time-series of annual values.
}
\description{
Annual time series
}
\details{
The function computes an annual time-series using the annual mean or the
annual sum of daily values. It allows the use of hydrological years. The date corresponds
to the middle of the year, i.e. the 182th day.
}
\author{
P. Chevallier - Nov 2022
}
