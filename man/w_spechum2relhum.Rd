% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/w_spec2rel_hum.R
\name{w_spechum2relhum}
\alias{w_spechum2relhum}
\title{Convert specific humidity to relative humidity}
\source{
David LeBauer - 2014

from Bolton 1980 The computation of Equivalent Potential Temperature

\url{https://earthscience.stackexchange.com/questions/2360/how-do-i-convert-specific-humidity-to-relative-humidity}
}
\usage{
w_spechum2relhum(f_spechum, f_temp, f_atm)
}
\arguments{
\item{f_spechum}{file of specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass}

\item{f_temp}{file of temperature degrees C}

\item{f_atm}{file of atmospheric pressure in mb}
}
\value{
a file of relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
}
\description{
Convert specific humidity to relative humidity
}
\details{
Converting specific humidity into relative humidity.
from Bolton 1980 The computation of Equivalent Potential Temperature
}
\author{
P. Chevallier - Nov 2022
}
