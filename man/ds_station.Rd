% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds_station.R
\name{ds_station}
\alias{ds_station}
\title{Create, Modify or Remove a station}
\usage{
ds_station()
}
\value{
Station created, modified ou removed from the data base
}
\description{
Create, Modify or Remove a station.
}
\details{
If operation is Create, the fields Id_Station, Type_Station and
Name (name_st) are compulsory. The field Name can be modified afterwards.

If op is Create or Modify the following fields can be completed optionnaly: Country,
Zone, Sub-zone, Large basin, Basin, Small basin, River, Longitude, Latitude,
Altitude, Basin area, Manager.

If op is Modify, station type and station id cannot be modified. The sensors and
data corresponding to the station are conserved.

If op is Remove, all data and sensors of the station are removed.
}
\author{
P. Chevallier - Jan 2018 - Sep 2023
}
