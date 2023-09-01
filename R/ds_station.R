#' @title Shiny app: create, modify or remove a station from a data base
#'
#' @author P. Chevallier - Apr 2020 - Aug 2023
#'
#' @description Shiny application of the \code{\link{d_station}} function
#'
#' @return a shiny session
#'
ds_station <- function() {
  runApp(system.file("extdata/app_station", package="htsr"))
}
