#' @title Shiny app: create, modify or remove a sensor from a data base
#'
#' @author P. Chevallier - Nov 2020 - Aug 2023
#'
#' @description Shiny application of the \code{\link{d_sensor}} function
#'
#' @return a shiny session
#'
ds_sensor <- function() {
  runApp(system.file("extdata/app_sensor", package="htsr"))
}
