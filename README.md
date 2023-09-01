  <!-- badges: start -->
  [![R-CMD-check](https://github.com/p-chevallier/htsr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/p-chevallier/htsr/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# htsr

The goal of 'htsr' is to manage and handle hydrological and meteorological time-series stored
in a 'sqlite' data base.

## Installation

You can install the released version of htsr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("htsr")
```

## User manual

A pdf file summarizing all the functions can be downloaded from [CRAN Packages repository](https://cran.r-project.org/web/packages/htsr/index.html)

## Cautionary note

All functions can be executed on Linux, Windows or MacOS platforms, except `d_convert_hydraccess`  only
usable on Windows (not tested on MacOS), because it needs a Microsoft Access data base and the associated OBDC library.
