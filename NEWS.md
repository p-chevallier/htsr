## version 2.1.2 (development version)

### general
* The subfunctions `d_exp_hts`, `d_inventory`, `d_station`,`d_sensor`, `h_tstep`, `h_month` are included in their shiny equivalent and not anymore displayed as autonomous.
* The subfunctions `p_line`, `p_bar` and `z_set` are included in the calling functions and not anymore displayed as autonomous.

### new function
* 

### changes
* `ds_inventory`: The inventory can be stored in xlsx ou csv files.
* `hs_tstep`: offers new options, which was previously included in `h_month`.
* Changes were made in `ds_exp_hts`, `ps_plothts`, `d_wind` and `h_wl_di` in order to include the subfunctions `p_line`, `p_bar` and `z_set`. 

### bug fixed
* 

### removed functions
* `h_etp`, which duplicated `w_etp`

### bug suspected
* `f_substitute`: It seems that in few cases it doesn't work correctly. But the error trigger isn't clearly identified. If somebody encounters this issue, please document it and contact the author.

-------
## version 2.1.1 (19 September 2023)

### general
* A Git repo has been initialized on github.com

* Reorganization of the shinyapps in the main R folder

* Dependency issues resolved and simplified

### changes
* Accompanying the fixed bug in `ps_plothts`, slight changes have been made also in the codes of `p_line`, `p_bar` and `z_set`.
* The function `d_convert_weewx` is modified, allowing to update the htsr data base with the most recent records of the weewx data base.
* Changes in `hs_tstep` and in `h_month` allowing to compute min and max values.

### bug fixed
* `ps_plot`: several bugs fixed.
* `ds_station`: correction of the inversion between latitude and longitude.

### bug suspected
* `f_substitute`: It seems that in few cases it doesn't work correctly. But the error trigger isn't clearly identified. If somebody encounters this issue, please document it and contact the author.

-------

## version 2.1.0 (29 August 2023)

### general
* Several shiny functions have been revisited; see details below.
* New dependencies are needed: `fs`, `shinyFiles` and `waiter`.
* The dependency `zoo` is removed.
* As requested by CRAN, a change in the configuration corrects the `htsr-package` file.

### new function
* none

### changes
* The basic plotting functions have been re-coded: `p_line`, `p_bar`, `z_set` and `ps_plothts`.
* `ds_inventory`, `ds_exp_hts`, `ds_station`, `ds_sensor` and `hs_tstep` were redesigned. The arguments are now passed within the shiny display.
* `d_exp_hts`: the function has been slightly re-coded, in order to not use anymore the dependency `zoo`.

### removed functions
* `p_line_app`and `p_bar_app`, useless after the re-coding of the plotting functions, are removed.
* `ds_dismeas`: this function, which was still experimental has been removed from the package. It could reappear in the future.


### bug fixed
* none

### bug suspected
* `f_substitute`: It seems that in few cases it doesn't work correctly. But the error trigger isn't clearly identified. If somebody encounters this issue, please document it and contact the author.

-------

## version 2.0.1 (29 June 2023)

### general
* Imports in DESCRIPTION file have been simplified. 
* The deprecated dependency `raster` has been replaced by the dependency `terra`.
* Both above points led to slight changes in the NAMESPACE file.
* The shiny apps are not anymore shown in the browser, but in a temporary window (the browser can be displayed starting from it).
* The default color palette is changed, `ggplot2` replacing `Dark2`.

### new function
* `h_changetz`: allows to change the timezone of a time series.

### changes
* `p_hypso`: The called functions of the deprecated dependency `raster` have been replaced by these of the dependency `terra`.
* `d_convert_weewx`: slight changes.
* `p_line` & `p_bar`: the plot frame has been slightly modified.

### bug fixed
* none

### bug suspected
* `f_substitute`: It seems that in few cases it doesn't work correctly. But the trigger isn't clearly identified. If somebody encounters this issue, please document it and contact the author.

-------

## version 2.0.0 (5 June 2023)

### general
This new version of the package contains rewritten functions in C++ in order to accelerate them. It needs now a compilation using the Rcpp package, which must be loaded, if not done before.

### new functions
* none

### changes
* `h_common`: the double loop within the function is rewritten in C++. It considerably accelerates the process. This function is also called by `d_wind`, `f_csv_multivar`, `h_condition`, `h_weightedsum`, `p_scatter`, `w_atmp_alt`
* `h_timestep`: the function is considerably accelerated, clling a sub-function written in C++. It is also called by `hs_tstep`.
* `d_convert_weewx`: slight changes in the documentation notice
* `d_wind`: slight changes in the function

### bug fixed
* `p_scatter`: Computing issue solved.

### bug suspected
* `f_substitute`: It seems that in few cases it doesn't work correctly. But the trigger isn't clearly identified. If somebody encounters this issue, please document it and contact the author.


------

## version 1.2.0 (22 dec 2022)

### new functions
* `f_month2day`: interpolation of daily records from a monthly time series 
* `h_avday` computes a one year time series filled with the mean values of each calendar day over an interval longer than 4 years.
* `h_year` extracts an annual time series from a daily time series.
* `w_spc2rel_hum` converts specific humidity to relative humidity.

### changes
* `f_properties`: display improved.
* `h_etp`: which was duplicated with the name `w_etp`, is removed, keeping only the second one.

### bugs fixed
* `d_rem_hts`: issue fixed.
* `d_sensor` and `d_station`: severe issue fixed with the "remove" option.
* `w_atmp_alt`: a computation error has been fixed. 
* `w_etp`: a bug has been fixed in the `u_ra` subfunction used for the methods "Penman-Monteith", "Priestley-Taylor", "Makkink" and "Heargraves-Samani".

### bug suspected
* `f_substitute`: It seems that in few cases it doesn't work correctly. But the trigger isn't clearly identified. If somebody encounters this issue, please document it and contact the author.


------

## version 1.1.5 (6 nov 2022)

### bugs to fix
* It seems that in few cases `f_substitute` doesn't work correctly. But the trigger isn't clearly identified. If somebody encounters this issue, please document it and contact the author.

### bugs fixed
* Bad column names fixed in `f_convert` when converting from xls/xlsx file to hts file

### addings
* `h_addna` adds NA values within a time series.

------

## version 1.1.4 (17 apr 2022)

### changes
* In `h_timestep` and `hs_tstep` it is now possible to shift the interval of computing in the case of a daily timestep.
* `f_convert_hdsm2hts` and `f_convert_hts2hdsm`, which regard solely the hydrological modelling with HDSM, are removed. If somebody is interested to use these functions, please contact the author.
* Small changes in the function `h_statbasic`, which allow to process several files and not only one.

### bugs to fix
* It seems that in few cases `f_substitute` doesn't work correctly. But the trigger isn't clearly identified. If somebody encounters this issue, please document it and contact the author.

### bugs fixed
* Call to `d_station` in `d_convert_weewx`
* Correction in the documentation of `hs_tstep`.

### addings
* `f_csv_multivar` build a csv table with several variables.

------

## version 1.1.3 (30 dec 2021)

### changes
* An URL is added in the DESCRIPTION file
* A new category of function is created, regarding specific operations for weather data. All functions of this category begin with w_. As a consequence `h_etp`is renamed `w_etp`.
* `h_gappfill`, is now providing  linear interpolated values within the gap interval.
* `f_convert` does not include anymore the conversion to HDSM format. See below in the "addings", new interface functions with the model HDSM.

### bugs to fix
* It seems that in some cases the function `f_substitute` doesn't work correctly.

### addings
* The function `w_temp_alt` computes the air temperature, function of altitude.
* The function `w_atmp_alt` computes the atmospheric pressure, function of altitude.
* The function `f_convert_hdsm2hts` converts output files from the model HDSM in to hts data-series files.
* The function `f_convert_hdsm2hts` converts input hts data-series files into a calibration file that can be used by the model HDSM.

### documentation
* Date editions were corrected in few function notices.

------

## version 1.1.2 (31 oct 2021)

### changes
* homogenization of parameter names: `fsq` for sqlite data base file, `file` for common file
* `hs_tstep`: change in option display 
* the previous function `d_exp_discalib` is now included within `p_discalib` and renamed `u_exp_discalib`

### bugs fixed
* `p_bar`,bug fixed, regarding facet plot
* `p_discalib`, bug fixed, regarding the plot display 
* `ds-exp_hts`, bugs fixed, regarding a missing call to an external library and mistakes in station and sensor variables
* `f_properties`, bug fixed, regarding the stacapt variable not correctly written
* `h_weightedsum`, bug fixed, regarding the case of n=1, which was wrong
* `p_hypso`, bug fixed, regarding the case of multiple files

### addings
* `ds_dismeas` for adding, modifying or removing discharge measurements (Shiny app)

### documentation
* `h_common`, help notice now displayed correctly 

------

## version 1.1.0 (6 Dec 2020)
### additions
* new "shiny functions" are added: `ds_station`, `ds_sensor` and `hs_tstep`
* a new function is added `h_wl_di`

### changes
* to be more relevant with the package function names, "shiny functions" are renamed as `ds_invent`, `ds_exp_hts`, `ds_inventory` and `ps_plothts`
* small change in the `ps_plothts` function

### bugs fixed
* warnings fixed regarding RSQLite in few `d_` functions
* bugs fixed in `d_inventory` and `ds_inventory`
* bugs fixed: in `d_station` and `d_sensor`

------

## version 1.0.4 (Nov 2020)
First version posted on CRAN
