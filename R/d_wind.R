#' @title Create a wind table
#'
#' @author P. Chevallier - Dec 2019 - Sep 2023
#'
#' @description Create a tibble with wind direction and speed
#'
#' @param fsq Full name of the htsr data base
#' @param sta Station id
#' @param swd Id of wind direction sensor
#' @param swv Id of wind speed sensor
#'
#' @seealso \code{\link{p_wind}} plots wind roses
#'
#' @examples \dontrun{
#'
#' h_wind (fsq, sta="VB", swd="WD", swv="WV")
#' }
#'
#' @return
#' A RData file containing a tibble named "data_wind" with 5 columns date, month, year, wind_dir, wind_spd

# function h_wind

d_wind <- function(fsq, sta, swd, swv){

	requireNamespace("openair", quietly = TRUE)

	d_exp_hts <- function(fsq, sta,sen,rtime=FALSE,dstart=NA,dend=NA, rplot=FALSE){

		# fonction u_statnom
		u_statnom <- function(fsq,sta){
			conn <- dbConnect(SQLite(),fsq)
			sta <- paste("'",sta,"'",sep="")
			selection <- paste ("SELECT * FROM ST WHERE Id_station =",sta)
			xt <- dbGetQuery(conn, selection)
			nom <- xt$Nom[1]
			dbDisconnect(conn)
			return(nom)
		}

		# fonction u_stacapt
		u_stacapt <- function(fsq,table,sta,sen){
			Valeur <- NULL
			conn <- dbConnect(SQLite(),fsq)
			table1 <- paste("'",table,"'",sep="")
			sta1 <- paste("'",sta,"'",sep="")
			sen1 <- paste("'",sen,"'",sep="")
			selection <- paste ("SELECT * FROM", table1, " WHERE Id_Station =",sta1,
													" AND Capteur =",sen1)
			x <- dbGetQuery(conn, selection)
			xt <- tibble::as_tibble(x)
			dbDisconnect(conn)
			yt <- dplyr::select(xt,Date,Valeur)
			yt <- dplyr::arrange(yt,Date)
			return(yt)
		}

		# corps de fonction
		# suppressWarnings()

		# initialisation
		Sys.setenv(TZ='UTC')
		conn <- dbConnect(SQLite(),fsq)
		sta1 <- paste0("'",sta,"'")
		sen1 <- paste0("'",sen,"'")
		sel <- paste ("SELECT * FROM SS WHERE Id_Station =",sta1,
									" AND Capteur =",sen1)
		t <- dbGetQuery(conn,sel)
		table <- as.character(t$Tabl)
		dbDisconnect(conn)
		#  if(table=="PR") op <-"S" else op <- "Mo"

		# appel u_stacapt
		z <- u_stacapt(fsq, table, sta, sen)
		colnames(z) <- c("Date", "Value")
		z$Date <- as_datetime(z$Date)

		# preparation pour rafinage
		dstart <- as_datetime(dstart)
		dend <- as_datetime(dend)
		date_start <- as_datetime(min(z$Date))
		date_end <- as_datetime(max(z$Date))

		if(rtime) {
			if(is.na(dstart)) dstart <-date_start
			if(is.na(dend)) dend <- date_end
			# z <- window (z, start = dstart, end = dend)
			z <- filter(z, Date > dstart)
			z <- filter(z, Date <= dend)
		}

		nomfic <- paste (dirname(fsq),"/",sen,"_",sta,".hts",sep="")
		tstab <- mutate(z, Station = as.factor(sta), Sensor = as.factor(sen))
		save(tstab, file=nomfic)

		# plot graphe
		if(rplot){
			htsr::z_set(file.names = nomfic, plot.label = sen, title = sta)
			if (table=="PR") p <- htsr::p_bar() else p <- htsr::p_line()
			show(p)
		}

		# sortie
		write(file="",paste("File",nomfic,"extracted !"))
		return (tstab)
	}


  #extraction
  tstab <- d_exp_hts (fsq, sta = sta, sen = swd)
  fwd <- (paste0(swd,"_",sta,".hts"))
  save(file=fwd,tstab)
  rm(tstab)
  tstab <- d_exp_hts (fsq, sta = sta, sen = swv)
  fwv <- (paste0(swv,"_",sta,".hts"))
  save(file=fwv,tstab)
  rm(tstab)

  #tibble de travail
  fcom <- h_common(c(fwd, fwv))
  load(fcom[1])
  data_wind <- select(tstab, date = Date)
  data_wind <- openair::cutData(data_wind, type = "month")
  data_wind <- openair::cutData(data_wind, type = "year")
  data_wind <- mutate(data_wind, wind_dir=tstab$Value)
  load(fcom[2])
  data_wind <- mutate(data_wind, wind_spd=tstab$Value)

  #suppression des fichiers de calcul
  file.remove(fwd)
  file.remove(fwv)
  file.remove(paste0("co_",fwd))
  file.remove(paste0("co_",fwv))

  save (data_wind, file="data_wind.RData")

  return (message("data_wind table created in the data_wind.RData file"))
}
