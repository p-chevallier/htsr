#' @title Computation of the discharges from water-levels
#'
#' @author P. Chevallier - Dec 2020
#' @description Computes a discharge time-series from water levels data and calibration curves
#'
#' @param fsq htsr data base
#' @param sta Station Id.
#' @param seni Input sensor Id (water levels)
#' @param seno Output sensor Id (discharges)
#' @param dstart Start date (NA by default)
#' @param dend End date (NA by default)
#' @param dbo Includes the result in the data base (TRUE by default)
#'
#' @details
#' Calibration curves must exist in the data base.
#'
#' If 'dbo' is TRUE, a discharge table "DI" and the sensor 'seno'
#' must exist in the data base. The new discharge time-series overwrites the already existing data ; however, it is
#' asked to confirm the operation. In any case the data base is previously backed up.
#'
#' @seealso The functions \code{\link{ds_exp_hts}} and \code{\link{d_imp_hts}}are used for export the water levels,
#' respectively import the discharges within the data base. The function u_exp_discalib
#' included in \code{\link{p_discalib}} is used for loading the calibration curves.
#'
#' @return Writes an hts file with the resulting discharges and optionally includes it in the data base.
#'


h_wl_di <- function(fsq, sta, seni, seno, dstart=NA, dend=NA, dbo = TRUE){

  u_exp_discalib <- function(fsq, sta, calib=TRUE, dism=TRUE) {

    # initialisation
    tzo <- NULL
    load(file=system.file("extdata/settings.RData",package="htsr"))
    if(calib==FALSE & dism==FALSE)
      return(warning("\nAt least one between calib and dism must be TRUE."))
    Id_Station <- Capteur <- Date <- H <- Q <- Active <- NULL

    # extraction
    # etalonnages
    if(calib==TRUE) {
      conn <- dbConnect(SQLite(),fsq)
      table <- "LC"
      sta1 <- paste("'",as.character(sta),"'",sep="")
      selection <- paste ("SELECT * FROM",
                          table, " WHERE Id_Station =",sta1)
      xt <- dbGetQuery(conn, selection)
      dbDisconnect(conn)

      # controle
      blab <- as.character (xt$Date)
      if(is.na(blab[1])==TRUE)
        return(warning("\nNo calibration data for this station/sensor.\n"))

      # constitution du tableau de sortie
      xt$Date <- as.character(as.POSIXct(xt$Date, origin="1970-01-01"))
      calibtab <- data.frame(xt$Id_Station,xt$Capteur,xt$Capteur_Sortie,xt$Date,
                             xt$H,xt$Q)
      colnames(calibtab) <-c("Id_Station","Sensor","Sen_Out","Date","H","Q")
      calibtab <- as_tibble(calibtab)
    } else calibtab <- NA

    #jaugeages
    if(dism==TRUE) {
      conn <- dbConnect(SQLite(),fsq)
      table <- "DM"
      sta1 <- paste("'",as.character(sta),"'",sep="")
      selection <- paste ("SELECT * FROM",
                          table, " WHERE Id_Station =",sta1)
      xt <- dbGetQuery(conn, selection)
      dbDisconnect(conn)

      # controle
      blab <- as.character (xt$Date)
      if(is.na(blab[1])==TRUE)
        return(warning("\nNo discharge measurement data for this station/sensor.\n"))

      # constitution du tableau de sortie
      xt$Date <- as.character(as.POSIXct(xt$Date, origin="1970-01-01", tz=tzo))
      dismtab <- as_tibble(xt)
      dismtab <- select(dismtab, Id_Station, Capteur, Date, H, Q, Active)
      colnames(dismtab) <-c("Id_Station","Sensor","Date","H","Q","Active")
    } else dismtab <- NA

    # retour
    caldis <- list(calibtab, dismtab)
    return (caldis)
  }
  #FIN FUNCTION
  #----------


  # fonction hauteur-debit
  hq <- function (h, Hc, Qc) {
    q <- NA
    for (i in 1:(length(Hc) - 1)) {
      if (!is.na(h) && h >= Hc[i] && h < Hc[i+1])
        q <- Qc[i] + ((Qc[i+1]-Qc[i]) * (h-Hc[i]) / (Hc[i+1]-Hc[i]))
    }
    return(q)
  }

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



  # nommage fichier sortie
  fileo <- paste0(dirname(fsq),"/",seno,"_",sta,".hts")

  # extraction du fichier des hauteurs de la base de donnees
  if(is.na(dstart)) rtime <- FALSE else rtime <- TRUE
  xh <- d_exp_hts(fsq =fsq, sta =sta, sen = seni, rtime = rtime,
    dstart, dend, rplot = FALSE)

  # extraction des courbes de tarage de la base de donnees
  CD <- NULL
  calib <- u_exp_discalib(fsq = fsq, sta=sta, calib = TRUE, dism = FALSE)[[1]]
  calib <- mutate (calib, CD = as.factor(Date))
  calib$Date <- as_datetime(calib$Date)
  nc <- length(levels(calib$CD))   # nbre d'etalonnages
  dcdeb <- as_datetime(levels(calib$CD)) # date debut de chaque etalonnage

  # boucle sur les etalonnages
  tstab <- x <-  NULL

  # cas etalonnage unique
  if(nc==1) {
    Hc <- calib$H
    Qc <- calib$Q
    xhf1 <- filter(xh, Date < dcdeb[1])
    xhf1$Value <- NA
    xhf2 <- dplyr::filter (xh, Date >= dcdeb[1])
    a <- vector(mode = "double", length = nrow(xhf2))
    for(j in 1: nrow(xhf2)) a[j] <- hq(xhf2$Value[j], Hc, Qc)
    xhf2 <- transmute(xhf2, Date, Value = a, Station = as.factor(sta),
      Sensor = as.factor (seno))
    tstab <- bind_rows(xhf1, xhf2)
  } else {

    # cas plusieurs etalonnages
    for(i in 1:nc) {
      calibi <- dplyr::filter(calib, CD == levels(calib$CD)[i])
      Hc <- calibi$H
      Qc <- calibi$Q
      if (i == 1) {
        x <- filter(xh, Date < dcdeb[1])
        x$Value <- NA
      }
      xhf <- dplyr::filter(xh, (Date >= dcdeb[i]))
      if (i != nc) xhf <- dplyr::filter(xhf, (Date < dcdeb[i+1]))
      a <- vector(mode = "double", length = nrow(xhf))
      for(j in 1: nrow(xhf)) a[j] <- hq(xhf$Value[j], Hc, Qc)
      x <- transmute(xhf, Date, Value = a, Station = as.factor(sta),
        Sensor = as.factor (seno))
      tstab <- bind_rows(tstab, x)
    }
  }

  # écriture fichier et enregistrement dans la base de données
  save(tstab, file = fileo)
  message("File written: ", fileo)
  if (dbo) d_imp_hts(fsq=fsq, file = fileo, table = "DI", bku = TRUE)
}
