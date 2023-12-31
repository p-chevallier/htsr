#' @title Import a hts file into a data base
#'
#' @author P. Chevallier - jan 2019-jan 2024
#'
#' @description Import a hts file into a tshm sqlite base
#'
#' @details
#' The main table where the data have to be imported must be selected with one of the
#' following abbreviation: WL (water level), DI (discharge), WE (weather), PR (precipitation)
#' or QU (quality)
#' If records already exist during the same interval, they are removed and replaced.

#' @param fsq Full name of the data base
#' @param filein Full name of hts file to import
#' @param table Table
#' @param bku Automatic Backup TRUE (default) / FALSE
#'
#'
#' @return Actualized data base
#'


d_imp_hts <- function(fsq, filein, table, bku = TRUE) {

  #Init
  tstab <- Table <- Station <- Sensor <- Value <-NULL

  # Warnings
  if (!file.exists(fsq))
    return(warning("\nThis data base doesn't exist, Verify!\n"))
  conn <- dbConnect(SQLite(),fsq)
  ltable <- dbListTables(conn)
  dbDisconnect(conn)
  if(!("ST") %in% ltable || !("SS") %in% ltable)
    return(warning("\nNo table 'ST' and/or 'SS in the data base.\n"))
  if(!(table %in% c("WL", "DI", "WE", "PR", "QU")))
    return(warning("\nTable name not authorized.\n"))
  if(!(table %in% ltable))
    return(warning("\nNo table", table, "in the data base.\n"))
  if((table %in% c("WL", "DI", "QU"))) type_st <- "H"
  if((table %in% c("WE", "PR"))) type_st <- "M"

  load(file=filein)
  sta <- as.character(tstab$Station[1])
  sen <- as.character(tstab$Sensor[1])


  conn <- dbConnect(SQLite(),fsq)
    selec <- paste ("SELECT * FROM ST")
    xst <- dbGetQuery(conn, selec)
    sta1 <- paste0("'",sta,"'")
    selec <- paste ("SELECT * FROM SS WHERE Id_Station =", sta1)
    xcp <- dbGetQuery(conn, selec)
  dbDisconnect(conn)
  if(!(sta %in% xst$Id_Station))
    return(warning("\nNo station", sta, "in the data base. Create it\n"))
  if(!(sen %in% xcp$Capteur))
    return(warning("\nNo sensor", sen, "in the data base for the considered station. Create it\n"))

  # Remove existing records
  conn <- dbConnect(SQLite(),fsq)
    sen1 <- paste0("'",sen,"'")
    selec <- paste ("SELECT * FROM", table, "WHERE Id_Station =", sta1, "AND Capteur = ", sen1)
    xrec <- dbGetQuery(conn, selec)
  dbDisconnect(conn)
  if(nrow(xrec)!= 0) {
    st <- min(as.numeric(as.POSIXct(tstab$Date), origin ="1970-01-01", tz="UTC"))
    en <- max(as.numeric(as.POSIXct(tstab$Date), origin ="1970-01-01", tz="UTC"))
    d_rem_hts(fsq, table, sta, sen, start=st, end=en)
  }
  # if (is.na(rt)) return(message("\n"))
  # if (rt == 0 && bku == TRUE) d_backup(fsq)

  # Adding records
  x <- transmute (tstab, Type_Station = as.factor(type_st), Id_Station = Station,
    Capteur = Sensor, Date = Date, Valeur = Value,
    Origine = NA, Qualite = NA)
  if(table == "PR") x <- mutate(x, Nature = NA)
  nrl <- nrow(x)
  conn <- dbConnect(SQLite(),fsq)
    dbWriteTable(conn, name=table, x, append = TRUE)
  	message("\n ",nrl, " records have been added to the table.\n")
  dbDisconnect(conn)
}

#Fin
