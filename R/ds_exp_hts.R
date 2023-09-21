#' @title Extraction of a time-series from htsr data base
#'
#' @author P. Chevallier - Oct 2017 - Sep 2023
#'
#' @description The function display a web page allowing to extract a time-series in the "hts" format.
#'
#' @details
#'  Complete the requested information in the left panel, then press the submit button in
#' order to extract the file. If you want to display the plot of the extracted file,
#' choose "line" or "bar" and press the plot button.
#'
#'  When the subfunction "d_exp_hts(fsq, sta,sen,rtime=FALSE,dstart=NA,dend=NA, rplot=FALSE)"
#'  is used solely it returns a tibble tstab with 4 columns Date, Value, Station, Sensor.
#'  In this last subfunction fsq is the sqlite data base; sta, the station id, sen, the sensor id; rtime, dstart and
#'  dend define a time interval; rplot, the resulted plot.
#'
#' @return The function returns a file (nomfic) with the following name: <sensor.id>_<station.id>.hts
#'


# Define UI ------
ds_exp_hts <- function () {

	requireNamespace("shiny", quietly = TRUE)
	requireNamespace("shinyFiles", quietly = TRUE)
	requireNamespace("tibble", quietly = TRUE)
	requireNamespace("dplyr", quietly = TRUE)
	requireNamespace("lubridate", quietly = TRUE)
	requireNamespace("ggplot2", quietly = TRUE)

	ui <- fluidPage(
		theme = NULL,
		titlePanel("hts file import"),

		fluidRow(sidebarLayout(
			sidebarPanel(
				width = 5,
				shinyFilesButton("file", "File select", "Please select a sqlite data base",
												 multiple = FALSE, viewtype = "detail", class = "btn btn-primary"),
				textInput("Station_id", "Station ID"),
				textInput("Sensor_id", "Sensor ID"),
				checkboxInput("Set_time", "Reduce time interval", value = FALSE),
				dateRangeInput("dates", "Time interval range"),
				actionButton("submit", "Submit", class = "btn btn-warning"),
				radioButtons(
					"plot_opt",
					br("Plot the extracted file"),
					c("line" = "line", "bar" = "bar"),
					selected = "line"
				),
				actionButton("plot", "Plot", class = "btn btn-success"),
				hr(),
				actionButton("close", "Done", class="btn btn-danger")
			),
			mainPanel(
				width = 7,
				textOutput("fsq"),
				textOutput("extract_written"),
				plotOutput("file_plot")
			)
		))
	)


	server <- function(input, output, session) {

		volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())

		shinyFileChoose(input, "file", roots = volumes, session = session,
										filetypes="sqlite")

		observeEvent(input$file, {
			tab <- parseFilePaths(volumes, input$file)
			fsq <- as.character(tab[1,4])
			output$fsq <- renderText({paste("Selected sqlite data base:",fsq)})
		})

		re <- eventReactive (input$submit, ({
			req(input$file)
			tab <- parseFilePaths(volumes, input$file)
			fsq <- as.character(tab[1,4])
			output$fsq <- renderText({paste("Selected sqlite data base:",fsq)})

			sta <- input$Station_id
			sen <- input$Sensor_id
			sta1 <- paste0("'",sta,"'")
			wd <- paste0(dirname(fsq),"/")

			fileo <- paste0(wd, input$Sensor_id,"_",
											input$Station_id,".hts")
			compute <- TRUE

			if (sta == ""){
				compute <- FALSE
				rep <- paste0("A station id is mandatory!")
			}
			if (compute && sen == ""){
				compute <- FALSE
				rep <- paste0("A sensor id is mandatory!")
			}
			if (compute) {
				conn <- dbConnect(SQLite(),fsq)
				x <- dbReadTable(conn, "ST")
				nom <- x$Id_Station
				if (!(sta %in% nom)){
					compute <- FALSE
					rep <- paste0("The station ", sta, " is not in the data base!")
				}
				dbDisconnect(conn)
			}
			if (compute) {
				conn <- dbConnect(SQLite(),fsq)
				selection <- paste ("SELECT * FROM SS WHERE Id_station =",sta1)
				x <- dbGetQuery(conn, selection)
				nom <- x$Capteur
				if (!(sen %in% nom)){
					compute <- FALSE
					rep <- paste0("The sentor ", sen, " not exists for the station ", sta, "!")
				}
				dbDisconnect(conn)
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


			if (compute) {
				tstab <-d_exp_hts(fsq = fsq, sta, sen, rtime = input$Set_time,
					dstart=as.character((input$dates[1])), dend=as.character((input$dates[2])),
					rplot = FALSE)
				save (file=fileo, tstab)
				rep <- paste0("File written: ", fileo)
			}

			po <- eventReactive (input$plot, {
				htsr::z_set(file.names= fileo, plot.label = sen,title = sta)
				if (input$plot_opt=="bar") htsr::p_bar()
				else htsr::p_line()
			})

			output$file_plot <- renderPlot({po()})

			return(rep)
		}))

		output$extract_written <- renderText({re()})

		observeEvent(input$close, {stopApp()})

	}

	# Run the app
	shinyApp(ui = ui, server = server)
}

