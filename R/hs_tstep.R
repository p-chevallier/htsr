#' @title Shiny app: convert f file with fixed time-step
#'
#' @author P. Chevallier - Dec 2020 - Aug 2023
#'
#' @description Shiny application of the  functions \code{\link{h_timestep}} and \code{\link{h_month}}
#'
#' @details
#' The output files are written in the specified working directory.
#'
#'
#' @return a shiny session

hs_tstep <- function (){
#
	requireNamespace("shiny", quietly = TRUE)
	requireNamespace("shinyFiles", quietly = TRUE)
	requireNamespace("tibble", quietly = TRUE)
	requireNamespace("dplyr", quietly = TRUE)
	requireNamespace("lubridate", quietly = TRUE)
	requireNamespace("waiter", quietly = TRUE)

	tstep <- c("monthly", "daily", "12h", "6h", "3h", "2h", "hourly", "30mn",
						 "10mn", "5mn")
	lmode <- c("average", "max", "min", "sum")

	# Define UI
	ui <- fluidPage(

		waiter::use_waiter(),
		titlePanel("Calculation of fixed time-step files"),

		fluidRow(
			shinyFilesButton("file", "File upload", "Please select hts files in the same folder",
											 multiple = FALSE, viewtype = "detail", class="btn btn-primary"),
			textOutput("ff"),
			br()
		),

		fluidRow(
			column(width = 4,
						 selectInput("ts", "Time-step", tstep, selected = "daily"),
						 numericInput("shift", "If daily, shift (hours)",0,0,23,1)
			),
			column(width = 4,
						 selectInput("mode", "Mode", lmode, selected = "average")
			),
			column(width = 4,
						 p(strong("If monthly")),
						 checkboxInput("climedit", "climatogy file"),
						 checkboxInput("rmna", "remove NA"),
						 checkboxInput("gapfill", "gapfilling"),
						 checkboxInput("hts_year", "extract year stat")
			)
		),

		fluidRow(
			verbatimTextOutput("MESS2"),
			actionButton("submit", "Submit", class = "btn btn-warning"),
			br(),
			textOutput("mon"),
			textOutput("MESS"),
			textOutput("MESS1"),
			hr(),
			actionButton("close", "Done", class = "btn btn-danger")
		)

	)

	# Define server
	server <- function(input, output, session) {

		options(shiny.maxRequestSize=1000*1024^2)

		volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
		shinyFileChoose(input, "file", roots = volumes, session = session,
										filetypes="hts")

		observeEvent(input$file, {
			tabfile <- parseFilePaths(volumes, input$file)
			ff <- tabfile$datapath[1]

			output$ff <- renderText ({paste("selected file:", ff)})
			output$MESS2 <- renderText({
				"The calculation time depends on the number of records and the time step.
      It can last. Wait for the file writing message to appear. Be patient!"})
		})

		observeEvent(input$submit, {
			tabfile <- parseFilePaths(volumes, input$file)
			ff <- tabfile$datapath[1]

			tst <- 1440
			mn <- FALSE
			if(input$ts == "monthly") mn <- TRUE
			if(input$ts == "hourly") tst <- 60
			if(input$ts == "daily") tst <- 1440
			if(input$ts == "5mn") tst <- 5
			if(input$ts == "10mn") tst <- 10
			if(input$ts == "30mn") tst <- 30
			if(input$ts == "2h") tst <- 120
			if(input$ts == "3h") tst <- 180
			if(input$ts == "6h") tst <- 360
			if(input$ts == "12h") tst <- 720
			if(input$mode == "average") op <- "M"
			if(input$mode == "sum") op <- "S"
			if(input$mode == "min") op <- "Mn"
			if(input$mode == "max") op <- "Mx"
			shift <- as.numeric(input$shift)

			# Journalier et infra-journalier
			if (mn) {
				waiter <- waiter::Waiter$new()
				waiter$show()
				on.exit(waiter$hide())
				tst <- 1440
				f <- h_timestep(file=ff, tst=1440, op = op, shift = 0)
				f1 <- h_month(file = f, op = op, ba = NA, rmna = input$rmna, climedit = input$climedit,
											gapfill = input$gapfill, hts_year = input$hts_year)
				output$MESS1 <- renderText({paste("File written:", f1[1],
																					" with eventual accompanying files")})
			} else {
				waiter <- waiter::Waiter$new()
				waiter$show()
				on.exit(waiter$hide())
				f <- h_timestep(file=ff, tst=tst, op = op, shift = shift)
				output$MESS <- renderText({paste("File written:", f)})
			}
		})

		observeEvent(input$close, stopApp())
	}

	# Run the application
	shinyApp(ui = ui, server = server)
}
