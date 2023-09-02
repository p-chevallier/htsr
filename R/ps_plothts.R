#' @title Shiny app: plot hts files
#'
#' @author P. Chevallier - May 2020 - Sep 2023
#'
#' @description Shiny application of htsr functions \code{\link{p_line}} and \code{\link{p_bar}},
#'  associated with \code{\link{z_set}}
#'
#' @details When launched, a shiny window is open. Follow the instructions and steps.
#'

ps_plothts <- function () {

	library(shiny)
	library(shinyFiles)
	library(tibble)
	library(dplyr)
	library(lubridate)
	library(ggplot2)
	library(editData)
	library(htsr)

	# Define UI ------
	ui <- fluidPage(

		titlePanel("Files ploting"),

		fluidRow(
			verbatimTextOutput("text1"),

			shinyFilesButton("file", "File upload", "Please select hts files in the same folder",
											 multiple = TRUE, viewtype = "detail", class="btn btn-primary"),
			br(),

			editData::editableDTUI("table2"),
			br(),
			actionButton("savefil", "Save file settings", class = "btn-warning"),
			hr()
		),

		fluidRow(
			splitLayout(
				textInput("title", "Title", "Title"),
				textInput("yaxis", "y-Axis label", "y-Axis label"),
			)
		),
		fluidRow(
			column(4,
						 checkboxInput("fixy","Set y-Axis scale", FALSE),
						 conditionalPanel(
						 	condition = "input.fixy",
						 	numericInput("ymin", "Min Y value",NA),
						 	numericInput("ymax", "Max Y value",NA)
						 )
			),
			column(4,
						 checkboxInput("fixtime","Set time interval", FALSE),
						 conditionalPanel(
						 	condition = "input.fixtime",
						 	dateRangeInput("daterange", "Time interval",
						 								 start = as_date("2000-01-01"), end=as_date(now()))
						 )
			),
			column(4,
						 selectInput("pal", "Color palette", "ggplot2", choices = c(
						 	"R3", "R4", "ggplot2", "Okabe-Ito", "Accent", "Dark 2",
						 	"Paired", "Pastel 1", "Pastel 2", "Set 1", "Set 2", "Set 3",
						 	"Tableau 10", "Classic Tableau", "Polychrome 36", "Alphabet"))
			)
		),
		fluidRow(
			column(2,
						 checkboxInput("plot.point", "Plot points", FALSE)
			),
			column(2,
						 checkboxInput("normval","Normalized values", FALSE)
			),
			column(2,
						 checkboxInput("trend", "Linear trend", FALSE)
			),
			column(2,
						 checkboxInput("facet","Facet plot", FALSE)
			),
		),
		fluidRow(
			br(),
			actionButton("saveconf", "Save plot settings", class = "btn-warning"),
			hr(),
			splitLayout(
				actionButton("plot", "Plot", class="btn btn-success"),
				radioButtons("linbar", "Plot type", choices = c("line", "bar"), inline = TRUE),
				checkboxInput("saveplot", "Save plot")
			),
			conditionalPanel(
				condition = "input.plot",
				plotOutput("plotresult", height = "600px"),
				hr(),
			),
			conditionalPanel(
				condition = "input.saveplot",
				column(5,
							 textInput("plotname", "Plot name (allowed extension: jpg, png, pdf)", "plot.png")
				),
				column(2,
							 numericInput("plotwidth", "Width (cm)", 8)
				),
				column(2,
							 numericInput("plotheight", "Height (cm)", 6)
				),
				column(3,
							 actionButton("confirmsave", "Confirm Save", class="btn btn-success")
				),
			),
		),
		actionButton("close", "Done", class="btn btn-danger")
	)



	# Define server logic -----
	server <- function (input, output, session) {
		options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
		options(warn=-1)
		conf<-fil<-palette<-NULL

		volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
		shinyFileChoose(input, "file", roots = volumes, session = session,
										filetypes="hts")

		output$text1 <- renderPrint({
			cat('Upload files located in the same folder (8 max) and press "File settings".\n')
			cat('Modify the file parameters in the table, then press "Save file settings".\n')
			cat('And/or modify the general plotting parameters, then press "Save plot settings".\n')
			cat('Press "Plot" at any time after saving file and plotting parameters.\n')
			cat('The plot is saved in the directory of the files. The extensions allowed are .png, .jpg and .pdf')
		})

		observeEvent(input$file, {
			tab <- parseFilePaths(volumes, input$file)
			ff <- tab$datapath
			wd <- dirname(ff[1])

			nf <- nrow(tab)
			plot.label <- vector(mode="character", length = nf)
			for (i in 1:nf) {plot.label[i] <- paste ("label ", i)}

			fil <- tibble(file.names=ff, plot.label, line.type = 1, line.width = 0.2,
										point.shape = 20, point.size = 2)

			myfil <- fil
			df=callModule(editData::editableDT,"table2",data=reactive(myfil))

			observeEvent(input$savefil,{
				req(input$file)
				fil <- df()
				save(fil, file = "fil.RData")
			})
		})

		observeEvent(input$saveconf, {
			req(input$file)
			conf <- reactive(c(input$title,  input$yaxis,
												 input$normval, input$fixtime,
												 input$daterange[1], input$daterange[2],
												 input$fixy, input$ymin, input$ymax,
												 input$trend, input$facet, input$plot.point))
			conf <- conf()
			save(conf, file = "conf.RData")
		})


		observeEvent(input$plot, {
			req(input$file, input$saveconf, input$savefil)
			load(file ="fil.RData")
			load(file="conf.RData")
			palette <- input$pal
			tz <- "UTC"
			save(file="settings.RData", fil, conf, palette, tz)
			output$plotresult <- renderPlot({
				load(file = "settings.RData")
				if(input$linbar == "line"){
					p_line()
				} else {
					p_bar()
				}
			})
			observeEvent(input$confirmsave,{
				req(input$plot, input$file)
				tab <- parseFilePaths(volumes, input$file)
				ff <- tab$datapath
				wd <- dirname(ff[1])

				filename <- paste0(wd, "/", input$plotname)

				ggsave(filename=filename, width=input$plotwidth,
							 height=input$plotheight, dpi=300)
			})
		})

		observeEvent(input$close, {stopApp()})
	}


	# Run the app
shinyApp(ui = ui, server = server)
}
