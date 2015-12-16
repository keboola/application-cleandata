# Frontend

# need this library for the keboolaPage and keboolaModal methods
library(keboola.shiny.lib)

shinyUI(
    keboolaPage(
        fluidPage(
            column(3,
                wellPanel(
                    flowLayout(
                        selectizeInput("rangeVar", "Data Ranges", choices = c(), multiple = TRUE),
                        uiOutput("rangeSelectors")
                    ),
                    flowLayout(
                        selectizeInput("dateCol", "Date Ranges", choices = c(), multiple = TRUE),
                        uiOutput("datePicker")
                    ),
                    conditionalPanel(
                        condition = "input.tabsPanel == 'tableTab'",
                        selectInput("includedCols", "Show Columns", c(), multiple = TRUE)
                    )
                )
            ),
            column(9,
                tabsetPanel(type = "tabs", id = 'tabsPanel', 
                    tabPanel("Description",
                        uiOutput("description")
                    ),
                    tabPanel("Data plot",
                         plotOutput("plot"),
                         # plot specific settings
                         fluidPage(
                             column(6,
                                selectInput('xcol', 'X Axis', choices = c()),
                                selectInput('ycol', 'Y Axis', choices = c())
                             ),
                             column(6,
                                selectInput("color", "Colour", choices = c()),
                                selectInput("facetCol", "Facet Column", choices = c()),           
                                checkboxInput('smooth', 'Smoothed Mean')
                             )
                         ),
                         uiOutput("plotHint")
                    ),                        
                    tabPanel("Table",
                        uiOutput("cleanTable"),
                        value = "tableTab"
                    ),
                    tabPanel("Column Info",
                        uiOutput("vaiTable"),
                        value = "vaiTab"
                    ),
                    tabPanel("Histograms",
                        plotOutput('histPlot'),
                        fluidRow(
                            column(6,
                                   selectizeInput("histogramVar", "Show histogram for: ", choices = c(), multiple = FALSE)
                            ),
                            column(6,
                                   sliderInput("bins",
                                               "Number of bins:",
                                               min = 1,
                                               max = 100,
                                               value = 30
                                   )
                            )
                        )
                    )
                )
            )
        ), 
        appTitle = "Data Profiler"
    )
)