#  This application expects a bucket that has been produced by the 
#  KBC DataTypeAssistant application.

# Load libraries
library(shiny)
library(ggplot2)
library(lubridate)
library(DT)
library(keboola.shiny.lib)

shinyServer(function(input, output, session) {
    # create instance of keboola helper library [KEEP]
    klib <- KeboolaShiny$new()
    
    # a title for our application
    appTitle <- "Data Profiler"
    
    # The main method that connects to KBC
    keboola <- reactive({
        # what data does this application need to load from SAPI
        tables <- list(
            cleanData = list(name='CLN__1'), 
            columnTypes = list(name='VAI__1', reducible=FALSE)
        )
        # start it up
        ret <- klib$startup( 
                            list(appTitle = appTitle,
                                 tables = tables,
                                 cleanData = TRUE,
                                 dataToSave = selectedData,
                                 configCallback = configCallback,
                                 description = TRUE,
                                 customElements = NULL))
        
        ret$sourceData <- klib$sourceData()()
        
        # return the login info and sourcedata
        ret        
    })
    
    # shorthand to the data
    sourceData <- reactive({
        keboola()$sourceData
    })
    
    # update the inputs with loaded data values
    observe({
        dataSet <- sourceData()
        if (length(dataSet) == 0) {
            print("sourcedata is empty")
            NULL
        } else {
            updateSelectInput(session, "ycol", "Y Axis", choices = names(dataSet$cleanData))
            tsAxis <- dataSet$columnTypes[which(dataSet$columnTypes$is_ts == 1), 'var_name']
            updateSelectInput(session, "xcol", "X Axis", choices = names(dataSet$cleanData), selected = tsAxis)    
            
            updateSelectInput(session, "rangeVar", 
                              choices = names(dataSet$cleanData[sapply(dataSet$cleanData, is.numeric)]))
            updateSelectInput(session, "dateCol", 
                              choices = names(dataSet$cleanData[sapply(dataSet$cleanData, function (x) is.Date(x) | is.POSIXt(x))]))
            updateSelectInput(session, "color", 
                              choices = c("None", names(dataSet$cleanData[sapply(dataSet$cleanData, is.factor)])))       
            updateSelectInput(session, "facetCol", 
                              choices = c(None = ".", names(dataSet$cleanData[sapply(dataSet$cleanData, is.factor)])))
            updateSelectInput(session, "includedCols", 
                              choices = names(dataSet$cleanData), selected = names(dataSet$cleanData))
            updateSelectInput(session, "histogramVar", 
                              choices = names(dataSet$cleanData[sapply(dataSet$cleanData, function (x) { !is.factor(x)})]))
            
            print("Input elements updated with sourcedata")    
            TRUE
        }
    })
    
    #' method is called when an input configuration is loaded
    #' this method should create and update all necessary inputs    
    configCallback <- function(config) {
        updateSelectInput(session, "xcol", selected=config$xcol)
        updateSelectInput(session, "ycol", selected=config$ycol)
        updateSelectInput(session, "color", selected=config$color)
        updateSelectInput(session, "facetCol", selected=config$facetCol)
        updateCheckboxInput(session, "smooth", value=config$smooth)
        
        #sidebar
        updateSelectInput(session, "includedCols", selected=config$includedCols)
        updateSelectInput(session, "rangeVar", selected=config$rangeVar)
        for (range in config$rangeVar) {
            updateSliderInput(session, range, value=c(config[[range]][1],config[[range]][2]))
        }
        updateSelectInput(session, "dateCol", selected=config$dateCols)
        for (date in config$dateCol) {
            updateDateRangeInput(session, date, start=config[[date]][1], end=config[[date]][2])
        }
    }
    
    # Plot histogram
    output$histPlot <- renderPlot({
        
        options(scipen = 10)
        outdat <- selectedData()            
        
        x    <- outdat[, input$histogramVar]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        if (is.POSIXt(x)) {
            ret <- hist(x, breaks = bins, col = 'lightblue', border = 'white', xlab = input$histogramVar, format = "%m / %Y", main = paste("Histogram for", input$histogramVar))
        } else {
            ret <- hist(x, breaks = bins, col = 'lightblue', border = 'white', xlab = input$histogramVar, main = paste("Histogram for", input$histogramVar))
        }
        options(scipen=0)  # restore the default
        return(ret)
    })
    
    # get source data limited to input ranges
    selectedData <- reactive({
        if (is.null(sourceData())) return(NULL)
        outdat <- sourceData()$cleanData
        if (length(input$rangeVar) > 0) {
            for (i in 1:length(input$rangeVar)) {
                rangeElem <- input$rangeVar[i]
                outdat <- outdat[which(
                    (outdat[,rangeElem] > input[[rangeElem]][1]) &
                        (outdat[,rangeElem] < input[[rangeElem]][2])), ]
            }
        }
        if (length(input$dateCol) > 0) {
            for (i in 1:length(input$dateCol)) {
                dateElem <- input$dateCol[i]
                timeInterval <- new_interval(input[[dateElem]][1],input[[dateElem]][2])
                outdat <- outdat[which(
                    outdat[,dateElem] %within% timeInterval),]    
            }
        }
        # only include columns specified (if any).
        if (length(input$includedCols)) {
            outdat <- outdat[,names(outdat) %in% input$includedCols]
        }
        return(outdat)
    })
    
    output$cleanTable <- renderUI({
        df <- selectedData()
        ret <- list(DT::datatable(df))
        return(ret)
    })
    
    output$vaiTable <- renderUI({
        return(list(DT::datatable(sourceData()$columnTypes)))
    })
    
    # This is text that will appear under the main plot giving hints about which columns may be interesting.
    output$plotHint <- renderUI({ 
        vai <- sourceData()$columnTypes
        ts <- vai[which(vai$is_ts == 1), 'var_name']
        variable <- vai[which(vai$monotonic == 'variable' & vai$is_identity != 'yes'), 'var_name']
        monotonic <- vai[which((vai$monotonic == 'increasing' | vai$monotonic == 'decreasing') & (vai$is_identity != 'yes') & (vai$is_ts == 0)), 'var_name']
        ret <- list()
        if (length(ts) > 0) {
            # there will be always at most one TS variable 
            if (length(variable) > 0) {
                ret[[length(ret) + 1]] <- p(paste0("You might want to plot ", paste(variable, collapse = ', '), " (Y axis) against ", ts, " (X axis)."))
            }
            if (length(monotonic) > 0) {
                ret[[length(ret) + 1]] <- p(paste0("Plot ", paste(monotonic, collapse = ', '), " (Y axis) against ", ts, " (X axis) will be quite boring."))
            }
        }
        return(ret)
    })
    
    # Create the main plot
    output$plot <- renderPlot({ 
        if (input$xcol == "" || input$xcol == "None" || input$ycol == "" || input$ycol == "None") {
            return(NULL)
        }
        vai <- sourceData()$columnTypes
        mode <- vai[which(vai$var_name == input$ycol), 'mode']
        p <- ggplot(selectedData(), aes_string(x = input$xcol, y = input$ycol))
        if (mode == 'continuous') {
            p <- p + geom_line()
        } else {
            p <- p + geom_point()
        }
        
        if (input$color != 'None' & input$color != "") {
            p <- p + aes_string(color=input$color)
        }
        facets <- paste('. ~', input$facetCol)
        if (facets != '. ~ .' & input$facetCol != "") {
            p <- p + facet_grid(facets)
        }
        if (vai[which(vai$var_name == input$xcol), 'data_type'] == 'date') {
            p <- p + scale_x_date(labels = scales::date_format("%d. %m. %Y"))
        }
        if (vai[which(vai$var_name == input$xcol), 'data_type'] == 'datetime') {
            p <- p + scale_x_datetime(labels = scales::date_format("%d. %m. %Y"))
        }
        if (vai[which(vai$var_name == input$ycol), 'data_type'] == 'date') {
            p <- p + scale_y_date(labels = scales::date_format("%d. %m. %Y"))
        }
        if (vai[which(vai$var_name == input$ycol), 'data_type'] == 'datetime') {
            p <- p + scale_y_datetime(labels = scales::date_format("%d. %m. %Y"))
        }
        if (input$smooth) {
            p <- p + geom_smooth()
        }
        return(p)
    })
    
    # Dynamic numeric range select elements built for columns selected in input$rangeVar
    output$rangeSelectors <- renderUI({
        if (length(input$rangeVar) > 0) {
            sd <- sourceData()$cleanData
            config <- klib$kfig$selectedConfig()
            ret <- lapply(seq_along(input$rangeVar), function(x) {
                # the default range is set to the min/max of the selected column
                val <- c(
                    min(sd[,input$rangeVar[x]]),
                    max(sd[,input$rangeVar[x]]))
                # OR, if found in the loaded config, set to the values stored.
                if (input$rangeVar[x] %in% names(config)) {
                    val <- c(config[[input$rangeVar[x]]][1], 
                             config[[input$rangeVar[x]]][2])
                } 
                # our range element
                sliderInput(input$rangeVar[x], input$rangeVar[x],
                            min = min(sd[,input$rangeVar[x]]),
                            max = max(sd[,input$rangeVar[x]]),
                            value = val)
            })
        } else {
            ret <- NULL
        }
        return(ret)
    })
    
    # Dynamic daterange elements created for date/time columns selected in input$dateCol
    output$datePicker <- renderUI({
        if (length(input$dateCol) > 0) {
            sd <- sourceData()$cleanData
            lapply(seq_along(input$dateCol), function(x) {
                # default start and end are take as min/max values
                start = min(sd[,input$dateCol[[x]]])
                end = max(sd[,input$dateCol[[x]]])
                # Or set to be those stored in the selected config
                if (input$dateCol[[x]] %in% names(config)) {
                    start = config[[input$rangeVar[x]]][1]
                    end = config[[input$rangeVar[x]]][2]
                }
                # our daterange element
                dateRangeInput(input$dateCol[[x]], input$dateCol[[x]],
                               min = min(sd[,input$dateCol[[x]]]),
                               max = max(sd[,input$dateCol][[x]]),
                               start = start,
                               end = end
                )
            })
        }
    })
})