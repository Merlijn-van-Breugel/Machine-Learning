function(input, output, session) {

    #Load packages and source functions
    # install.packages('homals')        # Install package homals   
    # library(homals)                   # Load the homals package for nonlinear PCA
    # install.packages('stats')         # Install package stats for k-means    
    # library(stats)                    # Load package stats for k-means 
    # install.packages('ellipse')         # Install package stats for k-means   
    # library(ellipse)
    # library(ggplot2)
    # library(grid)
    # library(gridExtra)
     
    
    # source("plot.homals.R")           # Overrides default homals plot command
    # source("rescale.R")               # Rescales the output of homals    
    # Combine the selected variables into a new data frame


    imax <- reactive({
        kmeans(selectedData$objscores, centers = input$c, iter.max = 15, nstart = 1,
               algorithm = "Hartigan-Wong", trace=FALSE)$iter
    })
    
    output$plot1 <- renderPlot({
        plotClusters(data=selectedData,iterations = input$iteration,centers = input$c)
    })
    
    output$totss <- renderText({
        paste("Total within cluster sum of squares = ", 
              toString(round(plotClusters(data=selectedData,iterations = input$iteration,centers = input$c)$tot.withinss,digits = 2)))
    })
}


