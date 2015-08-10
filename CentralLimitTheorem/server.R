##Code originally by Alex Stringer
##substantive bug fixes and modifications by Victor Veitch
##10/08/2015


library(ggvis)
library(shiny)
library(dplyr)
rm(list = ls())

shinyServer(function(input,output)
{
    # Input objects:
    #   obsClick: counter, increments when Observation button is clicked
    #   obsClear: clear observations, save the sample mean
    
    # Output objects:
    #   currentObs: text of the current observation
    #   currentMean: text of the current sample mean
    
    # Static population mean
    pop.mean <- 5
    popOut <- renderText({ pop.mean })
    
    # Initialize the observation and mean vectors
    obsVec <- c(0)
    meanVec <- mean(obsVec)
    
    # Create a reactive object storing the obsVec length
    obsVecLength <- reactive({
        input$obsClick
        input$sampleSize
        length(obsVec)
    })
    
    # Create a reactive object storing the meanVec length
    meanVecLength <- reactive({
        input$obsClick
        length(meanVec)
    })
    
    # When the button is pressed, generate a new sample
    observe({
        input$obsClick
        meanVec <<- c(meanVec,mean(obsVec))
        obsVec <<- rchisq(input$sampleSize,df = pop.mean)
    })

    
    # Clear the means if the sample size changes
    observe({
        input$sampleSize
        obsVec <<- c(0)
        meanVec <<- c(0)
    })
    
    ### Make the plots ###
    
    # Initialize the density curve
    
    basePlt <- reactive({
        data.frame(x = seq(0,pop.mean*4,by = 0.1),
                   y = dchisq(seq(0,pop.mean*4,by = 0.1),df = pop.mean)
            ) %>%
            mutate(xobs = c(obsVec,rep(0,length(x) - obsVecLength())),
                   yobs = rep(0,length(x))
            ) %>%
            ggvis(~x,~y
            ) %>%
            layer_lines(
            ) %>%
            layer_points(~xobs,
                        ~yobs,
                        fill := 'orange',
                        stroke := 'black',
                        opacity := 0.7
            ) %>%
            add_tooltip(function(df) round(df$y,2)
            ) %>%
            add_axis('x',
                     title = 'Observations',
            ) %>%
            add_axis('y',
                     title = 'Density'
            )
    })
    
    # Make histogram of means
    
    histPlt <- reactive({
        input$obsClick
        input$sampleSize
        data.frame(x = meanVec
            ) %>%
            ggvis(~x
            ) %>%
            layer_histograms(
                width = 0.2
            ) %>%
            scale_numeric("x", domain = c(0, pop.mean*4), nice = FALSE) %>%
            add_axis('x',
                     title = 'Sample Means'
            ) %>%
            add_axis('y',
                     title = 'Count'
            )
    })
    
    # Redeploy the plots when their properties change
    
    observe({
        input$obsClick
        input$sampleSize
        basePlt() %>% bind_shiny('basePlt')
    })
    
    observe({
        input$obsClick
        input$sampleSize
        histPlt() %>% bind_shiny('histPlt')
    })
    
    # Display the current sample mean
    output$currentMean <- renderTable({
        input$obsClick
        input$sampleSize
        data.frame(Mean = round(meanVec,2))
    })
    
})

                     
    
    
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        