##Code originally by Alex Stringer
##substantive bug fixes and modifications by Victor Veitch
##10/08/2015


library(ggvis)
library(shiny)
library(dplyr)
library(sn)
rm(list = ls())

#fixed skew normal parameters
omega=5;
tau=0;

#cerulean
scheme_colour <- "#2fa4e7"

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
    
    # Initialize the mean vectors
     meanVec <- numeric(0)
#     
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
        obsVec <<- rsn(input$sampleSize,omega=omega,alpha=input$skew,tau=tau) 
        meanVec <<- c(meanVec,mean(obsVec))
    })

    
    # Clear the means if the sample size or skew changes
    cleared<-observe({
        input$skew
        input$sampleSize
        obsVec <<- numeric(0)
        meanVec <<- numeric(0)
    })
    
    ### Make the plots ###
    
    # Initialize the density curve
    
basePlt <- reactive({
  data.frame(x=obsVec, y=rep(0,obsVecLength())) %>%
    ggvis() %>%
    #scatter plot of the generated data points
    layer_points(~x,
                 ~y,
                 fill := scheme_colour,
                 stroke := 'black',
                 opacity := 0.7
    ) %>%
    #density curve     
    add_data(data = data.frame(xtick = seq(-omega*4,omega*4,by = 0.1), 
                               yval=dsn(seq(-omega*4,omega*4,by = 0.1),omega=omega,alpha=input$skew,tau=tau))
    ) %>% 
    layer_paths(x = ~ xtick, y = ~ yval) %>%
    #verticle line at sample mean. This is a total hack because appropriate functionality does not yet exist
    add_data(data = data.frame(x_coords = c(meanVec[length(meanVec)],meanVec[length(meanVec)]),
                               y_coords = c(0,0.16))
    ) %>% 
    layer_paths(x = ~ x_coords, y = ~ y_coords, stroke:=scheme_colour, strokeWidth := 4) %>%
    scale_numeric("x", domain = c(-omega*4, omega*4), nice = FALSE) %>%
    add_axis('x',
             title = 'Observations',
    ) %>%
    add_axis('y',
             title = 'Density'
    )
})
    
    # Make histogram of means
    
    histPlt <- reactive({
      #plot needs to be updated whenever any of these change
      input$obsClick
      input$sampleSize
      input$skew
      
      data.frame(x = meanVec
            ) %>%
            ggvis(~x, fill=scheme_colour
            ) %>%
            layer_histograms(
                width = 0.2
            ) %>%
            hide_legend("fill") %>%
            scale_numeric("x", domain = c(-omega*4, omega*4), nice = FALSE) %>%
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
        histPlt() %>% bind_shiny('histPlt')
        
        })
    
#     observe({
#       histPlt
#         histPlt() %>% bind_shiny('histPlt')
#     })
#     
    # Display the current sample mean
    output$currentMean <- renderTable({
        input$obsClick
        input$sampleSize
        data.frame(Mean = round(meanVec,2))
    })
    
})

                     
    
    
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        