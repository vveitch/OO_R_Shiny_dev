##Victor Veitch
##based on code originally by Alex Stringer
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
  
  #store the simulated data in this expression so the plots know when to update
  #contains:
  #obsVec, the vector of most recent observations
  #meanVec, the vector of sample means
  simData <- reactiveValues()
  
  # Clear the stored means if the sample size or skew changes
  cleared<-observe({
    input$skew
    input$sampleSize
    simData$obsVec <- numeric(0)
    simData$meanVec <- numeric(0)
  })
  
  #When the button is pressed, generate a new sample
  #this also triggers on input$sampleSize and input$skew
  observe({
    input$obsClick
    
     obsVec <- 0
     meanVec <- isolate(simData$meanVec)
    #numTrials is isolated to allow for changes to this parameter without replotting 
    for (i in 1:isolate(input$numTrials)){
       obsVec <- rsn(input$sampleSize,omega=omega,alpha=input$skew,tau=tau) 
       meanVec <- c(meanVec,mean(obsVec))
     }
     simData$obsVec <- obsVec
     simData$meanVec <- meanVec
  })

  output$totalTrials <- renderPrint(cat(paste("Total Number of Trials: ",length(simData$meanVec))))  
  
  
  ### Make the plots ###
  
  # Initialize the density curve
  
  basePlt <- reactive({
    data.frame(x=simData$obsVec, y=rep(0,length(simData$obsVec))) %>%
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
      #verticle line at sample mean. This is a total hack because appropriate functionality does not yet exist in ggvis
      add_data(data = data.frame(x_coords = c(simData$meanVec[length(isolate(simData$meanVec))],simData$meanVec[length(isolate(simData$meanVec))]),
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
    data.frame(x = simData$meanVec
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
    basePlt() %>% bind_shiny('basePlt')
    histPlt() %>% bind_shiny('histPlt')
  })
  
})



















