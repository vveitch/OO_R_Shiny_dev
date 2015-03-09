# server.R

source("power_logic.R")

shinyServer(function(input, output) {
  
  #notes: make the data re-simulation reactive 
  
  output$theoryPower <- renderText({paste("Theoretical power: ", 
                                                theory_power(input$hA,
                                                             input$sig_level,
                                                             input$sample_size,
                                                             input$effect_size))})  
  output$estPower <- renderText({paste("Estimated power: ", 
                                       sum(pval(input$hA,input$sample_size,simulateData())<input$sig_level)/input$num_trials)
  })
  
  #I made this reactive as best practice in anticipation of doing other things with it later; 
  #it doesn't matter if we're just calling plots on this
  simulateData <- reactive({
    sim_data(input$num_trials,input$sample_size,input$effect_size)
  })
  
  output$dataHist <- renderPlot({
    hist(simulateData(),breaks=20, 
         main="Histogram of number of successes per trial",
         xlab=paste("Number of success in ",input$sample_size," attempts"))
  })

  output$pValHist <- renderPlot({
    #breaks computed such that the first bin contains all the rejections of the null
    hist(pval(input$hA,input$sample_size,simulateData()),
         breaks=(0:ceiling(1/input$sig_level)/ceiling(1/input$sig_level)),
         main="Histogram of observed p-values",
         xlab="Observed p-value")
  })
  
  
}
)    

