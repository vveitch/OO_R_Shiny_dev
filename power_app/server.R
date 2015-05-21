# server.R

source("power_logic.R")

shinyServer(function(input, output) {
  
  output$summary <- renderPrint(summary_table(input$hA,
                                              input$sig_level,
                                              input$sample_size,
                                              input$effect_size,
                                              input$num_trials,
                                              simulateData()))
  
#   output$theoryPower <- renderText({paste("Theoretical power: ", 
#                                                 theory_power(input$hA,
#                                                              input$sig_level,
#                                                              input$sample_size,
#                                                              input$effect_size))})  
#   output$estPower <- renderText({paste("Estimated power: ", 
#                                        sum(pval(input$hA,input$sample_size,simulateData())<input$sig_level)/input$num_trials)
#   })
  
  #I made this reactive as best practice in anticipation of doing other things with it later; 
  #it doesn't matter if we're just calling plots on this
  simulateData <- reactive({
    sim_data(input$num_trials,input$sample_size,input$effect_size)
  })
  
  output$dataHist <- renderPlot({
    hist(simulateData(),breaks=20, 
         main="Number of Successes per Experiment",
         xlab=paste("Number of success in ",input$sample_size," attempts"),
         right=FALSE)
  })

  output$pValHist <- renderPlot({
    hist(pval(input$hA,input$sample_size,simulateData()),
         breaks=(0:40/40), #chosen to look reasonably good and force domain to be 0..1
         main="Observed p-values",
         xlab="Observed p-value",
          right=FALSE)
  })
  
  
}
)    

