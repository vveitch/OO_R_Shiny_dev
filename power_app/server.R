# server.R

source("power_logic.R")

shinyServer(function(input, output, session) {
  
  ###biased coin sever stuff
  output$summary <- renderPrint(summary_table(input$hA,
                                              input$sig_level,
                                              input$sample_size,
                                              input$effect_size,
                                              input$num_trials,
                                              simulateData()))
  
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
  
  ###two sample server stuff
  output$summary_two_sample <- renderPrint(summary_table_two_sample(input$hA_two_sample,
                                              input$sig_level_two_sample,
                                              input$sample_size_A,input$sample_size_B,
                                              input$effect_size_A,input$effect_size_B,
                                              input$num_trials_two_sample,
                                              simulateData_two_sample()))

  #I made this reactive as best practice in anticipation of doing other things with it later; 
  #it doesn't matter if we're just calling plots on this
  simulateData_two_sample <- reactive({
    sim_data_two_sample(input$num_trials_two_sample,input$sample_size_A,input$sample_size_B,input$effect_size_A,input$effect_size_B)
  })
  
  output$pValHist_two_sample <- renderPlot({
    hist(pval_two_sample(input$hA_two_sample,input$sample_size_A,input$sample_size_B,simulateData_two_sample()),
         breaks=(0:40/40), #chosen to look reasonably good and force domain to be 0..1
         main="Observed p-values",
         xlab="Observed p-value",
         right=FALSE)
  })
  
}
)    

