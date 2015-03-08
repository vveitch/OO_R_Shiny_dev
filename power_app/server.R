# server.R

source("power_logic.R")

shinyServer(function(input, output) {
  
  #notes: make the data re-simulation reactive 
  
    output$theoryPowerOutput <- renderText({paste("Theoretical power: ", 
                                                  theory_power(input$hA,
                                                               input$sig_level,
                                                               input$sample_size,
                                                               input$effect_size))})  
    
  }
)    

