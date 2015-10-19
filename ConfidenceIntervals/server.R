#Victor Veitch
#19/10/2015
#adapted from code by Tyler Hunt, http://psychoanalytix.com

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

  output$conf.plot<-renderPlot({
    
    #generator takes standard deviation as its input, but Tyler's app didn't take the sqrt... possible source of bugs here?
    generator(input$nsamp, input$mean, sqrt(input$variance), (input$conf.level/100))

  })

})

