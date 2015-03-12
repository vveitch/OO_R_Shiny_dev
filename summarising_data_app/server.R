# server.R

library(BHH2) #dotPlot
library(lattice) #histogram

##See http://shiny.rstudio.com/articles/dynamic-ui.html for advice on dynamic UI

load("data/skeleton.RData")
data<-SkeletonDatacomplete

shinyServer(function(input, output) {
  
  output$dataPlot <- renderPlot({
    switch(input$plotType,
           "box"= if (input$sepVar=="none"){
             boxplot(data[[input$mainVar]],main=input$mainVar)  
           } else {
             boxplot(data[[input$mainVar]]~data[[input$sepVar]], main = paste(input$mainVar," blocked by ",input$sepVar)) 
           },
           "hist" = if (input$sepVar=="none"){
             histogram(data[[input$mainVar]],main=input$mainVar)  
           } else {
             #inputs need to be text so we're obliged to enter the formula in this wacky way
             histogram(as.formula(paste("~ ",input$mainVar,"|", input$sepVar)),
                                  data=data, main = paste(input$mainVar," blocked by ",input$sepVar)) 
           },
           "pie" = pie(table(data[input$mainCatVar])),
           "bar" = barplot(table(data[input$mainCatVar])),
    )
  })
})  

