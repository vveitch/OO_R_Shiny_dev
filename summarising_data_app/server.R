# server.R

library(BHH2) #dotPlot
library(lattice) #histogram

##See http://shiny.rstudio.com/articles/dynamic-ui.html for advice on dynamic UI

load("data/skeleton.RData")
data<-SkeletonDatacomplete

shinyServer(function(input, output) {
  
  #this is a workaround to needing to get the variable name different depending on 
  #whether we're dealing with quant or cat var (because of how shiny works)
  mainVarName <- function(){
    if (input$plotType== "pie" || input$plotType == "bar"){
      input$mainCatVar
    } else {
      input$mainVar
    }
  }
  
  output$dataPlot <- renderPlot({
    switch(input$plotType,
           "box"= if (input$sepVar=="none"){
             boxplot(data[[mainVarName()]],main=mainVarName())  
           } else {
             boxplot(data[[mainVarName()]]~data[[input$sepVar]], main = paste(mainVarName()," blocked by ",input$sepVar)) 
           },
           "hist" = if (input$sepVar=="none"){
             histogram(data[[mainVarName()]],main=mainVarName())  
           } else {
             #inputs need to be text so we're obliged to enter the formula in this wacky way
             histogram(as.formula(paste("~ ",mainVarName(),"|", input$sepVar)),
                       data=data, main = paste(mainVarName()," blocked by ",input$sepVar)) 
           },
           #TODO: dotsize scaling set to work with largest size on my laptop. This is is stupid; surely there's a better way to do this
           "dot" = if (input$sepVar=="none"){
             ggplot(data, aes_string(mainVarName()), main=mainVarName()) + 
               geom_dotplot(binwidth=1, method='dotdensity',dotsize=10/max(table(round(SkeletonDatacomplete[[mainVarName()]]))))+
              scale_y_continuous(name = "", breaks = NULL)
           }else{
             ggplot(data, aes_string(mainVarName(),fill=input$sepVar), main=paste(mainVarName()," blocked by ",input$sepVar)) + 
               geom_dotplot(stackgroups=TRUE,binwidth=1, method='dotdensity',dotsize=10/max(table(round(SkeletonDatacomplete[[mainVarName()]])))) +  scale_y_continuous(name = "", breaks = NULL)
           },
           
           "pie" = pie(table(data[mainVarName()])),
           "bar" = barplot(table(data[mainVarName()])),
    )
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data[[mainVarName()]])
  })
})  


