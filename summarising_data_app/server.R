# server.R

library(ggplot2)
library(lattice)
source("summarising_data_app_logic.R")

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
             histogram(data[[mainVarName()]],main=mainVarName(),xlab=paste(mainVarName(),"Value"))  
           } else {
             #inputs need to be text so we're obliged to enter the formula in this whacky way
             histogram(as.formula(paste("~ ",mainVarName(),"|", input$sepVar)),
                       data=data, main = paste(mainVarName()," blocked by ",input$sepVar)) 
           },
           #TODO: dotsize scaling
           "dot" = if (input$sepVar=="none"){
             p<-ggplot(data, aes_string(mainVarName()), main=mainVarName()) + 
               geom_dotplot(binwidth=1, method='histodot',
                            dotsize=10/max(table(round(SkeletonDatacomplete[[mainVarName()]]))),
                            binpositions="all")+
               scale_y_continuous(name = "", breaks = NULL)
             print(p)
           }else{
             p<-ggplot(data, aes_string(mainVarName(),fill=input$sepVar), main=paste(mainVarName()," blocked by ",input$sepVar)) + 
               geom_dotplot(stackgroups=TRUE,binwidth=1, method='histodot',
                            dotsize=10/max(table(round(SkeletonDatacomplete[[mainVarName()]]))),
                            binpositions="all") +
               scale_y_continuous(name = "", breaks = NULL)
            print(p)
             },
           
           "pie" = pie(table(data[mainVarName()])),
           "bar" = barplot(table(data[mainVarName()])),
    )
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    if (input$sepVar=="none" || !(is.numeric(data[[mainVarName()]]))){
      data_summary(data[[mainVarName()]])  
    } 
    #this gives a summary for each level of the selected grouping factor. Awkward coding for rshiny reasons
    else if (is.numeric(data[[mainVarName()]])) {
      summary_list<-tapply(data[[mainVarName()]],data[[input$sepVar]],data_summary);
      acc=summary_list[[1]]
      for (i in 2:length(summary_list)){
        acc<-rbind(acc,summary_list[[i]]);
      }
      rownames(acc)<-names(summary_list);
      acc
      
    }
    
        
  })
})  


