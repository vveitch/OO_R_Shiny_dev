# server.R

library(ggplot2)
library(lattice)

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
  
  #extends summary to give mean and s.d. when applied to a numeric input and relative frequencies when applied to categorical input
  data_summary <- function(input_vec){
    if (is.numeric(input_vec)) {
      mean_stuff <- c(mean(input_vec),sd(input_vec));
      names(mean_stuff)<-c("Mean","s.d.");
      as.table(c(summary(input_vec),mean_stuff))
    } else {
      tabby <- rbind(table(input_vec),
                     table(input_vec)/sum(table(input_vec)));
      rownames(tabby)<-c("Counts","Rel. Freqs.");
      tabby
    }
  }
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    if (input$sepVar=="none"){
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


