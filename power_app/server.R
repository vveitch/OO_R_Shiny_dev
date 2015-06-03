# server.R
library(pwr)
library(ggplot2)
source("power_logic.R")

nA<-rep(seq(from=2,to=1000,by=10),100)
nB=vector(mode="integer",length=10000);
sequence_track <- seq(from=2,to=1000,by=10);
for (i in 1:100){
  nB[(100*(i-1)+1):(100*i)]=(sequence_track[i]);
}

shinyServer(function(input, output, session) {
  
  ###logic for single (s) sample proportion test
  #this generates the function that relates sample size and power
  #ie. it's a (reactive) closure 
  s.samp_pwr <- reactive({
    function(n) {(pwr.p.test(h=ES.h(input$true_prob,input$null_prob),
                             n=n,
                             sig.level=input$sig_level,
                             power=NULL,
                             alternative=input$hA))$power}
  })    
  
  output$summary <- renderPrint(format_output((s.samp_pwr())(input$sample_size)))
                                      
  
  output$s.powerPlot <- renderPlot({
    ggplot(data.frame(x=c(1, 1000)), aes(x)) + stat_function(fun=s.samp_pwr()) + 
      coord_cartesian(ylim = c(0, 1)) + 
      labs(title='Power vs Sample Size',x="Sample Size (n)",y=paste("Power (1-", expression(beta),")")) +
      theme(plot.title = element_text(size=20, face="bold", vjust=2))
  })
  
  ###logic for two equal (te) sample proportion test

  te.samp_pwr <- reactive({
    function(n) {(pwr.2p.test(h=ES.h(input$te.true_prob_A,input$te.true_prob_B),
                             n=n,
                             sig.level=input$te.sig_level,
                             power=NULL,
                             alternative=input$te.hA))$power}
  })    
  
  output$te.summary <- renderPrint(format_output((te.samp_pwr())(input$te.sample_size)))
  
  
  output$te.powerPlot <- renderPlot({
    ggplot(data.frame(x=c(1, 1000)), aes(x)) + stat_function(fun=te.samp_pwr()) + 
      coord_cartesian(ylim = c(0, 1)) + 
      labs(title='Power vs Sample Size',x="Sample Size (n)",y=paste("Power (1-", expression(beta),")")) +
      theme(plot.title = element_text(size=20, face="bold", vjust=2))
  })
  
  ###logic for two unequal (tu) sample proportion test
  
  tu.samp_pwr <- reactive({
    function(nA,nB) {(pwr.2p2n.test(h=ES.h(input$tu.true_prob_A,input$tu.true_prob_B),
                              n1=nA, n2=nB,
                              sig.level=input$tu.sig_level,
                              power=NULL,
                              alternative=input$tu.hA))$power}
  })    
  
  output$tu.summary <- renderPrint(format_output((tu.samp_pwr())(input$tu.sample_size_A,input$tu.sample_size_B)))
  
  powerdf <- reactive({
    power<-vapply(1:10000, function(x) (tu.samp_pwr())(nA[x],nB[x]), 1)
    data.frame(nA,nB,power)
    })
  
  output$tu.powerPlot <- renderPlot({
    ggplot(powerdf(), aes(nA,nB, z = power))+
     geom_tile(aes(fill = power)) + stat_contour()
  })
  
  }
)    

