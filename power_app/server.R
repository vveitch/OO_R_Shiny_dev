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
  s.samp_pwr <-  reactive(
    {function(n,null_prob,sig_level) {(pwr.p.test(h=ES.h(input$true_prob,null_prob),
                                                                             n=n,
                                                                             sig.level=sig_level,
                                                                             power=NULL,
                                                                             alternative=input$hA))$power}
  });

  
  output$summary <- renderPrint(format_output((s.samp_pwr())
                                              (input$sample_size,input$null_prob,input$sig_level)))
  
  
  output$s.powerPlot <- renderPlot({
    #normal inference only valid for np>10
    smallestN = ceiling(max(10/input$true_prob,10/(1-input$true_prob)));    
    #aesthetic options
    powerPlot <- ggplot(data.frame(x=c(smallestN, smallestN+1000)), aes(x)) + 
      coord_cartesian(ylim = c(0, 1)) + 
      labs(title='Power vs Sample Size',x="Sample Size (n)",
           y=paste("Power (",expression(1-Beta),")")) +
        #for reasons unknown actually having beta render slows things down a whole bunch
           #y=expression(paste("Power (",1-Beta,")"))) +
      theme(plot.title = element_text(size=20, face="bold", vjust=2));
    
    powerPlot <- powerPlot + stat_function(fun=s.samp_pwr(),
                                           arg = list(null_prob=input$null_prob,
                                                      sig_level=input$sig_level),
                                           aes(colour = "Main"));
    
    if (1 %in% input$s.comparisonCheckGroup){
      powerPlot<-powerPlot+stat_function(fun=s.samp_pwr(),
                                          arg = list(null_prob=input$s.c1.null_prob,
                                                     sig_level=input$s.c1.sig_level),
                                         aes(colour = "Comparison 1"));
    }
    if (2 %in% input$s.comparisonCheckGroup){

      powerPlot<-powerPlot+stat_function(fun=s.samp_pwr(),
                                         arg = list(null_prob=input$s.c2.null_prob,
                                                    sig_level=input$s.c2.sig_level),
                                         aes(colour = "Comparison 2"));
    }
    powerPlot+scale_colour_manual("Legend",values=c("Main"="black",
                                                    "Comparison 1"="red",
                                                    "Comparison 2"="blue"))
  })
  
  ###logic for two equal (te) sample proportion test
  
  te.samp_pwr <- reactive({
    function(n,sig_level) {(pwr.2p.test(h=ES.h(input$te.true_prob_A,input$te.true_prob_B),
                              n=n,
                              sig.level=sig_level,
                              power=NULL,
                              alternative=input$te.hA))$power}
  })    
  
  output$te.summary <- renderPrint(format_output((te.samp_pwr())
                                                 (input$te.sample_size,input$te.sig_level)))
  
  
  output$te.powerPlot <- renderPlot({
    #normal inference only valid for np>10
    smallestN = ceiling(max(10/input$te.true_prob_A,10/(1-input$te.true_prob_A),
                            10/input$te.true_prob_B,10/(1-input$te.true_prob_B)));   
    powerPlot <- ggplot(data.frame(x=c(smallestN, smallestN+1000)), aes(x)) + 
      coord_cartesian(ylim = c(0, 1)) + 
      labs(title='Power vs Sample Size',x="Sample Size (n)",
           y=paste("Power (",expression(1-Beta),")")) +
      #for reasons unknown actually having beta render slows things down a whole bunch
      #y=expression(paste("Power (",1-Beta,")"))) +
      theme(plot.title = element_text(size=20, face="bold", vjust=2));
    
    powerPlot <- powerPlot + stat_function(fun=te.samp_pwr(),
                                           arg = list(sig_level=input$sig_level),
                                           aes(colour = "Main"));
    
    if (1 %in% input$te.comparisonCheckGroup){
      powerPlot<-powerPlot+stat_function(fun=te.samp_pwr(),
                                         arg = list(sig_level=input$te.c1.sig_level),
                                         aes(colour = "Comparison 1"));
    }
    if (2 %in% input$te.comparisonCheckGroup){
      
      powerPlot<-powerPlot+stat_function(fun=te.samp_pwr(),
                                         arg = list(sig_level=input$te.c2.sig_level),
                                         aes(colour = "Comparison 2"));
    }
    powerPlot+scale_colour_manual("Legend",values=c("Main"="black",
                                                    "Comparison 1"="red",
                                                    "Comparison 2"="blue"))
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
    power <- vector(mode="double",length=10000);
    power<-vapply(1:10000, function(x) (tu.samp_pwr())(nA[x],nB[x]), 1.0);
    data.frame(nA,nB,power)
  })
  
  output$tu.powerPlot <- renderPlot({
    ggplot(powerdf(), aes(nA,nB, z = power))+
      geom_tile(aes(fill = power)) + stat_contour()
  })
  
}
)    

