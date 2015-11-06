#Victor Veitch
#19/10/2015
#adapted from code by Tyler Hunt, http://psychoanalytix.com

library(sn)
library(ggplot2)
rm(list = ls())

#cerulean
scheme_colour <- "#2fa4e7"

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

  test_intervals<-reactive({
    input$obsClick
    interval_gen(input$nsamp,input$mean,input$variance,input$skew,(input$conf.level/100),input$numTrials)
    })
  
  output$conf.plot<-renderPlot({   
    
    gen2(input$nsamp,input$mean,(input$conf.level/100),test_intervals())
    #generator takes standard deviation as its input, but Tyler's app didn't take the sqrt... possible source of bugs here?
    #generator(input$nsamp, input$mean, sqrt(input$variance), (input$conf.level/100))
  })
  
  output$samp_dist <- renderPlot({
    samp_dist_plot<-ggplot(data.frame(x=c(-10, 10)), aes(x)) + 
      coord_cartesian(ylim = c(0, 0.5)) + 
      labs(title='Distribution of Observations') +
      theme(plot.title = element_text(size=20, face="bold", vjust=2),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()
            );
    
    #skew normal parameters deduced from mean and variance
    #formulas from https://en.wikipedia.org/wiki/Skew_normal_distribution
    delta=input$skew/sqrt(1+(input$skew)^2);
    omega=sqrt(input$variance/(1-2*delta^2/pi))
    xi = input$mean - omega*delta*sqrt(2/pi)  
    
    samp_dist_plot <- samp_dist_plot + stat_function(fun=dsn,
                                           arg = list(xi=xi,
                                                      omega=omega,
                                                      alpha=input$skew,
                                                      tau=0)
                                        #   ,colour=scheme_colour
                                           );
    
    samp_dist_plot

    })
  
  output$hitProp <- renderPrint(cat(paste("Proportion of CIs containing true mean: ",hit_counter(input$mean,test_intervals())/input$numTrials)))
  
})

