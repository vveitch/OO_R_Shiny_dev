# ui.R

#need to include includeCSS("www/isim_header.css") in the fluidpage environment
isimHeaderPanel <- function(appTitle,windowTitle=appTitle){
  tags$div(class="header",HTML(paste("<div class=\"text\"><h1>",as.character(appTitle),"</h1></div><div class=\"image\"></div>")))
}

shinyUI(
  fluidPage(
    theme="bootswatch_cerulean.css",
    includeCSS("www/isim_header.css"),
    isimHeaderPanel("Distributional Calculator"),
    
    navbarPage("Plot Type",
               tabPanel("Percentile",
                        sidebarLayout(
                          sidebarPanel(

                            ##original included more distributions, just uncomment this and comment other list to get that to work again
#                             radioButtons("p_randist", "Choose the probability distribution:",
#                                          
#                                          list("continuous uniform" = "uniform",
#                                               
#                                               "binomial" = "binomial", 
#                                               
#                                               "normal" = "normal", 
#                                               
#                                               "t-distribution" = "t",
#                                               
#                                               "Poisson" = "Poisson", 
#                                               
#                                               "geometric" = "geometric", 
#                                               
#                                               "exponential" = "exponential"
#                                               
#                                          )),
                            
                            radioButtons("p_randist", "Choose the probability distribution:",
                                         
                                         list("continuous uniform" = "uniform",
                                              
                                              "binomial" = "binomial", 
                                              
                                              "normal" = "normal", 
                                              
                                              "t-distribution" = "t"
                                              
                                         )),
                            
                            br(),
                            
                            
                            
                            conditionalPanel(condition = "input.p_randist =='uniform'",
                                             
                                             helpText("Uniform Distribution Range"),
                                             
                                             numericInput("p_min", "Minimum value", 0),
                                             
                                             numericInput("p_max", "Maximum value", 1)
                                             
                            ),
                            
                            
                            
                            conditionalPanel(condition = "input.p_randist =='binomial'",
                                             
                                             helpText("Binomial Distribution Parameters"),
                                             
                                             numericInput("p_size", "Number of Trials", 1),
                                             
                                             numericInput("p_prob", "Probability of success", .5)
                                             
                            ),
                            
                            
                            
                            conditionalPanel(condition = "input.p_randist =='normal'",
                                             
                                             helpText("Normal Distribution Parameters"),
                                             
                                             numericInput("p_mu", "Mean", 0),
                                             
                                             numericInput("p_sigma", "Standard Deviation", 1)
                                             
                            ),
                            
                            conditionalPanel(condition = "input.p_randist =='t'",
                                             
                                             helpText("t-Distribution Parameters"),
                                             
                                             numericInput("p_df", "Degrees of Freedom", 10)
                                             
                            ),
                            
                            
                            conditionalPanel(condition = "input.p_randist =='Poisson'",
                                             
                                             helpText("Poisson Distribution Parameter"),
                                             
                                             numericInput("p_lambda", "Mean", 1)
                                             
                            ),
                            
                            
                            
                            conditionalPanel(condition = "input.p_randist =='geometric'",
                                             
                                             helpText("Geometric Distribution Parameter"),
                                             
                                             numericInput("p_gprob", "Probability", 0.5)
                                             
                            ),
                            
                            
                            
                            conditionalPanel(condition = "input.p_randist =='exponential'",
                                             
                                             helpText("Exponential Distribution Parameter"),
                                             
                                             numericInput("p_rate", "Rate", 1)
                                             
                            ),
                            
                            
                            
                            numericInput("p_x", "Enter the required percentile",  0.5),
                            
                            helpText(   a("Adapted from John Braun",     
                                          href="http://www.stats.uwo.ca/faculty/braun/RTricks/RTricks.php", 
                                          target="_blank",
                                          style="font-size:8pt"))
                            
                          
                            
                            ),
                          
                          
                          
                          
                          
                          mainPanel(
                            
                            
                            
                            h4("The percentile is:"),
                            
                            
                            
                            verbatimTextOutput("percentile"), 
                            
                            
                            
                            div(class="span6",plotOutput("percentile_graph", width = "500px", height = "500px",
                                                         
                                                         br(),downloadButton('png2','Printer-friendly Version')))
                            
                            
                            
                          )
                          
                        )
               ),
               tabPanel("Quantile",
                        sidebarLayout(
                          sidebarPanel(
                          
                            ##to enable extra distributions uncomment this and comment the list that follows
#                             radioButtons("q_randist", "Choose the probability distribution:",
#                                          
#                                          list("continuous uniform" = "uniform",
#                                               
#                                               "binomial" = "binomial", 
#                                               
#                                               "normal" = "normal", 
#                                               
#                                               "t-distribution"="t",
#                                               
#                                               "Poisson" = "Poisson", 
#                                               
#                                               "geometric" = "geometric", 
#                                               
#                                               "exponential" = "exponential"
#                                               
#                                          )),
                            
                            radioButtons("q_randist", "Choose the probability distribution:",
                                         
                                         list("continuous uniform" = "uniform",
                                              
                                              "binomial" = "binomial", 
                                              
                                              "normal" = "normal", 
                                              
                                              "t-distribution"="t"
                                              
                                         )),
                            
                            br(),
                            
                            
                            
                            conditionalPanel(condition = "input.q_randist =='uniform'",
                                             
                                             helpText("Uniform Distribution Range"),
                                             
                                             numericInput("q_min", "Minimum value", 0),
                                             
                                             numericInput("q_max", "Maximum value", 1)
                                             
                            ),
                            
                            
                            
                            conditionalPanel(condition = "input.q_randist =='binomial'",
                                             
                                             helpText("Binomial Distribution Parameters"),
                                             
                                             numericInput("q_size", "Number of Trials", 1),
                                             
                                             numericInput("q_prob", "Probability of success", .5)
                                             
                            ),
                            
                            
                            
                            conditionalPanel(condition = "input.q_randist =='normal'",
                                             
                                             helpText("Normal Distribution Parameters"),
                                             
                                             numericInput("q_mu", "Mean", 0),
                                             
                                             numericInput("q_sigma", "Standard Deviation", 1)
                                             
                            ),
                            
                            conditionalPanel(condition = "input.q_randist =='t'",
                                             
                                             helpText("t-Distribution Parameters"),
                                             
                                             numericInput("q_df", "Degrees of Freedom", 10)
                                             
                            ),
                            
                            conditionalPanel(condition = "input.q_randist =='Poisson'",
                                             
                                             helpText("Poisson Distribution Parameter"),
                                             
                                             numericInput("q_lambda", "Mean", 1)
                                             
                            ),
                            
                            
                            
                            conditionalPanel(condition = "input.q_randist =='geometric'",
                                             
                                             helpText("Geometric Distribution Parameter"),
                                             
                                             numericInput("q_gprob", "Probability", 0.5)
                                             
                            ),
                            
                            
                            
                            conditionalPanel(condition = "input.q_randist =='exponential'",
                                             
                                             helpText("Exponential Distribution Parameter"),
                                             
                                             numericInput("q_rate", "Rate", 1)
                                             
                            ),
                            
                            
                            
                            numericInput("q_x", "Enter the value (x):",  0),
                            
                            helpText(   a("Adapted from John Braun",     
                                          href="http://www.stats.uwo.ca/faculty/braun/RTricks/RTricks.php", 
                                          target="_blank",
                                          style="font-size:8pt"))
                            
                            
                            
                            
                          ),
                          
                          
                          
                          
                          
                          mainPanel(
                            
                            
                            
                            h4("Probability less than or equal x:"),
                            
                            
                            
                             verbatimTextOutput("probability"), 
                            
                            
                            
                            div(class="span6",plotOutput("quantile_graph", width = "500px", height = "500px",
                                                         
                                                         br(),downloadButton('png2','Printer-friendly Version')))
                            
                            
                            
                          )
                        )
               )
    ))
)