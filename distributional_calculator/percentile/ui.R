library(grid)
shinyUI(pageWithSidebar(
  headerPanel('Find a Given Percentile'),
  
  sidebarPanel(
    radioButtons("randist", "Choose the probability distribution:",
                 list("uniform" = "uniform",
                      "binomial" = "binomial", 
                      "normal" = "normal", 
                      "Poisson" = "Poisson", 
                      "geometric" = "geometric", 
                      "exponential" = "exponential"
)),
    br(),

    conditionalPanel(condition = "input.randist =='uniform'",
  helpText("Uniform Distribution Range"),
  numericInput("min", "Minimum value", 0),
  numericInput("max", "Maximum value", 1)
  ),

    conditionalPanel(condition = "input.randist =='binomial'",
  helpText("Binomial Distribution Parameters"),
  numericInput("size", "Number of Trials", 1),
  numericInput("prob", "Probability of success", .5)
  ),

    conditionalPanel(condition = "input.randist =='normal'",
  helpText("Normal Distribution Parameters"),
  numericInput("mu", "Mean", 0),
  numericInput("sigma", "Standard Deviation", 1)
  ),

    conditionalPanel(condition = "input.randist =='Poisson'",
  helpText("Poisson Distribution Parameter"),
  numericInput("lambda", "Mean", 1)
  ),

    conditionalPanel(condition = "input.randist =='geometric'",
  helpText("Geometric Distribution Parameter"),
  numericInput("gprob", "Probability", 0.5)
  ),

    conditionalPanel(condition = "input.randist =='exponential'",
  helpText("Exponential Distribution Parameter"),
  numericInput("rate", "Rate", 1)
  ),

    numericInput("x", "Enter the required percentile",  0.5), 
      
  
    radioButtons("plotit", "Plot the graph",
                 list("Yes" = "graph",
                      "No" = "nograph"))

  ),

  
 mainPanel(

  h4("The percentile is:"),

  verbatimTextOutput("percentile"), 

 div(class="span6",plotOutput("graph", width = "500px", height = "500px",
      br(),downloadButton('png2','Printer-friendly Version')))

)
   
  ))
