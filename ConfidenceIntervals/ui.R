#Victor Veitch
#19/10/2015
#adapted from code by Tyler Hunt, http://psychoanalytix.com

require(shiny)
#need to include includeCSS("www/isim_header.css") in the fluidpage environment
isimHeaderPanel <- function(appTitle,windowTitle=appTitle){
  tags$div(class="header",HTML(paste("<div class=\"text\"><h1>",as.character(appTitle),"</h1></div><div class=\"image\"></div>")))
}

shinyUI(fluidPage(
  theme="bootswatch_cerulean.css",
  includeCSS("www/isim_header.css"),
  isimHeaderPanel("Simulating Confidence Intervals"),
  
  sidebarPanel(
    
    #    HTML("<a href='http://psychoanalytix.com'> <img src='Logo_transparent_background.png'></a> ")
        wellPanel(

          sliderInput('nsamp',
                      'Sample Size per Trial',
                      min = 10,
                      max = 200,
                      value = 100,
                      step = 5),

          sliderInput('mean',
                      'Mean',
                      min = -2,
                      max = 2,
                      value = 0,
                      step = 0.25),

          sliderInput('variance',
                      'Variance',
                      min = 1,
                      max = 10,
                      value = 1,
                      step = 1),
          
          sliderInput('skew',
                      'Skew',
                      min = -100,
                      max = 100,
                      value = 0,
                      step = 5),          
          
          
          numericInput(inputId = "conf.level"
                       ,label=strong("Confidence Level")
                       ,value=95
                       ,min=1
                       ,max=99),
          
          sliderInput('numTrials',
                      'Number of Trials',
                      min = 100,
                      max = 1000,
                      value = 100,
                      step = 25),
          
          actionButton('obsClick',
                       'Simulate New Data Set')
    
          ,align="center"),

        wellPanel(
          helpText("This app is a demonstration of t-test confidence intervals. The length of the bars are the size of the intervals. If the interval does not overlap with the population value (the black vertical line) then it is colored red to indicate a type 1 error.")
        )
        ,helpText("App adapted from", a(href="http://psychoanalytix.com", "PsychoAnalytix.com"))
        )
 
      # Show a summary of the dataset and an HTML table with the requested
      # number of observations. Note the use of the h4 function to provide
      # an additional header above each output section.
      ,mainPanel(
        verbatimTextOutput("hitProp"),
        div(class="span6", align="center", plotOutput("conf.plot", width = "90%", height = "600px", 
                                                      
                                                      br(),downloadButton('png2','Printer-friendly Version'))),
  
        div(class="span6", align="center", plotOutput("samp_dist", width = "90%", height = "500px", 
                                                      
                                                      br(),downloadButton('png2','Printer-friendly Version')))
        )
    )
  )




