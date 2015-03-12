# ui.R

##TODO: to get the data selection thing to work see http://shiny.rstudio.com/articles/dynamic-ui.html for advice on dynamic UI
##eventually I'll probably want custom files corresponding to each dataset giving variable names and whatnot 

#names of categorical variables in Skeleton dataset
catNames = list( "Sex","BMIcat")
#
quantNames = list("BMIquant" ,  "Age"     ,   "DGestimate", "DGerror"  ,  "SBestimate", "SBerror"   )

shinyUI(fluidPage(
  titlePanel("Visualizing Skeleton Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", label = h3("Plot Type"), 
                  choices = 
                    list("box plot" = "box", "histogram" = "hist", "dot plot" = "dot", "pie chart"="pie","bar chart"="bar"), 
                  selected = "box"),
      
      ##plot specific options
      #quantitative variable panel
      conditionalPanel(
        condition = "input.plotType == \"box\" || input.plotType == \"hist\"",
        selectInput("mainVar", "Variable of interest",
                    quantNames),
      
        selectInput("sepVar", "Block by",
                    c("none", catNames))
        
        )
      
    ),
    
    mainPanel(
      
      plotOutput("dataPlot")      

      )
  )
))