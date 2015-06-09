# ui.R

#need to include includeCSS("www/isim_header.css") in the fluidpage environment
isimHeaderPanel <- function(appTitle,windowTitle=appTitle){
  tags$div(class="header",HTML(paste("<div class=\"text\"><h1>",as.character(appTitle),"</h1></div><div class=\"image\"></div>")))
}

##TODO: to get the data selection thing to work see http://shiny.rstudio.com/articles/dynamic-ui.html for advice on dynamic UI
##eventually I'll probably want custom files corresponding to each dataset giving variable names and whatnot 

#names of categorical variables in Skeleton dataset
catNames = list( "Sex","BMIcat")
#names of quantitative variables in Skeleton dataset
quantNames = list("BMIquant" ,  "Age"     ,   "DGestimate", "DGerror"  ,  "SBestimate", "SBerror")

shinyUI(fluidPage(
  theme="bootswatch_cerulean.css",
  includeCSS("www/isim_header.css"),
  isimHeaderPanel("Visualizing the Skeleton Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", label = h3("Plot Type"), 
                  choices = 
                    list("box plot" = "box", "histogram" = "hist", "dot plot" = "dot", "pie chart"="pie","bar chart"="bar"), 
                  selected = "box"),
      
      ##plot specific options
      #quantitative variable panel
      conditionalPanel(
        condition = "input.plotType == \"box\" || input.plotType == \"hist\" || input.plotType == \"dot\"",
        selectInput("mainVar", "Variable of interest",
                    quantNames),
        
        selectInput("sepVar", "Subgroup",
                    c("none", catNames))
      ),
      
      
      conditionalPanel(
        condition = "input.plotType == \"pie\" || input.plotType == \"bar\"",
        selectInput("mainCatVar", "Variable of interest",
                    catNames)
      )
      
    ),
    
    mainPanel(

      plotOutput("dataPlot"),    
      
      h3("Data Summary:"),
      verbatimTextOutput("summary")  
    )
  )
))