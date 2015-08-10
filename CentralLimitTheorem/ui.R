##Code originally by Alex Stringer
##substantive bug fixes and modifications by Victor Veitch
##10/08/2015

library(ggvis)

#need to include includeCSS("www/isim_header.css") in the fluidpage environment
isimHeaderPanel <- function(appTitle,windowTitle=appTitle){
  tags$div(class="header",HTML(paste("<div class=\"text\"><h1>",as.character(appTitle),"</h1></div><div class=\"image\"></div>")))
}

shinyUI(fluidPage(
  theme="bootswatch_cerulean.css",
  includeCSS("www/isim_header.css"),
  isimHeaderPanel("Central Limit Theorem"),
  
    sidebarPanel(
        
        actionButton('obsClick',
                     'Generate A Sample'),
        
        sliderInput('sampleSize',
                    'Sample Size',
                    min = 1,
                    max = 100,
                    value = 1,
                    step = 1),

        sliderInput('skew',
                    'Skew',
                    min = -20,
                    max = 20,
                    value = 0,
                    step = 1)
        
        
    ),
    
    mainPanel(
        
        ggvis::ggvisOutput('basePlt'),
        ggvis::ggvisOutput('histPlt')
        
    )
))