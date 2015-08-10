##Code originally by Alex Stringer
##substantive bug fixes and modifications by Victor Veitch
##10/08/2015

library(ggvis)

shinyUI(fluidPage(
    
    titlePanel('Central Limit Theorem'),
    
    sidebarPanel(
        
        actionButton('obsClick',
                     'Generate A Sample'),
        
        sliderInput('sampleSize',
                    'Sample Size',
                    min = 1,
                    max = 100,
                    value = 1,
                    step = 1),
        
        h4('Population mean: 5'),
        
        tableOutput('currentMean')
        
        

    ),
    
    mainPanel(
        
        ggvis::ggvisOutput('basePlt'),
        ggvis::ggvisOutput('histPlt'),
        h4('Current Sample Means: '),
        br(),
        br(),
        br()
        
    )
))