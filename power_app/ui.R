# ui.R

shinyUI(fluidPage(
  titlePanel("Power and Hypothesis Testing"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Examine outcomes of binomial experiments."),
      
      numericInput("sample_size", label = h3("Sample size"), value = 100),
     
      numericInput("num_trials", label = h3("Number of trials"), value = 1),
            
      sliderInput("effect_size", label = h3("True probability p"), min = 0, 
                  max = 1, value = 0.5),
      
      selectInput("hA", label = h3("Alternative hypothesis"), 
                  choices = 
                    list("p not equal 0.5" = "two_side", "p<0.5" = "<0.5", "p>0.5" = ">0.5"), 
                  selected = 1),
      
      sliderInput("sig_level", 
                  label = HTML("<h3>Significance level &alpha;:</h3>"),
                  min = 0, 
                  max = 0.5, value = 0.05)
      
    ),
    
    mainPanel(plotOutput("map"))
  )
))