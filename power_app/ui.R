# ui.R
library(markdown)

shinyUI(navbarPage("Statistical Tests 2",
                   tabPanel("Biased Coin",
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Null hypothesis: p=0.5"),
                                
                                numericInput("sample_size", label = h3("Sample size"), value = 100, min=10, max=1000),
                                
                                sliderInput("effect_size", label = h3("True probability p"), min = 0, 
                                            max = 1, value = 0.6),
                                
                                  selectInput("hA", label = h3("Alternative hypothesis"), 
                                            choices = 
                                              list("p not equal 0.5" = "two.sided", "p<0.5" = "less", "p>0.5" = "greater"), 
                                            selected = ">0.5"),
                                
                                sliderInput("sig_level", 
                                            label = HTML("<h3>Significance level &alpha;:</h3>"),
                                            min = 0, 
                                            max = 0.5, value = 0.05),
                                
                                numericInput("num_trials", label = h3("Number of repeated experiments"), value = 100, min=1, max=1000)
                                ),
                              mainPanel(
                                      verbatimTextOutput("summary"),
                                      plotOutput("dataHist"),
                                      plotOutput("pValHist")
                                
                              )                            
                            )
                   ),
                   tabPanel("Two Sample Comparison",
                            sidebarLayout(
                              sidebarPanel(
                                helpText(HTML("Null hypothesis: p<sub>A</sub> = p<sub>B</sub>")),
                                
                                numericInput("sample_size_A", label = h3("Sample size A"), value = 100, min=10, max=1000),
                        
                                sliderInput("effect_size_A", label = HTML("<h3>True probability p<sub>A</sub></h3>"), min = 0, 
                                            max = 1, value = 0.6),
                                
                                numericInput("sample_size_B", label = h3("Sample size B"), value = 100, min=10, max=1000),
                                
                                sliderInput("effect_size_B", label = HTML("<h3>True probability p<sub>B</sub></h3>"), min = 0, 
                                            max = 1, value = 0.4),
                                
                                
                                selectInput("hA_two_sample", label = h3("Alternative hypothesis"), 
                                            choices = 
                                              list("probabilities not equal" = "two.sided", "A less than B" = "less", "A greater than B" = "greater"), 
                                            selected = ">0.5"),
                                
                                sliderInput("sig_level_two_sample", 
                                            label = HTML("<h3>Significance level &alpha;:</h3>"),
                                            min = 0, 
                                            max = 0.5, value = 0.05),
                                
                                numericInput("num_trials_two_sample", label = h3("Number of repeated experiments"), value = 100, min=1, max=1000)
                                
                              ),
                              mainPanel(
                                verbatimTextOutput("summary_two_sample"),
                                plotOutput("pValHist_two_sample")
                                
                              )
                   )
                   )
))

# shinyUI(fluidPage(
#   titlePanel("Statistical Power"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       helpText("Examine outcomes of binomial experiments."),
#       
#       numericInput("sample_size", label = h3("Sample size"), value = 100, min=10, max=1000),
#       
#       numericInput("num_trials", label = h3("Number of repeated experiments"), value = 100, min=1, max=1000),
#       
#       sliderInput("effect_size", label = h3("True probability p"), min = 0, 
#                   max = 1, value = 0.6),
#       
#       selectInput("hA", label = h3("Alternative hypothesis"), 
#                   choices = 
#                     list("p not equal 0.5" = "two_side", "p<0.5" = "<0.5", "p>0.5" = ">0.5"), 
#                   selected = ">0.5"),
#       
#       sliderInput("sig_level", 
#                   label = HTML("<h3>Significance level &alpha;:</h3>"),
#                   min = 0, 
#                   max = 0.5, value = 0.05)
#       
#     ),
#     
#     mainPanel(
#       verbatimTextOutput("summary"),  
#       plotOutput("dataHist"),
#       plotOutput("pValHist")
#       
#       )
#   )
# ))