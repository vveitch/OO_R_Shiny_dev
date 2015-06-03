# ui.R
library(markdown)

shinyUI(navbarPage("Statistical Tests 2",
                   tabPanel("Biased Coin",
                            sidebarLayout(
                              sidebarPanel(
                                
                                numericInput("sample_size", label = h3("Sample size"), value = 100, min=10, max=1000),
                                
                                sliderInput("true_prob", label = h3("True probability p"), min = 0, 
                                            max = 1, value = 0.6),
                                
                                sliderInput("null_prob", label = HTML("<h3>Null hypothesis p<sub>0</sub></h3>"), 
                                            min = 0, max = 1, value = 0.5),
                                
                                selectInput("hA", label = h3("Alternative hypothesis"), 
                                            choices = 
                                              list("p not equal p0" = "two.sided", "p<p0" = "less", "p>p0" = "greater"), 
                                            selected = ">0.5"),
                                
                                sliderInput("sig_level", 
                                            label = HTML("<h3>Significance level &alpha;:</h3>"),
                                            min = 0, 
                                            max = 0.5, value = 0.05)
                              ),
                              mainPanel(
                                verbatimTextOutput("summary"),
                                plotOutput("s.powerPlot")
                                
                              )                            
                            )
                   ),
                   tabPanel("Equal Sample Size Comparison",
                            sidebarLayout(
                              sidebarPanel(
                                
                                numericInput("te.sample_size", label = h3("Sample size"), value = 100, min=10, max=1000),
                                
                                sliderInput("te.true_prob_A", label = HTML("<h3>True probability p<sub>A</sub></h3>"), min = 0, 
                                            max = 1, value = 0.6),
                                
                                sliderInput("te.true_prob_B", label = HTML("<h3>True probability p<sub>B</sub></h3>"), min = 0, 
                                            max = 1, value = 0.4),
                                
                                #                                 sliderInput("te.effect_size", label = HTML("<h3>Null hypothesis effect size e<sub>0</sub></h3>"), 
                                #                                             min = 0, max = 1, value = 0.5),
                                
                                selectInput("te.hA", label = h3("Alternative hypothesis"), 
                                            choices = 
                                              list("pA not equal pB" = "two.sided", "pA less than pB" = "less", "pA greater than pB" = "greater"), 
                                            selected = "two.sided"),
                                
                                sliderInput("te.sig_level", 
                                            label = HTML("<h3>Significance level &alpha;:</h3>"),
                                            min = 0, 
                                            max = 0.5, value = 0.05)
                                
                              ),
                              mainPanel(
                                verbatimTextOutput("te.summary"),
                                plotOutput("te.powerPlot")
                              )
                            )
                   ),
                   tabPanel("Unequal Sample Size Comparison",
                            sidebarLayout(
                              sidebarPanel(
                                
                                numericInput("tu.sample_size_A", label = h3("Sample size A"), value = 100, min=10, max=1000),
                                
                                numericInput("tu.sample_size_B", label = h3("Sample size B"), value = 100, min=10, max=1000),
                               
                                sliderInput("tu.true_prob_A", label = HTML("<h3>True probability p<sub>A</sub></h3>"), min = 0, 
                                            max = 1, value = 0.6),
                                
                                sliderInput("tu.true_prob_B", label = HTML("<h3>True probability p<sub>B</sub></h3>"), min = 0, 
                                            max = 1, value = 0.4),
                                
                                selectInput("tu.hA", label = h3("Alternative hypothesis"), 
                                            choices = 
                                              list("pA not equal pB" = "two.sided", "pA less than pB" = "less", "pA greater than pB" = "greater"), 
                                            selected = "two.sided"),
                                
                                sliderInput("tu.sig_level", 
                                            label = HTML("<h3>Significance level &alpha;:</h3>"),
                                            min = 0, 
                                            max = 0.5, value = 0.05)
                                
                              ),
                              mainPanel(
                                verbatimTextOutput("tu.summary"),
                                 plotOutput("tu.powerPlot")
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