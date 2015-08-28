# ui.R

#need to include includeCSS("www/isim_header.css") in the fluidpage environment
isimHeaderPanel <- function(appTitle,windowTitle=appTitle){
  tags$div(class="header",HTML(paste("<div class=\"text\"><h1>",as.character(appTitle),"</h1></div><div class=\"image\"></div>")))
}

shinyUI(
  fluidPage(
    theme="bootswatch_cerulean.css",
    includeCSS("www/isim_header.css"),
    isimHeaderPanel("Investigating Power"),
    
    navbarPage("Proportions",
               tabPanel("Single Sample",
                        sidebarLayout(
                          sidebarPanel(
                            tabsetPanel(
                              tabPanel("Main",
                                       numericInput("sample_size", label = h4("Sample size"), value = 100, min=10, max=1000),
                                       
                                       sliderInput("true_prob", label = h4("True probability p"), step=0.005,
                                                   min = 0, max = 1, value = 0.6),
                                       
                                       sliderInput("null_prob", label = HTML("<h4>Null hypothesis p<sub>0</sub></h4>"), step=0.005,
                                                   min = 0, max = 1, value = 0.5),
                                       
                                       #subscripts done with unicode here because (html) select does not support sub tag - this may hurt browser compatibility
                                       selectInput("hA", label = h4("Alternative hypothesis"), 
                                                   choices = 
                                                     list("p not equal p\u2080" = "two.sided", "p<p\u2080" = "less", "p>p\u2080" = "greater"), 
                                                   selected = ">0.5"),
                                       
                                       sliderInput("sig_level", 
                                                   label = HTML("<h4>Significance level &alpha;</h4>"),
                                                   min = 0, 
                                                   max = 0.5, value = 0.05)
                              ),
                              tabPanel("Comparison Plots",
                                       checkboxGroupInput("s.comparisonCheckGroup", label = h4("Include comparisons"), 
                                                          choices = list("Comparison 1" = 1, "Comparison 2" = 2)),
                                       sliderInput("s.c1.null_prob", label = HTML("<h4>p<sub>0</sub> for comparison 1</h4>"), step=0.005,
                                                   min = 0, max = 1, value = 0.5),
                                       sliderInput("s.c1.sig_level", 
                                                   label = HTML("<h4>&alpha; for comparison 1</h4>"),
                                                   min = 0, 
                                                   max = 0.5, value = 0.05),
                                       
                                       sliderInput("s.c2.null_prob", label = HTML("<h4>p<sub>0</sub> for comparison 2</h4>"), step=0.005,
                                                   min = 0, max = 1, value = 0.5),
                                       sliderInput("s.c2.sig_level", 
                                                   label = HTML("<h4>&alpha; for comparison 2</h4>"),
                                                   min = 0, 
                                                   max = 0.5, value = 0.05)
                              )
                            )),
                          mainPanel(
                            verbatimTextOutput("summary"),
                            
                            div(class="span6", align="center", plotOutput("s.powerPlot", width = "100%", height = "500px", 
                                                         
                                                         br(),downloadButton('png2','Printer-friendly Version')))
                            
#                            plotOutput("s.powerPlot")
                            
                          )                            
                        )
               ),
               tabPanel("Equal Sample Size Comparison",
                        sidebarLayout(
                          sidebarPanel(  
                            tabsetPanel(
                            tabPanel("Main",
                            
                            numericInput("te.sample_size", label = h4("Sample size"), value = 100, min=10, max=1000),
                            
                            sliderInput("te.true_prob_A", label = HTML("<h4>True probability p<sub>A</sub></h4>"), step=0.005,
                                        min = 0, max = 1, value = 0.6),
                            
                            sliderInput("te.true_prob_B", label = HTML("<h4>True probability p<sub>B</sub></h4>"), step=0.005,
                                        min = 0, max = 1, value = 0.4),
                            
                            selectInput("te.hA", label = h4("Alternative hypothesis"), 
                                        choices = 
                                          list("pA not equal pB" = "two.sided", "pA less than pB" = "less", "pA greater than pB" = "greater"), 
                                        selected = "two.sided"),
                            
                            sliderInput("te.sig_level", 
                                        label = HTML("<h4>Significance level &alpha;</h4>"),
                                        min = 0, 
                                        max = 0.5, value = 0.05)
                            
                            ),
                            tabPanel("Comparison Plots",
                                     checkboxGroupInput("te.comparisonCheckGroup", label = h4("Include comparisons"), 
                                                        choices = list("Comparison 1" = 1, "Comparison 2" = 2)),
                                     sliderInput("te.c1.sig_level", 
                                                 label = HTML("<h4>&alpha; for comparison 1</h4>"),
                                                 min = 0, 
                                                 max = 0.5, value = 0.05),
                                     
                                     sliderInput("te.c2.sig_level", 
                                                 label = HTML("<h4>&alpha; for comparison 2</h4>"),
                                                 min = 0, 
                                                 max = 0.5, value = 0.05)
                            )
                            )),
                          mainPanel(
                            verbatimTextOutput("te.summary"),
                            div(class="span6", align="center", plotOutput("te.powerPlot", width = "100%", height = "500px", 
                                                                          
                                                                          br(),downloadButton('png2','Printer-friendly Version')))
                          )
                        )
               ),
               tabPanel("Unequal Sample Size Comparison",
                        sidebarLayout(
                          sidebarPanel(
                            
                            numericInput("tu.sample_size_A", label = h4("Sample size A"), value = 100, min=10, max=1000),
                            
                            numericInput("tu.sample_size_B", label = h4("Sample size B"), value = 100, min=10, max=1000),
                            
                            sliderInput("tu.true_prob_A", label = HTML("<h4>True probability p<sub>A</sub></h4>"), step=0.005,
                                        min = 0, max = 1, value = 0.6),
                            
                            sliderInput("tu.true_prob_B", label = HTML("<h4>True probability p<sub>B</sub></h4>"), step=0.005,
                                        min = 0, max = 1, value = 0.4),
                            
                            selectInput("tu.hA", label = h4("Alternative hypothesis"), 
                                        choices = 
                                          list("pA not equal pB" = "two.sided", "pA less than pB" = "less", "pA greater than pB" = "greater"), 
                                        selected = "two.sided"),
                            
                            sliderInput("tu.sig_level", 
                                        label = HTML("<h4>Significance level &alpha;</h4>"),
                                        min = 0, 
                                        max = 0.5, value = 0.05)
                            
                          ),
                          mainPanel(
                            verbatimTextOutput("tu.summary"),
                            div(class="span6", align="center", plotOutput("tu.powerPlot", width = "100%", height = "500px", 
                                                                          
                                                                          br(),downloadButton('png2','Printer-friendly Version')))
                            

                          )
                        )
               )
    )))