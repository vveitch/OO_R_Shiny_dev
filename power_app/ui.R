# ui.R

#need to include includeCSS("www/isim_header.css") in the fluidpage environment
isimHeaderPanel <- function(appTitle,windowTitle=appTitle){
  tags$div(class="header",HTML(paste("<div class=\"text\"><h1>",as.character(appTitle),"</h1></div><div class=\"image\"></div>")))
}

shinyUI(
  fluidPage(
    theme="bootswatch_cerulean.css",
    includeCSS("www/isim_header.css"),
    isimHeaderPanel("Statistical Testing II"),
      
  navbarPage("Proportions",
    #title=div(img(src="isim_header_logo_no_padding.svg"), "My Title in the Navbar"),
                   tabPanel("Single Sample",
                            sidebarLayout(
                              sidebarPanel(
                                
                                numericInput("sample_size", label = h3("Sample size"), value = 100, min=10, max=1000),
                                
                                sliderInput("true_prob", label = h3("True probability p"), step=0.005,
                                            min = 0, max = 1, value = 0.6),
                                
                                sliderInput("null_prob", label = HTML("<h3>Null hypothesis p<sub>0</sub></h3>"), step=0.005,
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
                                
                                sliderInput("te.true_prob_A", label = HTML("<h3>True probability p<sub>A</sub></h3>"), step=0.005,
                                            min = 0, max = 1, value = 0.6),
                                
                                sliderInput("te.true_prob_B", label = HTML("<h3>True probability p<sub>B</sub></h3>"), step=0.005,
                                            min = 0, max = 1, value = 0.4),
                                
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
                               
                                sliderInput("tu.true_prob_A", label = HTML("<h3>True probability p<sub>A</sub></h3>"), step=0.005,
                                            min = 0, max = 1, value = 0.6),
                                
                                sliderInput("tu.true_prob_B", label = HTML("<h3>True probability p<sub>B</sub></h3>"), step=0.005,
                                            min = 0, max = 1, value = 0.4),
                                
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
)))