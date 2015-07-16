#Victor Veitch
#15-07-2015
#adapted from code by John Braun

library(shiny)

library(grid)

#cerulean
scheme_colour <- "#2fa4e7"

shinyServer(function(input, output){
  

  ###Quantile LOGIC
  quantile_datainput <- reactive({ 
    
    
    
    x <- input$q_x
    
    if (input$q_randist=="uniform") {
      
      m <- input$q_min
      
      M <- input$q_max  
      
      if (is.na(m)) m <- 0
      
      if (is.na(M)) M <- 1
      
      quant <- qunif(x, m, M)
      
    } 
    
    if (input$q_randist=="binomial") {
      
      m <- input$q_size
      
      p <- input$q_prob 
      
      if (is.na(m)) m <- 1
      
      if (is.na(p)) p <- .5
      
      quant <- qbinom(x, size=m, prob=p)
      
    }
    
    if (input$q_randist=="normal") {
      
      mu <- input$q_mu
      
      sigma <- input$q_sigma 
      
      if (is.na(mu)) mu <- 0
      
      if (is.na(sigma)) sigma <- 1
      
      quant <- qnorm(x, mu, sigma)
      
    }

    if (input$q_randist=="t") {
      
      df <- input$q_df
      
      #I really think this is a stupid thing to do, but I put it in for consistency w the other ones -Victor
      if (is.na(df)) df <- 10
      
      quant <- qt(x, df)
      
    }
    
    if (input$q_randist=="Poisson") {
      
      lambda <- input$q_lambda
      
      if (is.na(lambda)) lambda <- 1
      
      quant <- qpois(x, lambda)
      
    }
    
    if (input$q_randist=="geometric") {
      
      gprob <- input$q_gprob
      
      if (is.na(gprob)) gprob <- 0.5
      
      quant <- qgeom(x, gprob)
      
    }
    
    if (input$q_randist=="exponential") {
      
      rate <- input$q_rate
      
      if (is.na(rate)) rate <- 1
      
      quant <- qexp(x, rate)
      
    }
    
    quant
    
  })
  
  
  
  
  
  output$quantile_output <- renderPrint({
    
    value <- quantile_datainput()
#      value <- probability_datainput()

    value
    
  })
  
  
  
  
  
  
  
  output$quantile_graph <- renderPlot({
    
      
      
      pushViewport(viewport(width=.925, height=.85))
      
      
      
      if (input$q_randist=="uniform") {
        
        ran <- input$q_max-input$q_min
        
        lower <- input$q_min-.5*ran
        
        upper <- input$q_max+.5*ran
        
        cutoff <- quantile_datainput()
        
        #the +- epsilon is a hack to get around different edge behaviours of the fill and the curve drawing
        gridShadedCurve(dunif(x, input$q_min+10^-8, input$q_max-10^-8), 

                        from=lower, to=upper, fromfill=input$q_min, tofill=cutoff, 
  
  ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x =", cutoff))
        
      } 
      
      if (input$q_randist=="binomial") {
        
        x <- dbinom(0:input$q_size, input$q_size, input$q_prob)
        
        names(x) <- 0:input$q_size
        
        quant <- floor(quantile_datainput())
        colour <- c(rep(scheme_colour,quant+1),rep("#ffffff",input$q_size-quant))
        
        barplot(x, ylab="Probability", col=colour)
      }
      
      if (input$q_randist=="normal") {
        
        lower <- input$q_mu - 4*input$q_sigma
        
        upper <- input$q_mu + 4*input$q_sigma
        
        cutoff <- quantile_datainput()
        
        gridShadedCurve(dnorm(x, input$q_mu, input$q_sigma), 
                        
                        from=lower, to=upper, fromfill=lower, tofill=cutoff, 
                        
                        ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ",cutoff))
        
      } 
  
  if (input$q_randist=="t") {
    
    sigma = if (input$q_df > 2) {
      sqrt(input$q_df/(input$q_df-2))
    } else {
      5 #pretty arbitrary
    }
    
    lower <- -4*sigma
    
    upper <- 4*sigma
    
    cutoff <- quantile_datainput()
    
    gridShadedCurve(dt(x, input$q_df), 
                    
                    from=lower, to=upper, fromfill=lower, tofill=cutoff, 
                    
                    ylab="probability density", main="True Distribution")
    
    grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ",cutoff))
    
  } 
  
      if (input$q_randist=="Poisson") {
        
        lower <- as.integer(max(0, input$q_lambda - 4*sqrt(input$q_lambda)))
        
        upper <- as.integer(input$q_lambda + 5*sqrt(input$q_lambda))
        
        x <- dpois(lower:upper, lambda=input$q_lambda)
        
        names(x) <- lower:upper
        
        quant_index <- floor(quantile_datainput()-lower)
        colour <- c(rep(scheme_colour,quant_index+1),rep("#ffffff",upper-lower-quant_index))
        
        barplot(x, ylab="Probability", col=colour)
        
      } 
      
      if (input$q_randist=="geometric") {
        
        lower <- 0
        
        prob <- input$q_gprob
        
        upper <- as.integer(log(.001/prob)/log(1-prob))
        
        x <- dgeom(lower:upper, prob)
        
        names(x) <- lower:upper
        
        quant_index <- floor(quantile_datainput()-lower)
        colour <- c(rep(scheme_colour,quant_index+1),rep("#ffffff",upper-lower-quant_index))
        
        barplot(x, ylab="Probability", col=colour)
        
        
        
      } 
      
      if (input$q_randist=="exponential") {
        
        lower <- -.001
        
        upper <- 1/input$q_rate + 5/input$q_rate
        
        cutoff <- input$q_x
        
        gridShadedCurve(dexp(x, input$q_rate), 
                        
                        from=lower, to=upper, fromfill=lower, tofill=cutoff, 
                        
                        ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ", cutoff))
        
      } 
      
    
    
  })
  
  
  ##PROBABILITY LOGIC
  
  probability_datainput <- reactive({ 
    
    
    
    x <- input$pr_x
    
    if (input$pr_randist=="uniform") {
      
      m <- input$pr_min
      
      M <- input$pr_max  
      
      if (is.na(m)) m <- 0
      
      if (is.na(M)) M <- 1
      
      probability <- punif(x, m, M)
      
    } 
    
    if (input$pr_randist=="binomial") {
      
      m <- input$pr_size
      
      p <- input$pr_prob 
      
      if (is.na(m)) m <- 1
      
      if (is.na(p)) p <- .5
      
      probability <- pbinom(x, size=m, prob=p)
      
    }
    
    if (input$pr_randist=="normal") {
      
      mu <- input$pr_mu
      
      sigma <- input$pr_sigma 
      
      if (is.na(mu)) mu <- 0
      
      if (is.na(sigma)) sigma <- 1
      
      probability <- pnorm(x, mu, sigma)
      
    }
    
    if (input$pr_randist=="t") {
      
      df <- input$pr_df
      
      #I really think this is a stupid thing to do, but I put it in for consistency w the other ones -Victor
      if (is.na(df)) df <- 10
      
      probability <- pt(x, df)
      
    }
    
    
    if (input$pr_randist=="Poisson") {
      
      lambda <- input$pr_lambda
      
      if (is.na(lambda)) lambda <- 1
      
      probability <- 1 - ppois(x, lambda)
      
    }
    
    if (input$pr_randist=="geometric") {
      
      gprob <- input$pr_gprob
      
      if (is.na(gprob)) gprob <- 0.5
      
      probability <- pgeom(x, gprob)
      
    }
    
    if (input$pr_randist=="exponential") {
      
      rate <- input$pr_rate
      
      if (is.na(rate)) rate <- 1
      
      probability <- pexp(x, rate)
      
    }
    
    probability
    
  })
  
  
  
  
  
  output$probability <- renderPrint({
    
    value <- probability_datainput()
    
    value
    
  })
  
  
  
  
  
  
  
  output$prob_graph <- renderPlot({
    
    
      
      
      pushViewport(viewport(width=.95, height=.85))
      
      
      
      if (input$pr_randist=="uniform") {
        
        ran <- input$pr_max-input$pr_min
        
        lower <- input$pr_min-.5*ran
        
        upper <- input$pr_max+.5*ran
        
        cutoff <- input$pr_x
        
        gridShadedCurve(dunif(x, input$pr_min+10^-8, input$pr_max-10^-8), 
                        
                        from=lower, to=upper, fromfill=input$pr_min, tofill=cutoff, 
                        
                        ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ", cutoff))
        
      } 
      
      if (input$pr_randist=="binomial") {
        
        x <- dbinom(0:input$pr_size, input$pr_size, input$pr_prob)
        
        names(x) <- 0:input$pr_size
        
        cutoff <- input$pr_x
        
        colour <- c(rep(scheme_colour,input$pr_x+1),rep("#ffffff",input$pr_size-input$pr_x))
        
        barplot(x, ylab="Probability",  col=colour)
        
      }
      
      if (input$pr_randist=="normal") {
        
        lower <- input$pr_mu - 4*input$pr_sigma
        
        upper <- input$pr_mu + 4*input$pr_sigma
        
        cutoff <- input$pr_x
        
        gridShadedCurve(dnorm(x, input$pr_mu, input$pr_sigma), 
                        
                        from=lower, to=upper, fromfill=lower, tofill=cutoff, 
                        
                        ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ",  cutoff))
        
      } 
      
      if (input$pr_randist=="t") {
        
        sigma = if (input$pr_df > 2) {
          sqrt(input$pr_df/(input$pr_df-2))
        } else {
          5 #pretty arbitrary
        }
        
        lower <- -4*sigma
        
        upper <- 4*sigma
        
        cutoff <- input$pr_x
        
        gridShadedCurve(dt(x, input$pr_df), 
                        
                        from=lower, to=upper, fromfill=lower, tofill=cutoff, 
                        
                        ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ",cutoff))
        
      } 
      
      
      if (input$pr_randist=="Poisson") {
        
        lower <- as.integer(max(0, input$pr_lambda - 4*sqrt(input$pr_lambda)))
        
        upper <- as.integer(input$pr_lambda + 5*sqrt(input$pr_lambda))
        
        x <- dpois(lower:upper, lambda=input$pr_lambda)
        
        names(x) <- lower:upper
        
        quant_index <- floor(input$pr_x-lower)
        colour <- c(rep(scheme_colour,quant_index+1),rep("#ffffff",upper-lower-quant_index))
        
        
        barplot(x, ylab="Probability",  col=colour)
        
      } 
      
      if (input$pr_randist=="geometric") {
        
        lower <- 0
        
        prob <- input$pr_gprob
        
        upper <- as.integer(log(.001/prob)/log(1-prob))
        
        x <- dgeom(lower:upper, prob)
        
        names(x) <- lower:upper
        
        cutoff <- input$pr_x
        
        quant_index <- floor(input$pr_x-lower)
        colour <- c(rep(scheme_colour,quant_index+1),rep("#ffffff",upper-lower-quant_index))
        
        barplot(x, ylab="Probability", col=colour)
        
        
        
      } 
      
      if (input$pr_randist=="exponential") {
        
        lower <- -.001
        
        upper <- 1/input$pr_rate + 5/input$pr_rate
        
        cutoff <- input$pr_x
        
        gridShadedCurve(dexp(x, input$pr_rate), 
                        
                        from=lower, to=upper, fromfill=lower, tofill=cutoff, 
                        
                        ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ",  cutoff))
        
      } 
      
    
    
  })
  
    
})



gridShadedCurve <- function (expr, from, to, fromfill=from,
                             
                             tofill=to, n = 101, add = FALSE, lty = 1, ylab = NULL,
                             
                             log = NULL, xlim = NULL, ...)
  
{
  
  
  
  sexpr <- substitute(expr)
  
  if (is.name(sexpr)) {
    
    fcall <- paste(sexpr, "(x)")
    
    expr <- parse(text = fcall)
    
    if (is.null(ylab))
      
      ylab <- fcall
    
  }
  
  else {
    
    if (!(is.call(sexpr) && match("x", all.vars(sexpr), nomatch = 0)))
      
      stop("'expr' must be a function or an expression containing 'x'")
    
    expr <- sexpr
    
    if (is.null(ylab))
      
      ylab <- deparse(sexpr)
    
  }
  
  if (is.null(xlim))
    
    delayedAssign("lims", {
      
      pu <- par("usr")[1:2]
      
      if (par("xlog"))
        
        10^pu
      
      else pu
      
    })
  
  else lims <- xlim
  
  if (missing(from))
    
    from <- lims[1]
  
  if (missing(to))
    
    to <- lims[2]
  
  lg <- if (length(log))
    
    log
  
  else paste(if (add && par("xlog"))
    
    "x", if (add && par("ylog"))
      
      "y", sep = "")
  
  if (length(lg) == 0)
    
    lg <- ""
  
  x <- if (lg != "" && "x" %in% strsplit(lg, NULL)[[1]]) {
    
    if (any(c(from, to) <= 0))
      
      stop("'from' and 'to' must be > 0 with log=\"x\"")
    
    exp(seq(log(from), log(to), length = n))
    
  }
  
  else seq(from, to, length = n)
  
  y <- eval(expr, envir = list(x = x), enclos = parent.frame())
  
  xrange <- range(x)
  
  yrange <- range(y)
  
  vp <- viewport(x=0.5,y=0.5,xscale=xrange,yscale=yrange)
  
  pushViewport(vp)
  
  grid.lines(x=unit(x,"native"), y=unit(y,"native"), gp = gpar(lty = lty, ...))
  
  grid.xaxis()
  
  yfill <- y[x>=fromfill]
  
  xfill <- x[x>=fromfill]
  
  yfill <- yfill[xfill <= tofill]
  
  xfill <- xfill[xfill <= tofill]
  
  m <- length(xfill)
  
  grid.polygon(x=unit(c(xfill[1], xfill, xfill[m]),"native"),
               
#               y=unit(c(0,yfill,0),"native"), gp = gpar(fill="red", ...))
                y=unit(c(0,yfill,0),"native"), gp = gpar(fill=scheme_colour, ...))

  
  
}


