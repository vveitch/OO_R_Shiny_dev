#Victor Veitch
#14-07-2015
#modified code from ???? see ????

library(shiny)

library(grid)

#cerulean
scheme_colour <- "#2fa4e7"

shinyServer(function(input, output){
  

  ###PERCENTILE LOGIC
  percentile_datainput <- reactive({ 
    
    
    
    x <- input$p_x
    
    if (input$p_randist=="uniform") {
      
      m <- input$p_min
      
      M <- input$p_max  
      
      if (is.na(m)) m <- 0
      
      if (is.na(M)) M <- 1
      
      percentile <- qunif(x, m, M)
      
    } 
    
    if (input$p_randist=="binomial") {
      
      m <- input$p_size
      
      p <- input$p_prob 
      
      if (is.na(m)) m <- 1
      
      if (is.na(p)) p <- .5
      
      percentile <- qbinom(x, size=m, prob=p)
      
    }
    
    if (input$p_randist=="normal") {
      
      mu <- input$p_mu
      
      sigma <- input$p_sigma 
      
      if (is.na(mu)) mu <- 0
      
      if (is.na(sigma)) sigma <- 1
      
      percentile <- qnorm(x, mu, sigma)
      
    }

    if (input$p_randist=="t") {
      
      df <- input$p_df
      
      #I really think this is a stupid thing to do, but I put it in for consistency w the other ones -Victor
      if (is.na(df)) df <- 10
      
      percentile <- qt(x, df)
      
    }
    
    if (input$p_randist=="Poisson") {
      
      lambda <- input$p_lambda
      
      if (is.na(lambda)) lambda <- 1
      
      percentile <- qpois(x, lambda)
      
    }
    
    if (input$p_randist=="geometric") {
      
      gprob <- input$p_gprob
      
      if (is.na(gprob)) gprob <- 0.5
      
      percentile <- qgeom(x, gprob)
      
    }
    
    if (input$p_randist=="exponential") {
      
      rate <- input$p_rate
      
      if (is.na(rate)) rate <- 1
      
      percentile <- qexp(x, rate)
      
    }
    
    percentile
    
  })
  
  
  
  
  
  output$percentile <- renderPrint({
    
    value <- percentile_datainput()
    
    value
    
  })
  
  
  
  
  
  
  
  output$percentile_graph <- renderPlot({
    
      
      
      pushViewport(viewport(width=.925, height=.85))
      
      
      
      if (input$p_randist=="uniform") {
        
        ran <- input$p_max-input$p_min
        
        lower <- input$p_min-.5*ran
        
        upper <- input$p_max+.5*ran
        
        cutoff <- percentile_datainput()
        
        #the +- epsilon is a hack to get around different edge behaviours of the fill and the curve drawing
        gridShadedCurve(dunif(x, input$p_min+10^-8, input$p_max-10^-8), 

                        from=lower, to=upper, fromfill=input$p_min, tofill=cutoff, 
  
  ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x =", cutoff))
        
      } 
      
      if (input$p_randist=="binomial") {
        
        x <- dbinom(0:input$p_size, input$p_size, input$p_prob)
        
        names(x) <- 0:input$p_size
        
        quant <- floor(percentile_datainput())
        colour <- c(rep(scheme_colour,quant+1),rep("#ffffff",input$p_size-quant))
        
        barplot(x, ylab="Probability", col=colour)
      }
      
      if (input$p_randist=="normal") {
        
        lower <- input$p_mu - 4*input$p_sigma
        
        upper <- input$p_mu + 4*input$p_sigma
        
        cutoff <- percentile_datainput()
        
        gridShadedCurve(dnorm(x, input$p_mu, input$p_sigma), 
                        
                        from=lower, to=upper, fromfill=lower, tofill=cutoff, 
                        
                        ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ",cutoff))
        
      } 
  
  if (input$p_randist=="t") {
    
    sigma = if (input$p_df > 2) {
      sqrt(input$p_df/(input$p_df-2))
    } else {
      5 #pretty arbitrary
    }
    
    lower <- -4*sigma
    
    upper <- 4*sigma
    
    cutoff <- percentile_datainput()
    
    gridShadedCurve(dt(x, input$p_df), 
                    
                    from=lower, to=upper, fromfill=lower, tofill=cutoff, 
                    
                    ylab="probability density", main="True Distribution")
    
    grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ",cutoff))
    
  } 
  
      if (input$p_randist=="Poisson") {
        
        lower <- as.integer(max(0, input$p_lambda - 4*sqrt(input$p_lambda)))
        
        upper <- as.integer(input$p_lambda + 5*sqrt(input$p_lambda))
        
        x <- dpois(lower:upper, lambda=input$p_lambda)
        
        names(x) <- lower:upper
        
        quant_index <- floor(percentile_datainput()-lower)
        colour <- c(rep(scheme_colour,quant_index+1),rep("#ffffff",upper-lower-quant_index))
        
        barplot(x, ylab="Probability", col=colour)
        
      } 
      
      if (input$p_randist=="geometric") {
        
        lower <- 0
        
        prob <- input$p_gprob
        
        upper <- as.integer(log(.001/prob)/log(1-prob))
        
        x <- dgeom(lower:upper, prob)
        
        names(x) <- lower:upper
        
        quant_index <- floor(percentile_datainput()-lower)
        colour <- c(rep(scheme_colour,quant_index+1),rep("#ffffff",upper-lower-quant_index))
        
        barplot(x, ylab="Probability", col=colour)
        
        
        
      } 
      
      if (input$p_randist=="exponential") {
        
        lower <- -.001
        
        upper <- 1/input$p_rate + 5/input$p_rate
        
        cutoff <- input$p_x
        
        gridShadedCurve(dexp(x, input$p_rate), 
                        
                        from=lower, to=upper, fromfill=lower, tofill=cutoff, 
                        
                        ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ", cutoff))
        
      } 
      
    
    
  })
  
  
  ##QUANTILE LOGIC
  
  quantile_datainput <- reactive({ 
    
    
    
    x <- input$q_x
    
    if (input$q_randist=="uniform") {
      
      m <- input$q_min
      
      M <- input$q_max  
      
      if (is.na(m)) m <- 0
      
      if (is.na(M)) M <- 1
      
      probability <- punif(x, m, M)
      
    } 
    
    if (input$q_randist=="binomial") {
      
      m <- input$q_size
      
      p <- input$q_prob 
      
      if (is.na(m)) m <- 1
      
      if (is.na(p)) p <- .5
      
      probability <- pbinom(x, size=m, prob=p)
      
    }
    
    if (input$q_randist=="normal") {
      
      mu <- input$q_mu
      
      sigma <- input$q_sigma 
      
      if (is.na(mu)) mu <- 0
      
      if (is.na(sigma)) sigma <- 1
      
      probability <- pnorm(x, mu, sigma)
      
    }
    
    if (input$q_randist=="t") {
      
      df <- input$q_df
      
      #I really think this is a stupid thing to do, but I put it in for consistency w the other ones -Victor
      if (is.na(df)) df <- 10
      
      probability <- pt(x, df)
      
    }
    
    
    if (input$q_randist=="Poisson") {
      
      lambda <- input$q_lambda
      
      if (is.na(lambda)) lambda <- 1
      
      probability <- 1 - ppois(x, lambda)
      
    }
    
    if (input$q_randist=="geometric") {
      
      gprob <- input$q_gprob
      
      if (is.na(gprob)) gprob <- 0.5
      
      probability <- pgeom(x, gprob)
      
    }
    
    if (input$q_randist=="exponential") {
      
      rate <- input$q_rate
      
      if (is.na(rate)) rate <- 1
      
      probability <- pexp(x, rate)
      
    }
    
    probability
    
  })
  
  
  
  
  
  output$probability <- renderPrint({
    
    value <- quantile_datainput()
    
    value
    
  })
  
  
  
  
  
  
  
  output$quantile_graph <- renderPlot({
    
    
      
      
      pushViewport(viewport(width=.95, height=.85))
      
      
      
      if (input$q_randist=="uniform") {
        
        ran <- input$q_max-input$q_min
        
        lower <- input$q_min-.5*ran
        
        upper <- input$q_max+.5*ran
        
        cutoff <- input$q_x
        
        gridShadedCurve(dunif(x, input$p_min+10^-8, input$p_max-10^-8), 
                        
                        from=lower, to=upper, fromfill=input$p_min, tofill=cutoff, 
                        
                        ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ", cutoff))
        
      } 
      
      if (input$q_randist=="binomial") {
        
        x <- dbinom(0:input$q_size, input$q_size, input$q_prob)
        
        names(x) <- 0:input$q_size
        
        cutoff <- input$q_x
        
        colour <- c(rep(scheme_colour,input$q_x+1),rep("#ffffff",input$q_size-input$q_x))
        
        barplot(x, ylab="Probability",  col=colour)
        
      }
      
      if (input$q_randist=="normal") {
        
        lower <- input$q_mu - 4*input$q_sigma
        
        upper <- input$q_mu + 4*input$q_sigma
        
        cutoff <- input$q_x
        
        gridShadedCurve(dnorm(x, input$q_mu, input$q_sigma), 
                        
                        from=lower, to=upper, fromfill=lower, tofill=cutoff, 
                        
                        ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ",  cutoff))
        
      } 
      
      if (input$q_randist=="t") {
        
        sigma = if (input$q_df > 2) {
          sqrt(input$q_df/(input$q_df-2))
        } else {
          5 #pretty arbitrary
        }
        
        lower <- -4*sigma
        
        upper <- 4*sigma
        
        cutoff <- input$q_x
        
        gridShadedCurve(dt(x, input$p_df), 
                        
                        from=lower, to=upper, fromfill=lower, tofill=cutoff, 
                        
                        ylab="probability density", main="True Distribution")
        
        grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ",cutoff))
        
      } 
      
      
      if (input$q_randist=="Poisson") {
        
        lower <- as.integer(max(0, input$q_lambda - 4*sqrt(input$q_lambda)))
        
        upper <- as.integer(input$q_lambda + 5*sqrt(input$q_lambda))
        
        x <- dpois(lower:upper, lambda=input$q_lambda)
        
        names(x) <- lower:upper
        
        quant_index <- floor(input$q_x-lower)
        colour <- c(rep(scheme_colour,quant_index+1),rep("#ffffff",upper-lower-quant_index))
        
        
        barplot(x, ylab="Probability",  col=colour)
        
      } 
      
      if (input$q_randist=="geometric") {
        
        lower <- 0
        
        prob <- input$q_gprob
        
        upper <- as.integer(log(.001/prob)/log(1-prob))
        
        x <- dgeom(lower:upper, prob)
        
        names(x) <- lower:upper
        
        cutoff <- input$q_x
        
        quant_index <- floor(input$q_x-lower)
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


