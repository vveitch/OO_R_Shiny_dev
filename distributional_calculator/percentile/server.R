#Victor Veitch
#14-07-2015
#modified code from ???? see ????

library(shiny)

library(grid)

shinyServer(function(input, output){

    datainput <- reactive({ 



    x <- input$x

    if (input$randist=="uniform") {

        m <- input$min

        M <- input$max  

        if (is.na(m)) m <- 0

        if (is.na(M)) M <- 1

        percentile <- qunif(x, m, M)

    } 

    if (input$randist=="binomial") {

        m <- input$size

        p <- input$prob 

        if (is.na(m)) m <- 1

        if (is.na(p)) p <- .5

        percentile <- qbinom(x, size=m, prob=p)

    }

    if (input$randist=="normal") {

        mu <- input$mu

        sigma <- input$sigma 

        if (is.na(mu)) mu <- 0

        if (is.na(sigma)) sigma <- 1

        percentile <- qnorm(x, mu, sigma)

    }

    if (input$randist=="Poisson") {

        lambda <- input$lambda

        if (is.na(lambda)) lambda <- 1

        percentile <- qpois(x, lambda)

    }

    if (input$randist=="geometric") {

        gprob <- input$gprob

        if (is.na(gprob)) gprob <- 0.5

        percentile <- qgeom(x, gprob)

    }

    if (input$randist=="exponential") {

        rate <- input$rate

        if (is.na(rate)) rate <- 1

        percentile <- qexp(x, rate)

    }

    percentile

})





output$percentile <- renderPrint({

    value <- datainput()

    value

})







output$graph <- renderPlot({

    if (input$plotit=="graph") {



         pushViewport(viewport(width=.925, height=.85))



        if (input$randist=="uniform") {

            ran <- input$max-input$min

            lower <- input$min-.5*ran

            upper <- input$max+.5*ran

            cutoff <- datainput()

            gridShadedCurve(dunif(x, input$min, input$max), 

                from=lower, to=upper, fromfill=lower, tofill=cutoff, 

                ylab="probability density", main="True Distribution")

            grid.text(x=unit(cutoff, "native"), y=-.075, paste("x =", cutoff))

        } 

        if (input$randist=="binomial") {

            x <- dbinom(0:input$size, input$size, input$prob)

            names(x) <- 0:input$size

            cutoff <- datainput()

            colour <- 2 - ((0:input$size) > cutoff) 

            barplot(x, ylab="Frequency", main="True Distribution", col=colour)

        }

        if (input$randist=="normal") {

            lower <- input$mu - 4*input$sigma

            upper <- input$mu + 4*input$sigma

            cutoff <- datainput()

            gridShadedCurve(dnorm(x, input$mu, input$sigma), 

                from=lower, to=upper, fromfill=lower, tofill=cutoff, 

                ylab="probability density", main="True Distribution")

            grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ",cutoff))

        } 

        if (input$randist=="Poisson") {

            lower <- as.integer(max(0, input$lambda - 4*sqrt(input$lambda)))

            upper <- as.integer(input$lambda + 5*sqrt(input$lambda))

            x <- dpois(lower:upper, lambda=input$lambda)

            names(x) <- lower:upper

            cutoff <- datainput()

            colour <- 2- ((lower:upper) > cutoff) 

            barplot(x, ylab="Frequency", main="True Distribution", col=colour)

        } 

        if (input$randist=="geometric") {

            lower <- 0

            prob <- input$gprob

            upper <- as.integer(log(.001/prob)/log(1-prob))

            x <- dgeom(lower:upper, prob)

            names(x) <- lower:upper

            cutoff <- datainput()

            colour <- 2- ((lower:upper) > cutoff) 

            barplot(x, ylab="Frequency", main="True Distribution", col=colour)



        } 

        if (input$randist=="exponential") {

            lower <- -.001

            upper <- 1/input$rate + 5/input$rate

            cutoff <- input$x

            gridShadedCurve(dexp(x, input$rate), 

                from=lower, to=upper, fromfill=lower, tofill=cutoff, 

                ylab="probability density", main="True Distribution")

            grid.text(x=unit(cutoff, "native"), y=-.075, paste("x = ", cutoff))

        } 

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

          y=unit(c(0,yfill,0),"native"), gp = gpar(fill="red", ...))



}



