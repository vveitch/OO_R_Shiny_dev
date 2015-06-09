##Logic for use in Summarising Data rshiny app
##Victor Veitch
##06/04/2015

#extends summary to give s.d. when applied to a numeric input and relative frequencies when applied to categorical input
#also renames "NA's" to "missing" and moves it to be listed after s.d.
data_summary <- function(input_vec){
  if (is.numeric(input_vec)) {
    sd_stuff <- round(sd(input_vec[!is.na(input_vec)])*100)/100; #2 decimal places, excludes NA's
    names(sd_stuff)<-"s.d.";
    summ <- summary(input_vec);
    if ("NA's" %in% names(summ)){
      #this isn't actually necessary because of how the isimPrint function works, but leaving the name unchanged might lead to confusion later
      names(summ)[7] <- "Missing"; 
      c(summ[1:6],sd_stuff,summ[7])
    } else {
      miss<-0;
      names(miss)="Missing"
      c(summ,sd_stuff,miss)
    }
  } else {
    tabby <- table(input_vec);
    tabby <- rbind(as.character(tabby),
                   round(1000*table(input_vec)/sum(table(input_vec)))/1000);
    rownames(tabby)<-c("Counts","Rel. Freqs.");
    as.table(tabby)
  }
}


#a modification of print.summaryDefault to always right align format data in a way that looks nice for ISIM app
#this really just means changing the "NA's" label trigger to "Missing" and forcing right align
isimPrintSummary<-function (x, ...) 
{
  xx <- if (is.numeric(x) || is.complex(x)) 
    zapsmall(x)
  else x #should never trigger
 if (is.table(x)){
    m <- match("Missing", colnames(xx), 0)
    xx <- cbind(format(xx[,-m]), `Missing` = as.character(xx[,m]))
    isimPrintTable(xx, justify="right")
    invisible(x)
  }
  else{
    class(xx) <- class(x)[-1]
    m <- match("Missing", names(xx), 0)
    #this part of code not modified to account for table structure
    if (inherits(x, "Date") || inherits(x, "POSIXct")) {
      xx <- if (length(a <- attr(x, "NAs"))) 
        c(format(xx), `NA's` = as.character(a))
      else format(xx)
      print(xx, ...)
      return(invisible(x))
    }
    else if (m && !is.character(x)) 
      xx <- c(format(xx[-m]), `Missing` = as.character(xx[m]))
    print.table(xx)
    invisible(x)
  }
}

#this is to get characters genuinely right aligned
isimPrintTable <- function (x, digits = getOption("digits"), quote = FALSE, na.print = "", 
                        zero.print = "0", justify = "none", ...) 
{
  d <- dim(x)
  if (any(d == 0)) {
    cat("< table of extent", paste(d, collapse = " x "), 
        ">\n")
    return(invisible(x))
  }
  xx <- format(unclass(x), digits = digits, justify = justify)
  if (any(ina <- is.na(x))) 
    xx[ina] <- na.print
  if (zero.print != "0" && any(i0 <- !ina & x == 0)) 
    xx[i0] <- zero.print
    print(xx, quote = quote, right = TRUE, ...)
  invisible(x)
}