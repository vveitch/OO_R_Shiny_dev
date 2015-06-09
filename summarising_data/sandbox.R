###Sandbox for summary app
###Victor Veitch
###03/11/2015


rm(list = ls())

load("data/skeleton.RData")
data<-SkeletonDatacomplete

#a modification of print.summaryDefault to format data in a way that looks nice for ISIM app
#this really just means changing the "NA's" label trigger to "Missing"
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
    tabby <- rbind(tabby,
                   round(1000*table(input_vec)/sum(table(input_vec)))/1000);
    rownames(tabby)<-c("Counts","Rel. Freqs.");
    tabby
  }
}

summary_list<-tapply(data[["SBestimate"]],data[["Sex"]],data_summary)
acc<-rbind(summary_list[[1]],summary_list[[2]])
rownames(acc)<-names(summary_list)

isimPrintSummary(akk)




# #SkeletonDatacomplete <- read.table("~/Documents/OO_R_Shiny_dev/summarising_data_app/data/SkeletonDatacomplete.txt", header=TRUE, quote="\"")
# load("data/skeleton.RData")
# 
# ###Quantitative Summaries
# #boxplot options
# #TODO: I can do naming automatically via names(SkeletonDatacomplete) yadda yadda yadda
# boxplot(SkeletonDatacomplete$Age)
# #have a "group by" option
# boxplot(SkeletonDatacomplete$Age~SkeletonDatacomplete$Sex,main="Age vs Sex")
# boxplot(SkeletonDatacomplete$Age~SkeletonDatacomplete$BMIcat,main="BMI vs Sex")
# #ggplot2 version:
# 
# 
# 
# #histograms
# #same basic thing
# 
# ###Categorical summaries
# sexCount <- table(SkeletonDatacomplete$Sex)
# bmiCount <- table(SkeletonDatacomplete$BMIcat)
# 
# #dotplot
# tmpPlot<-ggplot(SkeletonDatacomplete, aes_string("BMIquant"), main="some title") + geom_dotplot(binwidth=0.5, method='histodot')+ylim(0,40)    
# tmpPlot+facet_grid(paste(".~","Sex"))
# 
# #pie charts
# pie(sexCount)
# 
# #bar chart
# pie(sexCount)
# barplot(table(SkeletonDatacomplete$Sex,SkeletonDatacomplete$BMIcat),
#         legend=rownames(table(SkeletonDatacomplete$Sex,SkeletonDatacomplete$BMIcat)))
