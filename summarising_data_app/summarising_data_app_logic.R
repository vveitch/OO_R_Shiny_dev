##Logic for use in Summarising Data rshiny app
##Victor Veitch
##06/04/2015

#extends summary to give s.d. when applied to a numeric input and relative frequencies when applied to categorical input
#also renames "NA's" to "missing" and moves it to be listed after s.d.
data_summary <- function(input_vec){
  if (is.numeric(input_vec)) {
    sd_stuff <- round(sd(input_vec[!is.na(input_vec)])*100)/100; #2 decimal places, excludes NA's
    names(sd_stuff)<-"s.d.";
    summ <- round(100*summary(input_vec))/100;
    if ("NA's" %in% names(summ)){
      names(summ)[7] <- "Missing";
      as.table(c(summ[1:6],sd_stuff,summ[7]))
    } else {
      as.table(c(summ,sd_stuff))
    }
  } else {
    tabby <- table(input_vec);
    tabby <- rbind(tabby,
                   round(1000*table(input_vec)/sum(table(input_vec)))/1000);
    rownames(tabby)<-c("Counts","Rel. Freqs.");
    tabby
  }
}