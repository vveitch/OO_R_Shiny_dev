###Sandbox for summary app
###Victor Veitch
###03/11/2015


rm(list = ls())

#SkeletonDatacomplete <- read.table("~/Documents/OO_R_Shiny_dev/summarising_data_app/data/SkeletonDatacomplete.txt", header=TRUE, quote="\"")
load("data/skeleton.RData")

###Quantitative Summaries
#boxplot options
#TODO: I can do naming automatically via names(SkeletonDatacomplete) yadda yadda yadda
boxplot(SkeletonDatacomplete$Age)
#have a "group by" option
boxplot(SkeletonDatacomplete$Age~SkeletonDatacomplete$Sex,main="Age vs Sex")
boxplot(SkeletonDatacomplete$Age~SkeletonDatacomplete$BMIcat,main="BMI vs Sex")

#histograms
#same basic thing

###Categorical summaries
sexCount <- table(SkeletonDatacomplete$Sex)
bmiCount <- table(SkeletonDatacomplete$BMIcat)

#dotplot
dotPlot(sexCount)

#pie charts
pie(sexCount)

#bar chart
pie(sexCount)
barplot(table(SkeletonDatacomplete$Sex,SkeletonDatacomplete$BMIcat),
        legend=rownames(table(SkeletonDatacomplete$Sex,SkeletonDatacomplete$BMIcat)))
