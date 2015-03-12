##Helper functions for visualization app
##Victor Veitch
##03/11/2015

#for now these should just be run by the programmer to make the relevant lists for the app, 
#but they'll hopefully be useful if/when I get around to allowing data upload

#returns the names of all the categorical variables (factors) in a dataframe
factorNames <- function(data){
  names(data)[sapply(names(data), function(n) class(data[[n]])=="factor")]
} 

#returns the names of all the quantitative variables (non-factors) in a dataframe
nonfactorNames <- function(data){
  names(data)[sapply(names(data), function(n) class(data[[n]])!="factor")]
} 