###Logic and helper functions for power app
###Victor Veitch
###03/08/2015


rm(list = ls())

###user inputs
# sample_size=100;
# num_trials=100;
# p=0.6;
# hA=">0.5";
# sig_level=0.05;

###logic
#

sim_data <- function(num_trials,sample_size,p){
  rbinom(num_trials,sample_size,as.matrix(rep(p,num_trials)))  
}

#p-values assuming null p=0.5
pval <- function(hA,sample_size,sim_data){
  switch(hA,
         "two_side"= pbinom(0.5*sample_size + abs(simulate_data-0.5*sample_size),sample_size,0.5,lower.tail=F)
         + pbinom(0.5*sample_size - abs(simulate_data-0.5*sample_size),sample_size,0.5,lower.tail=T),
         "<0.5" = pbinom(0.5*sample_size - abs(simulate_data-0.5*sample_size),sample_size,0.5,lower.tail=T),
         ">0.5" = pbinom(0.5*sample_size + abs(simulate_data-0.5*sample_size),sample_size,0.5,lower.tail=F)
  )
}


#power 
#qbinom has off-by-one errors, not sure why
#for the two sided test we'll assume a symmetric rejection interval (ie. equal mass in lower and upper tail)
theory_power <- function(hA,sig_level,sample_size,p){
  switch(hA,
         "two_side"= pbinom(qbinom(sig_level/2,sample_size,0.5, lower.tail=T)-1,sample_size,p,lower.tail=T)
         +pbinom(qbinom(sig_level/2,sample_size,0.5, lower.tail=T),sample_size,p,lower.tail=T),
         "<0.5" = pbinom(qbinom(sig_level,sample_size,0.5, lower.tail=T)-1,sample_size,p,lower.tail=T),
         ">0.5" = pbinom(qbinom(sig_level,sample_size,0.5, lower.tail=F),sample_size,p,lower.tail=F)
  )
}


# num_rejects=sum(pval<0.05)
# est_power <- num_rejects/num_trials
# 
# hist(simulate_data,breaks=20)
# hist(pval,breaks=(0:ceiling(1/sig_level)/ceiling(1/sig_level)))

#mosaic(as.matrix(c(num_rejects,num_trials-num_rejects)))