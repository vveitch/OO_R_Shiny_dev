###Logic and helper functions for power app
###Victor Veitch
###03/08/2015


rm(list = ls())

##user inputs for debugging
# sample_size=100;
# num_trials=100;
# p=0.6;
# hA="greater";
# sig_level=0.05;

###logic for biased coin

sim_data <- function(num_trials,sample_size,p){
  rbinom(num_trials,sample_size,as.matrix(rep(p,num_trials)))  
}

#p-values assuming null p=0.5
pval <- function(hA,sample_size,simulate_data){
 
  
    switch(hA,
           "two.sided"= pbinom(0.5*sample_size + abs(simulate_data-0.5*sample_size),sample_size,0.5,lower.tail=F)
           + pbinom(0.5*sample_size - abs(simulate_data-0.5*sample_size),sample_size,0.5,lower.tail=T),
           "less" = pbinom(simulate_data,sample_size,0.5,lower.tail=T),
           "greater" = pbinom(simulate_data,sample_size,0.5,lower.tail=F)
   )
  
#   single_p_val <- function(sim_data){
#     exact<-switch(hA,
#                   "two_side"= binom.test(sim_data,sample_size,alternative="two.sided"),
#                   "<0.5" = binom.test(sim_data,sample_size,alternative="less"),
#                   ">0.5" = binom.test(sim_data,sample_size,alternative="greater"))
#     
#     exact$p.value
#     };
#  
#   sapply(simulate_data, single_p_val)
}





#power 
#qbinom has off-by-one errors, not sure why: this means we need a -1 for lower.tail=T statments
#pbinom with lower.tail=F takes >x, but we want >= : this means we need a -1
#beware: the -1's are in here for totally conceptually different reasons
#for the two sided test we'll assume a symmetric rejection interval (ie. equal mass in lower and upper tail)
theory_reject_prob <- function(hA,sig_level,sample_size,p){
  out <- switch(hA, 
                "two.sided"<- pbinom(qbinom(sig_level/2,sample_size,0.5, lower.tail=T)-1,sample_size,p,lower.tail=T) + 
                    pbinom(qbinom(sig_level/2,sample_size,0.5, lower.tail=F)-1,sample_size,p,lower.tail=F),
                "less" = pbinom(qbinom(sig_level,sample_size,0.5, lower.tail=T)-1,sample_size,p,lower.tail=T),
                "greater" = pbinom(qbinom(sig_level,sample_size,0.5, lower.tail=F)-1,sample_size,p,lower.tail=F)
  )
    round(1000*out)/1000 #3 decimal place display
}

summary_table <- function(hA,sig_level,sample_size,p,num_trials,sim_data){
  output <- rbind(theory_reject_prob(hA, sig_level, sample_size,p),
              sum(pval(hA,sample_size,sim_data)<sig_level)/num_trials);
  colnames(output) <- "Rejection Probability";
  rownames(output) <- c("Theoretical", "Estimated");
  output  
}


###logic for comparisons
#prop.test collapses to the logic I want for the 2 sample case

num_trials=100;
sample_size_A=100;
sample_size_B=81;
p_A=0.6;
p_B=0.4;
hA="greater";
sig_level=0.05;

sim_data_two_sample <- function(num_trials,sample_size_A,sample_size_B,p_A,p_B){
  data.frame(A=rbinom(num_trials,sample_size_A,as.matrix(rep(p_A,num_trials))),
             B=rbinom(num_trials,sample_size_B,as.matrix(rep(p_B,num_trials))))
}

#p-values assuming null pA=pB
pval_two_sample <- function(hA,sample_size_A,sample_size_B,simulate_data){
    single_p_val <- function(sim_data){
      switch(hA,
        "two.sided"= (prop.test(as.numeric(sim_data),c(sample_size_A,sample_size_B),alternative="two.sided"))$p.value,
        "less" = (prop.test(as.numeric(sim_data),c(sample_size_A,sample_size_B),alternative="less"))$p.value,
        "greater" = (prop.test(as.numeric(sim_data),c(sample_size_A,sample_size_B),alternative="greater"))$p.value)
      };
   
    apply(simulate_data, 1, function(x) single_p_val(as.numeric(x)))
}

theory_reject_prob_two_sample <- function(hA,sig_level,sample_size_A,sample_size_B,p_A,p_B){
  #under H0: T_0 ~ N(0,1)
  #under HA: T_0-(p_A-p_B)/sqrt(p_A(1-p_A)/n_A + p_B(1-p_B)/n_B) ~ N(0,1)
  
  #TODO: this is obviously a bit sketchy because the actual test statistic uses an estimate of the variance; the qnorms should be qts 
  #but in that case it would be a real pain to work with the distribution under HA (some kind of skewed t distribution)
  
  #get rejection region from qnorm using N(0,1) and find rejection prob via pnorm with variance and mean determined by true p_A and p_B
  true_mean=(p_A-p_B)/sqrt(p_A*(1-p_A)/sample_size_A + p_B*(1-p_B)/sample_size_B);
  out <- switch(hA, 
                "two.sided"<- pnorm(qnorm(sig_level/2,lower.tail=T),
                                    mean=true_mean,
                                    lower.tail=T) +
                  pnorm(qnorm(sig_level/2,lower.tail=F),
                        mean=true_mean,
                        lower.tail=F), 
                "less" = pnorm(qnorm(sig_level,lower.tail=T),
                               mean=true_mean,
                               lower.tail=T),
                "greater" = pnorm(qnorm(sig_level,lower.tail=F),
                                  mean=true_mean,
                                  lower.tail=F)
  )
  round(1000*out)/1000 #3 decimal place display
}

summary_table_two_sample <- function(hA,sig_level,sample_size_A,sample_size_B,p_A,p_B,num_trials,sim_data){
  output <- rbind(theory_reject_prob_two_sample(hA,sig_level,sample_size_A,sample_size_B,p_A,p_B),
                  sum(pval_two_sample(hA,sample_size_A,sample_size_B,sim_data)<sig_level)/num_trials);
  colnames(output) <- "Rejection Probability";
  rownames(output) <- c("Theoretical", "Estimated");
  output  
}