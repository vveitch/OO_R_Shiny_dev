interval_gen = function(n,pop.mean,pop.var,skew=0,conf.level,num.trials){
  #skew normal parameters deduced from mean and variance
  #formulas from https://en.wikipedia.org/wiki/Skew_normal_distribution
  delta=skew/sqrt(1+skew^2);
  omega=sqrt(pop.var/(1-2*delta^2/pi))
  xi = pop.mean - omega*delta*sqrt(2/pi)  
  
  intervals=vector("list",num.trials);
  for (i in 1:num.trials){
    x<- rsn(n,xi=xi,omega=omega,alpha=skew,tau=0);
    intervals[[i]] <- (t.test(x,conf.level=conf.level))$conf.int;
  }
  intervals  
}

hit_counter = function(pop.mean,intervals){
#inputs:
  #pop.mean=population mean
  #intervals = list of num.trials interval objects from t.test$conf.inv#output: number of CI's that do not 
#output:
  #number of CI's that do not contain the true population mean
hits=0;
for (i in 1:length(intervals)){
  interval <- intervals[[i]]
  if(pop.mean>interval[1] & pop.mean<interval[2]){
    hits=hits+1;
  }
}
hits
}

gen2 = function(n,pop.mean,conf.lvl,intervals){
#inputs:
  #n=sample size
  #pop.mean=population mean
  #intervals = list of 100 interval objects from t.test$conf.inv
#output:
  #CI plot
  plot(NULL
       #fixed xlimits of app chosen to look good with hardcoded parameter range
       ,xlim = c(-5,5)
       ,ylim = c(0,100)
       ,yaxt = 'n'
       ,xlab = (conf.lvl)
       ,ylab = (n)
       ,main = "Confidence Intervals of 100 Simulated Trials"
  )
  
  abline(v = pop.mean, col = 'black')
  mtext(expression(mu), cex = 2, at = pop.mean)
  
  for (i in 1:100){
    interval <- intervals[[i]]
    
    if(pop.mean>interval[1] & pop.mean<interval[2]){
      lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='black')
    }
    else{
      lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='red' )
    } 
  } 
}

generator = function(n, pop.mean , pop.sd, conf.lvl) {   
  plot(NULL
       #changed app to have fixed xlimits
       ,xlim = c(-3,3)
       ,ylim = c(0,100)
       ,yaxt = 'n'
       ,xlab = (conf.lvl)
       ,ylab = (n)
#        ,main = "Confidence Intervals of 100 Samples"
       )
  
  abline(v = pop.mean, col = 'black')
  mtext(expression(mu), cex = 2, at = pop.mean)
  
  for (i in 1:100){
    x <- rnorm(n, mean = pop.mean, sd = pop.sd)
    test <- t.test(x,conf.level=conf.lvl)
    interval <- test$conf.int
    
    if(pop.mean>interval[1] & pop.mean<interval[2]){
      lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='black')
    }
    else{
      lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='red' )
    } 
  } 
}