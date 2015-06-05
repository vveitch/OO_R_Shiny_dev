##scratch work for power app development

#plotting one sample
one_samp_pwr <- function(n){
  (pwr.p.test(h=0.3,n=n,sig.level=0.05,power=NULL,alternative="two.sided"))$power
}

ggplot(data.frame(x=c(1, 1000)), aes(x)) + stat_function(fun=one_samp_pwr) + 
  labs(title='Power vs Sample Size',x="Sample Size (n)",y=paste("Power (1-", expression(beta),")")) +
  theme(plot.title = element_text(size=20, face="bold", vjust=2))

#plotting two sample with equal sample size
two_samp_pwr <- function(n){
  (pwr.2p.test(h=0.3,n=n,sig.level=0.05,power=NULL,alternative="two.sided"))$power
}

ggplot(data.frame(x=c(1, 1000)), aes(x)) + stat_function(fun=two_samp_pwr)+
  labs(title='Power vs Sample Size',x="Sample Size (n)",y=paste("Power (1-", expression(beta),")")) +
  theme(plot.title = element_text(size=20, face="bold", vjust=2))
