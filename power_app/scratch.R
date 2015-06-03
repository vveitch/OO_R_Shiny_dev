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

rm(list = ls())

nA=seq(from=2,to=1000,by=10)
nB=seq(from=2,to=1000,by=10)nB=seq(from=2,to=1000,by=10)

snark <- function(nA,nB) {(pwr.2p2n.test(h=ES.h(0.4,0.6),
                                n1=nA, n2=nB,
                                sig.level=0.05,
                                power=NULL))$power}

frame_producer <- function(x){
  sapply(nA, function(y) snark(x,y))
}

nA<-rep(seq(from=2,to=1000,by=10),100)
nB=vector(mode="integer",length=10000);
sequence_track <- seq(from=2,to=1000,by=10);
for (i in 1:100){
  nB[(100*(i-1)+1):(100*i)]=(sequence_track[i]);
}

power <- vapply(1:10000, function(x) snark(nA[x],nB[x]), 1)

df <- data.frame(nA,nB,power)
v <- ggplot(df, aes(nA,nB, z = power))
v + geom_tile(aes(fill = power)) + stat_contour()