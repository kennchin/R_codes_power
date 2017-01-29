#power analysis with y-axis as sample size and x-axis as pooled standard deviation
#assumption: alpha=0.05, power=0.80, two-sided tail


install.packages("pwr")
library("pwr")

#pooled standard deviation, which is the square root of the average of the two standard deviations
sigma1=sigma2=96.7
s_pooled = sqrt((sigma1^2+sigma2^2)/2)
#equivalent
sp = sqrt((((n-1)*sigma1^2+(m-1)*sigma2^2)/(n+m-2)))



#creating table for equal sd vs sample size keeping effect size=12
ptab<-cbind(NULL, NULL)       # initalize ptab

 for (i in seq(from=10,to=100,by=5)){
   pwrt<-pwr.t.test(d=(0-12)/i,power=.8,sig.level=.05,type="two.sample",alternative="two.sided")
   ptab<-rbind(ptab, cbind(i, pwrt$n))
  
 }

ptab


plot(ptab[,1],ptab[,2],type="b",xlab="standard deviation",ylab="sample size")


#creating table for equal sd vs sample size keeping effect size=24
ptab2<-cbind(NULL, NULL)       # initalize ptab

 for (i in seq(from=10,to=100,by=5)){
   pwrt<-pwr.t.test(d=(0-24)/i,power=.8,sig.level=.05,type="two.sample",alternative="two.sided")
   ptab2<-rbind(ptab2, cbind(i, pwrt$n))
  
 }

ptab2


plot(ptab[,1],ptab[,2],type="b",xlab="standard deviation",ylab="sample size",col="red",axes=F)
lines(ptab2[,1],ptab2[,2],type="b",col="green")

title("Sample Size Estimation for t-test Studies\n
Sig=0.05 (Two-tailed)") legend("topleft", title="Effect Size",c("12 hour","24 hour"),lwd=c(1,1),col=c("red","green"))
axis(side=1,at=seq(0,100,5),lwd.ticks=2,mgp=c(0,0.5,0))
axis(side=2,at=seq(0,1200,50),lwd.ticks=2,mgp=c(0,0.5,0))

library(calibrate)
textxy(ptab[,1],ptab[,2],trunc(ptab[,2]))
textxy(ptab2[,1],ptab2[,2],trunc(ptab2[,2]))
