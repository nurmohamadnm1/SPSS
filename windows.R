
if (FALSE) {
#$f = gi "C:\Windows\secret.dll"; $f.CreationTime = $f.LastWriteTime = "2026-03-24"

  ###1
time=c(75,166,110,144,26,118,131,14,49,153,66,36,32,11,11,15,75,85,100)
length(time)
#sort(time)
status=c(rep(1,17),rep(0,2))
dataset=data.frame(time,status)
dataset

#(a)
library(survival)
km.fit=survfit(Surv(time,status==1)~1,data=dataset)
summary(km.fit)

plot(km.fit,col='red',lwd=2,main='Plot of estimated survival curve with 95%
     Confidence belts',col.main='blue',xlab='Survival Time',col.lab='blue',
     xlim=c(0,180),ylab='Survival Probability',col.lab='blue',conf.int=0.95)

#abline(v=45,col=4,lwd=2)
abline(h=0.75,v=26,col=4,lwd=2)

time=km.fit$time[-17]
s=km.fit$surv[-17]
km.fit$time
time
s
x=log(time)
y=log(-log(s))
plot(x,y,xlim=c(1.5,5),ylim=c(-3,2),main="Weibull plot of remission data",
     xlab="log(t)",ylab="log(-log(S(t)))")
abline(lsfit(x,y),col=4,lwd=2)
abline(h=0,col=3,lwd=2)

abline(h=c(0,-3.06),col=c(3,2),lwd=2)
abline(v=4.48,col=4,lwd=2)

#=======================================================
#2
#=======================================================

###2
#MLEs of Weibull parameters under Type-I censoring

# Twenty patients were given a treatment and it was planned to terminate the treatment on 180 weeks. The survival times in weeks of 16 patients by 180 weeks are as follows. 
# 70, 160, 105, 140, 20, 113, 121, 10, 44, 150, 60, 30, 30, 11, 11, 15
# It is assumed that the above data follows a two parameter Weibull distribution.

# (i) Estimate the parameter using ML method
# (ii) Calculate the standard error of the estimates. Construct the 95% confidence intervals of the parameters.
# (iii) Estimate the 75th percentile and also estimate the survival probability after 200 weeks.
# (iv) Comment on your findings. 

x=c(70,160,105,140,20,113,121,10,44,150,60,30,30,11,11,15)
length(x)
quantile(x,.632)
n=20;t0=180;r=16

library(stats4)   #loading package stats4 for mle()

logL=function(a,b){
  term1=-r*log(b)
  term2=b*r*log(a)
  term3=-(b-1)*sum(log(x))
  term4=sum((x/a)^b)
  term5=(n-r)*((t0/a)^b)
  term1+term2+term3+term4+term5  #-log-likelihood function
}

coefs=coef(mle(minuslogl = logL,start = list(a=quantile(x,.632),b=1.5)))
coefs

var=vcov(mle(minuslogl = logL,start = list(a=quantile(x,.632),b=1.5)))
var

diag(var)
sqrt(diag(var))

#95% confidence interval for scale parameter
LCL=coefs[1]-1.96*sqrt(diag(var)[1])
LCL
UCL=coefs[1]+1.96*sqrt(diag(var)[1])
UCL

#95% confidence interval for shape parameter
LCL=coefs[2]-1.96*sqrt(diag(var)[2])
LCL
UCL=coefs[2]+1.96*sqrt(diag(var)[2])
UCL

## Estimated survival probability after time 200 weeks
1-pweibull(200,shape=coefs[2],scale=coefs[1])
## Estimated 60 percentile
qweibull(0.75,shape=coefs[2],scale=coefs[1])



#=======================================================
#3
#=======================================================

##3
#parallel
x=c(rep(1,24),rep(0,24),rep(0,24),rep(1,24),rep(log(0.25),8),rep(log(0.5),8),
    rep(log(1),8),rep(log(0.25),8),rep(log(0.5),8),rep(log(1),8) )
x=matrix(x,48,3)
x
ys1=c(6,6.8,6.2,6.6,6.4,6,6.9,6.3)
ys2=c(9.4,8.8,9.4,9.6,9.8,9.2,10.8,10.6)
ys3=c(12.8,13.6,13.4,13.8,12.8,14,13.2,12.8)
yt1=c(4.9,4.8,4.9,4.8,5.3,5.1,4.9,4.7)
yt2=c(8.2,8.1,8.1,8.2,7.6,8.3,8.2,8.1)
yt3=c(11,11.5,11.4,11.8,11.8,11.4,11.7,11.4)
y=c(ys1,ys2,ys3,yt1,yt2,yt3)
y
b=solve(t(x)%*%x)%*%t(x)%*%y
b
b[1]
M=(b[2]-b[1])/b[3] ##Estimate of log-potency
M
yhat=x%*%b
yhat
s=sum((y-yhat)^2)/45
s
r1=exp(M) ##Estimate of potency
r1
v=s*solve(t(x)%*%x)
v
h=(1/b[3])*c(-1,1,-M)
h
var.M=t(h)%*%v%*%h
var.M
s.e.M=sqrt(var.M)
s.e.M
s.e.r1=sqrt(r1^2*var.M)
s.e.r1
LCL=exp(M-1.96*s.e.M)
LCL
UCL=exp(M+1.96*s.e.M)
UCL



#Slope-ratio
x=c(rep(1,48),rep(0.25,8),rep(0.5,8),rep(1,8),rep(0,24),rep(0,24),
    rep(0.25,8),rep(0.5,8),rep(1,8) )
x=matrix(x,48,3)
x
y
b=solve(t(x)%*%x)%*%t(x)%*%y
b
r2=b[3]/b[2]
r2

yhat=x%*%b
yhat
s=sum((y-yhat)^2)/45
s
v=s*solve(t(x)%*%x)
v
h=(1/b[2])*c(0,-r2,1)
h
var.r2=t(h)%*%v%*%h
var.r2
s.e.r2=sqrt(var.r2)
s.e.r2

LCL=r2-1.96*s.e.r2
LCL
UCL=r2+1.96*s.e.r2
UCL


#=======================================================
#4
#=======================================================
##4
##Practical for odds ratio
n=35840;p11=0.0140;p12=0.1147;p21=0.0088;p22=0.8625
n11=n*0.0140
n12=n*0.1147
n21=n*0.0088
n22=n*0.8625
data <- matrix(c(n11,n12,n21,n22), 
               nrow = 2,byrow = TRUE,
               dimnames = list("Birthweight" = c("<= 2.5 kg", "> 2.5 kg"),
                               "Outcome" = c("Dead", "Alive")))
data
#install.packages("epitools")
library(epitools)

#(i)
odds.ratio<- oddsratio(data)
odds.ratio

#(ii)
chi_test <- chisq.test(data)
chi_test


o=(n11*n22)/(n12*n21)
o

ar=(p11*p22-p12*p21)/((p11+p21)*(p21+p22))
ar
se=sqrt((p12+ar*(p11+p22))/(n*p21))
se

LCL=1-exp(log(1-ar)+1.96*se)
LCL
UCL=1-exp(log(1-ar)-1.96*se)
UCL
}
