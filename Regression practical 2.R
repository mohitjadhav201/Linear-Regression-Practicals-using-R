rm(list=ls(all=TRUE))
getwd()
data=read.table("C:\\Users\\mohit jadhav\\Documents\\Systolic Blood Pressure1.txt")
#Q1 a)
data
plot(data)
##Comment : Systolic blood presure of a person is positively correlated with age and weight
#Q1 b)
SB=as.numeric(data[-1,1])
SB
Age=as.numeric(data[-1,2])
Age
Wt=as.numeric(data[-1,3])
Wt
Reg=lm(SB~Age+Wt) #fitting linear regression model
Reg
summary(Reg)
anova(Reg)
#Q1 c)##Estimation of Regression Coefficient
S=rep(1,11)
X=cbind(S,Age,Wt)
Y=SB
C=solve(t(X)%*%X)
Betahat=solve(t(X)%*%X)%*%(t(X)%*%Y)
Betahat
#Estimation of 90% and 95% confidence interval for regression coefficient
confint(Reg,level=0.90)
confint(Reg,level=0.95)

Lowerbound=c()
Upperbound=c()
for (i in 1:3)
{
  Lowerbound[i]=Betahat[i]-qt(0.025,8,lower.tail=FALSE)*sqrt(MSRes*C[i,i])
  Upperbound[i]=Betahat[i]+qt(0.025,8,lower.tail=FALSE)*sqrt(MSRes*C[i,i])
}
Lowerbound
Upperbound

Yhat=30.9941+0.8614*Age+0.334859*Wt
Yhat
Y
e=Y-Yhat
e
round(sum(e))
SSRes=sum(e^2)  ##Residual sum of Square
SSRes
MSRes=SSRes/8
MSRes
SSR=t(Betahat)%*%t(X)%*%Y-(sum(Y))^2/11
SSR
MSR=SSR/2
SST=SSR+SSRes
SST
##OR
Ybar=mean(Y)
Xbar=mean(X)
SST=sum((Y-Ybar)^2)
SSR=SST-SSRes
SSR
#Residual analysis
plot(e)
qqnorm(e)
qqline(e)
acf(e)
library(nortest)
ad.test(e)

t=c()
for (i in 1:length(Betahat))
{t[i]=Betahat[i]/(sqrt(MSRes*C[i,i]))
}
t
###Significance of regression
Fval=MSR/MSRes
fv=pf(Fval,2,8,lower.tail=FALSE)
1-fv
