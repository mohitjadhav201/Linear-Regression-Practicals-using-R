rm(list=ls(all=TRUE))
Data=read.table("C:\\Users\\mohitjadhav\\Documents\\AutoInsurance.txt",header=T,dec=".")
Data
attach(Data)
Y=Data[,2];Y

X=Data[,1];X

plot(X,Y,title('X vs Y'),xlab = 'swapnil',ylab='morkhade')
scatter.smooth(Y,X)
linearMod=lm(Y~X)
print(linearMod)
summary(linearMod)
anova(linearMod)
Xbar=mean(X)
Ybar=mean(Y)
SXX=sum((X-Xbar)^2)
SXY=sum((X-Xbar)*(Y-Ybar))
SXY
beta1=SXY/SXX
beta0=Ybar-beta1*(Xbar)
Yhat=beta0+beta1*X
Yhat
resid=Y-Yhat
round(sum(resid)) ##sum of residuals is zero
plot(resid)  ##Residual plot
qqnorm(resid)
install.packages('nortest')
library(nortest)
ad.test(resid)
SSRes=sum(resid^2)
SSRes
MSRes=SSRes/(62)
SST=sum((Y-Ybar)^2)
SST
var(resid)  ###check whether MSRes=var(resid)
Rsquared=(1-(SSRes/SST))
Rsquared
adRsquared=(1-(SSRes/62)/(SST/63)) ##adjusted R squared


            
