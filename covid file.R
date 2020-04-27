library("fBasics")
library(urca)
library(psych)
library(tseries)
library(vars)
library(fGarch)
library(forecast)

data <- read.csv("covid.csv")
summary(data)
str(data)

ts.plot(data)
pacf(data)
describe(data)
cov<- as.matrix(data$ï..9)

#Ho=unit root
daily_adf=ur.df(cov, type="drift")
summary(daily_adf)

#daily_adf2=ur.df(cov, type="trend")
#summary(daily_adf2)

##KPSS Test Ho=stationary

daily_kpss=ur.kpss(cov, type=c("mu","tau"), lags = "long") #mu= intercept only tau=intercept + trend
summary(daily_kpss)
##Not stationary

arma<-auto.arima(cov)
arma 
##first difference##
fdcov <- diff(cov)
ts.plot(fdcov)
fd2cov<-diff(fdcov)
ts.plot(fd2cov)
##

library(fGarch)

?garchFit
fit1=garchFit(~garch(1,1),include.mean=T,data=fd2cov,trace=F,cond.dist="QMLE") #include mean = include intercept, conditional distribution=QMLE, robust SE)
fit1
summary(fit1) #Under Standardised residuals, for Q(10) and above, dont have serial correlation
ts.plot(fit1@h.t)
#Saving parameters
omega=coef(fit1)[2]
alpha=coef(fit1)[3]
beta=coef(fit1)[4]


##Forecast##
farma<-forecast(arma)
ts.plot(cov)
plot(forecast(arma))
