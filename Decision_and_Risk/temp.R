library(tseries) # load the tseries library
library(moments)

library(fGarch)
library(KScorrect)
library(ADGofTest)
library(VineCopula)
library(forecast)
library(zoo)

library(MASS)

# download the prices, from January 3, 2000 until December 26, 2020
# price = get.hist.quote(instrument = "^gspc", start = "2000-01-03",
#                        end = "2020-12-31", quote="Close")

# head(price, 25)
# tail(price, 25)

rm(list = ls())
par(mfrow = c(1, 1))


# Question (a)

options("getSymbols.warning4.0"=FALSE)

SP500 <- get.hist.quote(instrument = "^GSPC", start = "2000-01-03",
                        end = "2020-12-26", compression = "w", quote="Close")

FTSE100 <- get.hist.quote(instrument = "^FTSE", start = "2000-01-03",
                          end = "2020-12-25", compression = "w", quote="Close")


# head(SP500, 6)

ret.SP500 <- diff(log(SP500), lag = 1)
ret.FTSE100 <- diff(log(FTSE100), lag = 1)

par(mfrow=c(1, 2))
plot(ret.FTSE100, cex.lab = 0.5, cex.axis = 0.5)

plot(ret.SP500, cex.lab = 0.5, cex.axis = 0.5)

ret.SP500 <- coredata(ret.SP500)
ret.FTSE100 <- coredata(ret.FTSE100)

# summary(ret.SP500); summary(ret.FTSE100)


plot(density(ret.SP500))
plot(density(ret.FTSE100))


shapiro.test(ret.SP500); shapiro.test(ret.FTSE100)  # Reject normality

# ks.test(as.vector(ret.SP500), pnorm)

jarque.bera.test(ret.SP500); jarque.bera.test(ret.FTSE100)


kurtosis(ret.SP500)
skewness(ret.SP500)

qqnorm(ret.SP500); qqline(ret.SP500, lty = 2)
qqnorm(ret.FTSE100); qqline(ret.FTSE100, lty = 2)


# Question (b)

L <- length(ret.SP500)

Pof <- cbind(ret.SP500, ret.FTSE100)
w <- matrix(c(0.5, 0.5))

ret.Pof <- log(1+((exp(Pof[, 1])-1)+(exp(Pof[, 2])-1))*(1/2))
# ret.Pof <- log(exp(Pof) %*% w)
ret.Pof.s <- sort(ret.Pof)

VaR.HS.99 <- -quantile(ret.Pof.s, 0.01)
VaR.HS.95 <- -quantile(ret.Pof.s, 0.05)


# Question (c)

sigma <- sqrt(t(w) %*% cov(Pof) %*% w) # Estimate portfolio volatility
mu <- t(w) %*% apply(Pof, 2, "mean")
skewness(ret.Pof)

VaR.Para.99 <- -as.numeric(sigma * qnorm(0.01) - mu)
VaR.Para.95 <- -as.numeric(sigma * qnorm(0.05) - mu)

# fit1 <- fitdistr(ret.SP500, "normal")
# fit2 <- fitdistr(ret.FTSE100, "normal")


# Question (d)

# Building AR models: the Box - Jenkins approach
# Step 1: Identification
par(mfrow=c(2,2))
acf(ret.SP500, col="green", lwd=2)
pacf(ret.SP500, col="green", lwd=2)
acf(ret.SP500^2, col="red", lwd=2)

temp1 <- auto.arima(ret.SP500, ic = "bic", allowmean = TRUE, max.q = 0)
temp1
# 2*(1-pt(0.6812/0.3039, 1091))
# 2*(1-pt(0.0563/0.0296, 1088))
tsdiag(temp1)


par(mfrow=c(2,2))
acf(ret.FTSE100, col="green", lwd=2)
pacf(ret.FTSE100, col="green", lwd=2)
acf(ret.FTSE100^2, col="red", lwd=2)

temp2 <- auto.arima(ret.FTSE100, ic = "bic", allowmean = TRUE, max.q = 0)
temp2
tsdiag(temp2)


# Step 2: Estimation
model.SP500 = garchFit(formula=~arma(0,0)+garch(1,1),data=ret.SP500,trace=F, cond.dist = "std")
model.FTSE100 = garchFit(formula=~arma(3,0)+garch(1,1),data=ret.FTSE100,trace=F, cond.dist="norm")


# Step 3: Model checking
# returns 1
res.SP500 <- residuals(model.SP500, standardize=TRUE)
par(mfrow=c(2,1))
acf(res.SP500, col="green", lwd=2)
acf(res.SP500^2, col="red", lwd=2)
par(mfrow=c(1,1))
Box.test(res.SP500, lag = 10, type = c("Ljung-Box"), fitdf = 1)
Box.test(res.SP500^2, lag = 10, type = c("Ljung-Box"), fitdf = 1)
model.SP500@fit$ics
u.SP500<-pstd(res.SP500, mean=0, sd=1)[4:length(ret.SP500)]
hist(u.SP500)

# Further distributional checks
#Kolmogorov-Smirnov test
set.seed(10)
KStest1<-LcKS(u.SP500, cdf = "punif")
KStest1$p.value
#Anderson-Darling test
ADtest1<-ad.test(u.SP500, null="punif")
ADtest1$p.value


# returns 2
res.FTSE100 <- residuals(model.FTSE100, standardize=TRUE)
# par(mfrow=c(2,1))
# acf(res.FTSE100, col="green", lwd=2)
# acf(res.FTSE100^2, col="red", lwd=2)
# par(mfrow=c(1,1))
Box.test(res.FTSE100, lag = 10, type = c("Ljung-Box"), fitdf = 3)
Box.test(res.FTSE100^2, lag = 10, type = c("Ljung-Box"), fitdf = 3)
model.FTSE100@fit$ics
u.FTSE100<-pnorm(res.FTSE100)[4:length(ret.FTSE100)]
hist(u.FTSE100)

# Further distributional checks
#Kolmogorov-Smirnov test
KStest2<-LcKS(u.FTSE100, cdf = "punif")
KStest2$p.value
#Anderson-Darling test
ADtest2<-ad.test(u.FTSE100, null="punif")
ADtest2$p.value


# Copula modelling
model=BiCopSelect(u.SP500, u.FTSE100, familyset=NA, selectioncrit="AIC", indeptest=TRUE, level=0.05)
model

# Value-at-Risk uisng Monte Carlo simulation
N=10000
set.seed(20045458)
u.sim=BiCopSim(N, family=model$family, model$par,  model$par2)

# Here we are assuming marginal models are N(0,1), completely ignoring our knowledge of real DGP.
y1simulated=qnorm(u.sim[,1], mean = 0, sd = 1) 
y2simulated=qnorm(u.sim[,2], mean = 0, sd = 1) 


portsim <- matrix(0, nrow = N, ncol = 1)
varsim <- matrix(0, nrow = 1, ncol = 2)

portsim=log(1+((exp(y1simulated)-1)+(exp(y2simulated)-1))*(1/2))
varsim=quantile(portsim,c(0.01,0.05))
varsim

VaR.MC.99 <- quantile(portsim, 0.01)
VaR.MC.95 <- quantile(portsim, 0.05)




