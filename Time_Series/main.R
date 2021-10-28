# MAS3911 Time Series Project

library(forecast)
library(lmtest)
library(timeSeries)
library(fpp2)
library(tseries)

setwd("~/Desktop/MAS3911")
data<-read.table("projectdata.txt") 
y<-data[, 57]

# >>>>>>>>>> Section 2 <<<<<<<<<< #
elec <- list()
elec$data <- ts(y, start = c(2006, 1), frequency = 12)
elec$logdata <- ts(log(y), start = c(2006, 1), frequency = 12)

plot(elec$data, ylab = "Electricity Consumption") # Fig.1
points(elec$data, pch = 21, cex = 0.4, bg = 1)

# >>>>>>>>>> Section 3.1.1 <<<<<<<<<< #
elec$season = gl(12, 1, 120)
# Use curve fitting to estimate trend
elec$time = c(1 : 120)

# Compare trans and untrans
fit1 <- lm(elec$data ~ elec$time + elec$season)
fit2 <- lm(elec$logdata ~ elec$time + elec$season)
c(summary(fit1)$r.squared, summary(fit1)$adj.r.squared)
c(summary(fit2)$r.squared, summary(fit2)$adj.r.squared)

# BoxCox parameter test
lambda <- BoxCox.lambda(elec$data)
print(lambda)
lambda <- BoxCox.lambda(elec$logdata)
print(lambda)
# elec$tdata <- BoxCox(elec$data, lambda)

# We use log trans - fit2, use elec$logdata throughout the project
elec$trend <- ts(fitted.values(fit2), start = c(2006, 1), frequency = 12)

# Plot of logdata and trend
plot(elec$logdata, ylab = "Log(Electricity Consumption)") # Fig.2
points(elec$logdata, pch = 21, cex = 0.4, bg = 1)
lines(elec$trend, col = 2, lty = 2)
legend(2006, 3.8, legend = c("Log(Electricity)", "Trend"), col = c(1, 2), lty = c(1, 2), pch = c(21, NA), pt.bg = c(1, NA), pt.cex = c(0.4, NA), cex = 0.8)

# Now we are focusing elec$resid, the residuals of the fitted trend model
elec$resid <- ts(residuals(fit2), start = c(2006, 1), frequency = 12)
plot(elec$resid, ylab = "Residual Component") # Fig.3

# ADF Test
adf.test(elec$resid)

acf(elec$resid, ci.type = "ma", xlab = "Lag/year", main = "") # Fig.4
# acf(elec$logdata, lag.max = 120, ci.type = "ma", xlab = "Lag/year")

# Durbin-Watson Statistic
dwtest(fit2)

# Peaks and Troughs test
residt = timeSeries(residuals(fit2), elec$time)
turnsStats(residt)
# |Z| = 4.07, p-value = 2*(1-pnorm(4.07))


pacf(elec$resid, xlab = "Lag/year", main = "") # Fig.5

# >>>>>>>>>> Section 3.1.2 <<<<<<<<<< #

# AR(1)
ar1 <- arima(elec$resid, order = c(1, 0, 0))
ar1
pt(8.951, 118)

tsdiag(ar1, gof.lag = 40) # Fig.6, 800*700

plot.default(elec$resid-ar1$residuals, ar1$residuals, xlab = "Fitted Values", ylab = "Residuals") # Fig.7, 700*550


# MA(2)
ma2 <- arima(elec$resid, order = c(0, 0, 2))
ma2
pt(2.2712, 117) # (120 - 2 - 1) = 117 degrees of freedom
2 * (1 - pt(2.2712, 117)) # < 0.05, all terms needed
tsdiag(ma2, gof.lag = 40) # Fig.8
plot.default(elec$resid-ma2$residuals, ma2$residuals, xlab = "Fitted Values", ylab = "Residuals") # Fig.9


# ARMA(1, 1)
arma11 = arima(elec$resid, order = c(1, 0, 1))
arma11
pt(3.219, 117)
2 * (1 -  pt(3.219, 117)) 
pt(1.527, 117)
2 * (1 - pt(1.527, 117))
# No need!

# AR(2)
ar2 <- arima(elec$resid, order = c(2, 0, 0))
ar2
pt(1.249, 117)
# No need!

# MA(3)
ma3 <- arima(elec$resid, order = c(0, 0, 3))
ma3
pt(2.0898, 116)
2 * (1 - pt(2.0898, 116)) # < 0.05, all terms needed
tsdiag(ma3, gof.lag = 40) # Fig.10
plot.default(elec$resid-ma3$residuals, ma3$residuals, xlab = "Fitted Values", ylab = "Residuals") # Fig.11

# MA(4)
ma4 <- arima(elec$resid, order = c(0, 0, 4))
ma4
pt(1.566, 115)
2 * (1 - pt(1.566, 115))
# No need!


# >>>>>>>>>> Section 3.2.1 <<<<<<<<<< #

# ARIMA
# Seasonally differenced transformed electricity consumption
elec$logdata %>% diff(lag=12) %>% ggtsdisplay(xlab="Year", main="") # Fig.12


# >>>>>>>>>> Section 3.2.2 <<<<<<<<<< #

# Check AICc for 10 models near 2 chosen models
(fit_i1 <- Arima(elec$data, order=c(0,0,3), seasonal=c(0,1,1), lambda = 0))$aicc
(fit_i2 <- Arima(elec$data, order=c(1,0,3), seasonal=c(0,1,1), lambda = 0))$aicc # Best
(fit_i3 <- Arima(elec$data, order=c(0,0,4), seasonal=c(0,1,1), lambda = 0))$aicc
(fit_i4 <- Arima(elec$data, order=c(0,0,3), seasonal=c(1,1,1), lambda = 0))$aicc
(fit_i5 <- Arima(elec$data, order=c(0,0,3), seasonal=c(0,1,2), lambda = 0))$aicc
(fit_i6 <- Arima(elec$data, order=c(1,0,0), seasonal=c(2,1,0), lambda = 0))$aicc
(fit_i7 <- Arima(elec$data, order=c(2,0,0), seasonal=c(2,1,0), lambda = 0))$aicc
(fit_i8 <- Arima(elec$data, order=c(1,0,1), seasonal=c(2,1,0), lambda = 0))$aicc
(fit_i9 <- Arima(elec$data, order=c(1,0,0), seasonal=c(3,1,0), lambda = 0))$aicc
(fit_i10 <- Arima(elec$data, order=c(1,0,0), seasonal=c(2,1,1), lambda = 0))$aicc


fit_i2 <- Arima(elec$data, order=c(1,0,3), seasonal=c(0,1,1), lambda = 0)

# checkresiduals(fit_i2)
tsdiag(fit_i2, gof.lag = 40) # Fig.13
plot.default(elec$resid-fit_i2$residuals, fit_i2$residuals, xlab = "Fitted Values", ylab = "Residuals") # Fig.14


# >>>>>>>>>> Section 3.3 <<<<<<<<<< #

# Repeat the procedures above to determine "best" models using only nine years of data

####################### Repeating Process #######################

elec$train <- window(elec$data, start = c(2006, 1), end = c(2014, 12))
elec$test <- window(elec$data, start = c(2015, 1))
elec$logtrain <- window(elec$logdata, start = c(2006, 1), end = c(2014, 12))
elec$logtest <- window(elec$logdata, start = c(2015, 1))

elec$ftime = c(1 : 108)
elec$fseason = gl(12, 1, 108)
fit_f1 <- lm(elec$logtrain ~ elec$ftime + elec$fseason)
elec$ftrend <- ts(fitted.values(fit_f1), start = c(2006, 1), frequency = 12)

elec$fresid <- ts(residuals(fit_f1), start = c(2006, 1), frequency = 12)
plot(elec$fresid, ylab = "Residual Component")

acf(elec$fresid, ci.type = "ma", xlab = "Lag/year", main = "") # suggests MA(2)

dwtest(fit_f1) # Reject Randomness 

pacf(elec$fresid, xlab = "Lag/year", main = "") # suggests AR(1)

# so the results are the same as using 10 years, then keep checking...

# AR(1) - Yes!!!
far1 <- arima(elec$fresid, order = c(1, 0, 0))
far1
tsdiag(far1, gof.lag = 40) # Yes!
plot.default(elec$fresid-far1$residuals, far1$residuals, xlab = "Fitted Values", ylab = "Residuals") # Yes!

# MA(2) - Yes!!!
fma2 <- arima(elec$fresid, order = c(0, 0, 2))
fma2
2 * (1 - pt(2.2712, 117)) # Yes!
tsdiag(fma2, gof.lag = 40) # Yes!
plot.default(elec$fresid-fma2$residuals, fma2$residuals, xlab = "Fitted Values", ylab = "Residuals") # Yes!

# ARMA(1, 1) - No!!!
farma11 = arima(elec$fresid, order = c(1, 0, 1))
farma11
2 * (1 - pt(1.274, 117)) # No!

# AR(2) - No!!!
far2 <- arima(elec$fresid, order = c(2, 0, 0))
far2

# MA(3) - Yes!!!
fma3 <- arima(elec$fresid, order = c(0, 0, 3))
fma3
2 * (1 - pt(2.104, 116)) # Yes!
tsdiag(fma3, gof.lag = 40) # Yes!
plot.default(elec$fresid-fma3$residuals, fma3$residuals, xlab = "Fitted Values", ylab = "Residuals")  # Yes!

# MA(4) - No!!!
fma4 <- arima(elec$fresid, order = c(0, 0, 4))
fma4
2 * (1 - pt(1.639, 115)) # No!

far1
fma2
fma3

# Check above three models and choose AR(1) again.

elec$logtrain %>% diff(lag=12) %>% ggtsdisplay(xlab="Year", main="")
# same as above

(fit_fi1 <- Arima(elec$train, order=c(0,0,3), seasonal=c(0,1,1), lambda = 0))$aicc
(fit_fi2 <- Arima(elec$train, order=c(1,0,3), seasonal=c(0,1,1), lambda = 0))$aicc # best
(fit_fi3 <- Arima(elec$train, order=c(0,0,4), seasonal=c(0,1,1), lambda = 0))$aicc
(fit_fi4 <- Arima(elec$train, order=c(0,0,3), seasonal=c(1,1,1), lambda = 0))$aicc
(fit_fi5 <- Arima(elec$train, order=c(0,0,3), seasonal=c(0,1,2), lambda = 0))$aicc
(fit_fi6 <- Arima(elec$train, order=c(1,0,0), seasonal=c(2,1,0), lambda = 0))$aicc
(fit_fi7 <- Arima(elec$train, order=c(2,0,0), seasonal=c(2,1,0), lambda = 0))$aicc
(fit_fi8 <- Arima(elec$train, order=c(1,0,1), seasonal=c(2,1,0), lambda = 0))$aicc
(fit_fi9 <- Arima(elec$train, order=c(1,0,0), seasonal=c(3,1,0), lambda = 0))$aicc
(fit_fi10 <- Arima(elec$train, order=c(1,0,0), seasonal=c(2,1,1), lambda = 0))$aicc

######################### End Repeating #########################


# Predict two models using first nine years data

# AR(1)
predictedtrend <- fit_f1$coef[1] + fit_f1$coef[2]*(109:120)
season <- c(0, fit_f1$coef[3:13])
predtrendseas <- predictedtrend + season
predtrendseas <- ts(predtrendseas, start = c(2015, 1), frequency = 12)

plot(elec$logdata, xlim = c(2006, 2016), ylim = c(2.5, 4.0))
points(predtrendseas, col = 2, cex = 0.6)
lines(predtrendseas, col = 2, lty = 2)
legend(2006, 3.8, legend = c("Log(Electricity)", "Predict"), col = c(1, 2), lty = c(1, 2), pch = c(NA, 21), pt.cex = c(NA, 0.6), cex = 0.8)

far1 <- arima(elec$fresid, order = c(1, 0, 0))
far1P <- predict(far1, n.ahead = 12)
far1P$pred <- ts(far1P$pred, start = c(2015, 1), frequency = 12)
far1PT <- far1P$pred + predtrendseas
far1P$se <- ts(far1P$se, start = c(2015, 1), frequency = 12)
far1PTU <- far1PT + 2*far1P$se
far1PTL <- far1PT - 2*far1P$se


# Predicted values and 95% CI for AR(1)
round(cbind(exp(far1PT), exp(far1PTL), exp(far1PTU)), 2)


# Predicted values and 95% CI for ARIMA
a <- elec$train %>%
     Arima(order=c(1,0,3), seasonal=c(0,1,1), lambda=0) %>%
     forecast(h = 12, level = 95)
round(cbind(a$mean, a$lower, a$upper), 2)

# Compute RMSE and plot
#AR(1)
sqrt(mean((exp(far1PT) - elec$test)^2)) # 1.673

#ARIMA
sqrt(mean((a$mean - elec$test)^2)) # 1.465


plot(elec$data, xlim = c(2006, 2016), ylab = "Electricity Consumption") # Fig.15
lines(exp(far1PT), col = 2, lty = 2)
legend(2006, 45, legend = c("Electricity", "AR(1) Predict"), col = c(1, 2), lty = c(1, 2))

ts_arima <- ts(a$mean, start = c(2015, 1), frequency = 12)
plot(elec$data, xlim = c(2006, 2016), ylab = "Electricity Consumption") # Fig.16
lines(ts_arima, col = "blue", lty = 2)
legend(2006, 45, legend = c("Electricity", "ARIMA Predict"), col = c(1, "blue"), lty = c(1, 2))


# >>>>>>>>>> Section 4 <<<<<<<<<< #

# Predict 2016 for AR(1)
# We already fit a model using 10 years before: fit2

predictedtrend2 <- fit2$coef[1] + fit2$coef[2]*(121:126)
season2 <- c(0, fit2$coef[3:7])
predtrendseas2 <- predictedtrend2 + season2
predtrendseas2 <- ts(predtrendseas2, start = c(2016, 1), frequency = 12)

ar1 <- arima(elec$resid, order = c(1, 0, 0))
ar1P <- predict(ar1, n.ahead = 12)
ar1P$pred <- ts(ar1P$pred, start = c(2016, 1), frequency = 12)
ar1PT <- ar1P$pred + predtrendseas2
ar1P$se <- ts(ar1P$se, start = c(2016, 1), frequency = 12)
ar1PTU <- ar1PT + 2*ar1P$se
ar1PTL <- ar1PT - 2*ar1P$se

# Plot of original data, predicted values for 2016 and 95% CI
plot(elec$data, xlim = c(2006, 2017), ylab = "Electricity Consumption", ylim = c(10, 50)) # Fig.17
lines(exp(ar1PT), col = 2, lty = 2)
lines(exp(ar1PTU), col = 4, lty = 3)
lines(exp(ar1PTL), col = 4, lty = 3)
legend(2006, 50, legend = c("Electricity", "AR(1) Predict", "95% CI"), col = c(1, 2, 4), lty = c(1, 2, 3), cex = 0.8)


# Predicted values and 95% CI for AR(1) 2016
round(cbind(exp(ar1PT), exp(ar1PTL), exp(ar1PTU)), 2)

