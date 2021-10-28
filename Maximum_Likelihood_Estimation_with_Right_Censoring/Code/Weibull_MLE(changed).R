# Result of surveg, note the part 'censored or not'

# test whether survival package is loaded, if not, then load survival
if(!exists("survreg")) library(survival)

x <- cancer$time
r <- length(x)

xs <- sort(x)

#-----------Censored or not-----------#

# Considering censoring  /  is.optional(lung$status - 1)
# statusx <- lung$status

# Ignoring censoring
 statusx <- rep(1, r)

#-------------------------------------#

dat.weibull <- data.frame(xs, statusx)

names(dat.weibull) <- c("time", "status")

out.weibull <- survreg(Surv(time, status) ~ 1, dist = "weibull", data = dat.weibull)

scale <- exp(out.weibull$coef)
shape <- 1 / out.weibull$scale

alpha <- shape
lambda <- 1 / (scale^shape)

parms <- c(scale, shape, alpha, lambda)
names(parms) <- c("scale", "shape", "alpha", "lambda")

list(mles = parms)
