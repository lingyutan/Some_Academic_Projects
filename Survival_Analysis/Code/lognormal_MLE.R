# Result of surveg, note the part 'censored or not'

# test whether survival package is loaded, if not, then load survival
if(!exists("survreg")) library(survival)

x <- cancer$time
r <- length(x)

xs <- sort(x)

#-----------Censored or not-----------#

# Considering censoring  /  is.optional(lung$status - 1)
statusx <- lung$status

# Ignoring censoring
# statusx <- rep(1, r)

#-------------------------------------#

dat.weibull <- data.frame(xs, statusx)

names(dat.weibull) <- c("time", "status")

out.weibull <- survreg(Surv(time, status) ~ 1, dist = "lognormal", data = dat.weibull)

meanlog <- out.weibull$coef
sdlog <- out.weibull$scale

parms <- c(meanlog, sdlog)
names(parms) <- c("meanlog", "sdlog")

list(mles = parms)
