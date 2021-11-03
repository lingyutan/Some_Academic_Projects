# Result of surveg, note the part 'censored or not'

# test whether survival package is loaded, if not, then load survival

if(!exists("cancer")) library(survival)
if(!exists("flexsurvreg")) library(flexsurv)

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

flexsurvreg(Surv(time, status) ~ 1, dist = "gamma", data = dat.weibull)

