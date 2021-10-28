# Surv_Curve_func
# Fitted curve using likelihood function 

if(!exists("ggplot")) library(ggplot2)
if(!exists("survfit")) library(survival)
if(!exists("ggsurv")) library(GGally)

data(cancer)

sf.cancer <- survfit(Surv(time, status) ~ 1, data = cancer)
sf.sex <- survfit(Surv(time, status) ~ sex, data = cancer)

# Considering censoring
survfun1 = function(x)
{
  lambda = 0.000359415
  alpha = 1.313612382
  y <- exp(-lambda*((x)^alpha))
  d <- data.frame(x = x, y = y)
  return(d)
}
x1 <- seq(0, 1022, 1)
d1 <- survfun1(x1)
# q <- ggplot(d,aes(x,y)) + geom_line(color="red")+xlab("x")+ylab("y")

# Ignoring censoring
survfun2 = function(x)
{
  lambda <- 0.0002006867
  alpha <- 1.4624512710
  y <- exp(-lambda*((x)^alpha))
  d <- data.frame(x = x, y = y)
  return(d)
}
x2 <- seq(0, 1022, 1)
d2 <- survfun2(x2)

survfun3 = function(x)
{
  res <- ecdf(cancer$time)
  y <- 1 - res(x)
  d <- data.frame(x = x, y = y)
  return(d)
}
x3 <- seq(0, 1022, 1)
d3 <- survfun3(x3)


p <- ggsurv(sf.cancer, CI = T, surv.col = 'black', cens.col = 'red', size.est = 1, size.ci = 0, ylab = 'Survival Rate')


p <- ggsurv(sf.cancer, CI = T, surv.col = 'black', cens.col = 'red', size.est = 1, size.ci = 0) +
  geom_line(data = d1, aes(x, y, colour = "Censored"), size = 0.8) +
  scale_color_manual(name = "Type", values = c("Censored" = "blue")) +
  theme(legend.position = c(0.8, 0.8))

p <- ggsurv(sf.cancer, CI = T, surv.col = 'black', cens.col = 'red', size.est = 1, size.ci = 0.5) +
  geom_line(data = d1, aes(x, y, colour = "Censored"), size = 0.8) +
  scale_color_manual(name = "Type", values = c("Censored" = "blue")) +
  theme(legend.position = c(0.8, 0.8))


p <- ggsurv(sf.cancer, CI = T, surv.col = 'black', cens.col = 'red', size.est = 1, size.ci = 0) +
  geom_line(data = d1, aes(x, y, colour = "Censored"), size = 0.8) +
  geom_line(data = d2, aes(x, y, colour = "Uncensored"), size = 0.8) +
  scale_color_manual(name = "Type", values = c("Censored" = "blue", "Uncensored" = "green")) +
  theme(legend.position = c(0.8, 0.8))


p <- ggsurv(sf.cancer, CI = T, surv.col = 'black', cens.col = 'red', size.est = 1, size.ci = 0) +
  geom_line(data = d1, aes(x, y, colour = "Censored"), size = 0.8) +
  geom_line(data = d2, aes(x, y, colour = "Uncensored"), size = 0.8) +
  geom_line(data = d3, aes(x, y, colour = "Ecdf"), size = 0.8) +
  scale_color_manual(name = "Type", values = c("Censored" = "blue", "Uncensored" = "green", "Ecdf" = "red")) +
  theme(legend.position = c(0.8, 0.8))


p


