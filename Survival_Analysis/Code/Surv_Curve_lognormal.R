
if(!exists("ggplot")) library(ggplot2)
if(!exists("survfit")) library(survival)
if(!exist("ggsurv")) library(GGally)

data(cancer)

sf.cancer <- survfit(Surv(time, status) ~ 1, data = cancer)
# sf.sex <- survfit(Surv(time, status) ~ sex, data = cancer)

# Considering censoring
survfun1 = function(x)
{
  y <- 1 - plnorm(x, meanlog = 5.648811, sdlog = 1.084365)
  d <- data.frame(x = x, y = y)
  return(d)
}
x1 <- seq(0, 1000, 1)
d1 <- survfun1(x1)
# q <- ggplot(d,aes(x,y)) + geom_line(color="red")+xlab("x")+ylab("y")


# Ignoring censoring
survfun2 = function(x)
{
  y <- 1 - plnorm(x, meanlog = 5.422651, sdlog = 0.9050944)
  d <- data.frame(x = x, y = y)
  return(d)
}
x2 <- seq(0, 1000, 1)
d2 <- survfun2(x2)


p <- ggsurv(sf.cancer, CI = FALSE, surv.col = 'black', cens.col = 'red', size.est = 1) +
  geom_line(data = d1, aes(x, y, colour = "Censored"), size = 0.8) +
  geom_line(data = d2, aes(x, y, colour = "Uncensored"), size = 0.8) +
  scale_color_manual(name = "Type", values = c("Censored" = "blue", "Uncensored" = "green")) +
  theme(legend.position = c(0.8, 0.8))
p