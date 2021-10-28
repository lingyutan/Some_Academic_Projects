
if(!exists("ggplot")) library(ggplot2)
if(!exists("survfit")) library(survival)

data(cancer)

sf.cancer <- survfit(Surv(time, status) ~ 1, data = cancer)
sf.sex <- survfit(Surv(time, status) ~ sex, data = cancer)

# Considering censoring
survfun1 = function(x)
{
  scale = 417.514299
  shape = 1.222948
  y <- exp(-(x/scale)^shape)
  d <- data.frame(x = x, y = y)
  return(d)
}
x1 <- seq(0, 1000, 1)
d1 <- survfun1(x1)
# q <- ggplot(d,aes(x,y)) + geom_line(color="red")+xlab("x")+ylab("y")


# Ignoring censoring
survfun2 = function(x)
{
  scale = 336.699601
  shape = 1.466944
  y <- exp(-(x/scale)^shape)
  d <- data.frame(x = x, y = y)
  return(d)
}
x2 <- seq(0, 1000, 1)
d2 <- survfun2(x2)


ggsurv(sf.cancer, CI = FALSE) +
  geom_line(data = d1, aes(x, y, colour = "red"), size = 1) +
  geom_line(data = d2, aes(x, y, colour = "blue"), size = 1) +
  scale_color_discrete(name = "Fitted Curve", labels = c("Uncensored", "Censored"))
