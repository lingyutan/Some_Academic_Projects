# install.packages("ggfortify")

library(ggplot2)
library(ggfortify)
library(survival)


autoplot(survfit(Surv(time = lung$time, event = lung$status) ~ 1, data = lung), pval = TRUE, surv.colour = 'orange', censor.colour = 'red')

# install.packages("OIsurv")
# library(OIsurv)


surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)

fit1 <- survfit(surv_object ~ rx, data = ovarian)

data(lung)
my.surv<-Surv(time = ovarian$futime, event = ovarian$fustat)
my.fit<-survfit(my.surv ~ rx, data = ovarian)   #Kaplan-Meier
summary(my.fit)
plot(my.fit)
