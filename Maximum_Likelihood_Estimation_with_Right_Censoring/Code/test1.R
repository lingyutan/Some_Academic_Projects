install.packages("survminer")
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/survminer")

library(ggplot2)
library(ggpubr)
library(magrittr)
library(survival)
library(survminer)
library(dplyr)

data(ovarian)
# glimpse(ovarian)

surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)

fit1 <- survfit(surv_object ~ rx, data = ovarian)

plot(fit1, data = ovarian, pval = TRUE)
