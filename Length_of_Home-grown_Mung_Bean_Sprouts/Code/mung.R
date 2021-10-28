setwd("~/Desktop/UCL/STAT0029/ICA1")
rm(list = ls())

library(ggplot2)
library(dplyr)
library(gridExtra)

sprout <- read.csv("mung_all_modify.csv", header = TRUE)

############# Data Modification #############
# sprout.temp <- filter(sprout, length != 0)
# sprout.temp <- filter(sprout, temp != "low") # Remove low temp as no seeds survive ??
# sprout <- sprout.temp
#############################################

sprout <- within(sprout, temp <- relevel(temp, ref = "medium"))
sprout <- within(sprout, oxygen <- relevel(oxygen, ref = "medium"))
sprout <- within(sprout, water <- relevel(water, ref = "medium"))

sprout <- within(sprout, temp <- relevel(temp, ref = "low"))
sprout <- within(sprout, oxygen <- relevel(oxygen, ref = "low"))
sprout <- within(sprout, water <- relevel(water, ref = "low"))

# sprout$temp <- ordered(sprout$temp, levels = c("low", "medium", "high"))
# sprout$oxygen <- ordered(sprout$oxygen, levels = c("low", "medium", "high"))
# sprout$water <- ordered(sprout$water, levels = c("low", "medium", "high"))


myboxplot = list()

myboxplot[[1]]  = ggplot(sprout, aes(x = temp, y = length)) + geom_boxplot()

myboxplot[[2]]  = ggplot(sprout, aes(x = oxygen, y = length)) + geom_boxplot()

myboxplot[[3]]  = ggplot(sprout, aes(x = water, y = length)) + geom_boxplot()

grid.arrange(myboxplot[[1]], myboxplot[[2]], myboxplot[[3]], ncol = 3)

# There must be such suitable conditions in which the mung seeds grow fast, so using factor 
# rather than numeric makes more sense. (or will not be linear, inflection points are expected ??)

# Box plots give a general idea about the factor effects. High temp, low oxygen, high water 
# have zero median length which means many seeds died in these circumstances.

# If removing all terms with 0 length as Line 11 did, we won't obtain a model with relatively large R^2.
# Those factors of interest may influence more on 'germination rate' rather than length ??




lm1 <- lm(length ~ ., data = sprout)
summary(lm1)
par(mfrow = c(2, 2))
plot(lm1, which = c(1, 2, 3, 5))
anova(lm1)


lm2 <- lm(length ~ .^2, data = sprout)
summary(lm2)
step(lm2)

lm3 <- step(lm2)
summary(lm3)

bss <- regsubsets(length ~ .^2, force.in = 1:6, data = sprout, method = "backward", nvmax = 18)
bss_summary <- summary(bss)

best_adjr2 <- which.max(bss_summary$adjr2)
best_cp <- which.min(bss_summary$cp)
best_bic <- which.min(bss_summary$bic)


par(mfrow = c(1, 3))
plot(1:12, bss_summary$adjr2, xlab = "Number of variables", ylab = "Adjusted Rsq", type = "b")
points(best_adjr2, bss_summary$adjr2[best_adjr2], col = "red", pch = 16)
plot(1:12, bss_summary$cp, xlab = "Number of variables", ylab = "Mallow's Cp", type = "b")
points(best_cp, bss_summary$cp[best_cp], col = "red", pch = 16)
plot(1:12, bss_summary$bic, xlab = "Number of variables", ylab = "BIC", type = "b")
points(best_bic, bss_summary$bic[best_bic], col = "red", pch = 16)

coef(bss, 5)



par(mfrow = c(2, 2))
plot(lm3, which = c(1:3, 5))
anova(lm3)

######### 0 NOT accepted for Gamma #########

glm1 <- glm(length ~ ., data = sprout, family = Gamma(link = "log"))
# sumy1 <- summary(glm1)
# 1 - pchisq(sumy1$deviance, sumy1$df.residual)
plot(glm1)






