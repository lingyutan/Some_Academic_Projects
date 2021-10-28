setwd("~/Desktop/UCL/STAT0029/ICA1")
rm(list = ls())

library(ggplot2)
library(dplyr)
library(gridExtra)
library(leaps)
library(car)

sprout_temp <- read.csv("mung_all_modify.csv", header = TRUE)
attach(sprout_temp)

sprout_temp <- filter(sprout_temp, temp != "low")


# par(mfcol = c(2, 3))
# 
# interaction.plot(temp, oxygen, length, legend = F)
# mtext("oxygen", side = 3, cex = 0.7)
# interaction.plot(oxygen, temp, length, legend = F)
# mtext("temp", side = 3, cex = 0.7)
# interaction.plot(temp, water, length, legend = F)
# mtext("water", side = 3, cex = 0.7)
# interaction.plot(water, temp, length, legend = F)
# mtext("temp", side = 3, cex = 0.7)
# interaction.plot(oxygen, water, length, legend = F)
# mtext("water", side = 3, cex = 0.7)
# interaction.plot(water, oxygen, length, legend = F)
# mtext("oxygen", side = 3, cex = 0.7)


sprout_temp <- within(sprout_temp, temp <- relevel(temp, ref = "medium"))
sprout_temp <- within(sprout_temp, oxygen <- relevel(oxygen, ref = "medium"))
sprout_temp <- within(sprout_temp, water <- relevel(water, ref = "medium"))

sprout_temp <- within(sprout_temp, temp <- relevel(temp, ref = "low"))
sprout_temp <- within(sprout_temp, oxygen <- relevel(oxygen, ref = "low"))
sprout_temp <- within(sprout_temp, water <- relevel(water, ref = "low"))

myboxplot = list()

myboxplot[[1]]  = ggplot(sprout_temp, aes(x = temp, y = length)) + geom_boxplot()

myboxplot[[2]]  = ggplot(sprout_temp, aes(x = oxygen, y = length)) + geom_boxplot()

myboxplot[[3]]  = ggplot(sprout_temp, aes(x = water, y = length)) + geom_boxplot()

grid.arrange(myboxplot[[1]], myboxplot[[2]], myboxplot[[3]], ncol = 3)

sprout <- model.matrix( ~ . - 1, data = sprout_temp)[, -1]
sprout <- as.data.frame(sprout)



# There must be such suitable conditions in which the mung seeds grow fast, so using factor 
# rather than numeric makes more sense. (or will not be linear, inflection points are expected ??)

# Box plots give a general idea about the factor effects. High temp, low oxygen, high water 
# have zero median length which means many seeds died in these circumstances.

# If removing all terms with 0 length as Line 11 did, we won't obtain a model with relatively large R^2.
# Those factors of interest may influence more on 'germination rate' rather than length ??




lm1 <- lm(length ~ water +. , data = sprout)
summary1 <- summary(lm1)
par(mfrow = c(2, 1))
plot(lm1, which = c(1, 2))
anova(lm1)
round(summary1$coefficients[, 1], 2)
summary1$coefficients[, 4]


# lm.test <- lm(length ~ tempmedium, data = sprout)
# 
# lm2 <- lm(length ~ .^2 - tempmedium:temphigh - oxygenmedium:oxygenhigh - watermedium:waterhigh, data = sprout) 
# summary(lm2)
# 
# lm3 <- step(lm2, direction = "backward")
# summary(lm3)
# 
# vif(lm3)
# 
# par(mfrow = c(2, 2))
# plot(lm3, which = c(1:3, 5))
# anova(lm3)

bss <- regsubsets(length ~ .^2 - tempmedium:temphigh - oxygenmedium:oxygenhigh - watermedium:waterhigh,
                  data = sprout, force.in = 1:6, method = "exhaustive", nvmax = 18)
bss_summary <- summary(bss)

best_adjr2 <- which.max(bss_summary$adjr2)
best_cp <- which.min(bss_summary$cp)
best_bic <- which.min(bss_summary$bic)


par(mfrow = c(1, 3))
plot(1:12, bss_summary$adjr2, xlab = "Number of variables", ylab = "Adjusted Rsq", type = "b", cex.lab = 1.5)
points(best_adjr2, bss_summary$adjr2[best_adjr2], col = "red", pch = 16)
plot(1:12, bss_summary$cp, xlab = "Number of variables", ylab = "Mallow's Cp", type = "b", cex.lab = 1.5)
points(best_cp, bss_summary$cp[best_cp], col = "red", pch = 16)
plot(1:12, bss_summary$bic, xlab = "Number of variables", ylab = "BIC", type = "b", cex.lab = 1.5)
points(best_bic, bss_summary$bic[best_bic], col = "red", pch = 16)

round(coef(bss, 5), 2)

lm.final <- lm(length ~ tempmedium + temphigh + oxygenmedium + oxygenhigh + watermedium + waterhigh +
                 tempmedium:waterhigh + temphigh:oxygenmedium + temphigh:oxygenhigh + temphigh:waterhigh +
                 oxygenhigh:waterhigh, data = sprout)

summary(lm.final)

id <- seq(1, 261, 10)

pred <- predict(lm.final, sprout[id, ])

round(pred, 2)

which.max(pred)

anova(lm.final)


######### 0 NOT accepted for Gamma #########

glm1 <- glm(length ~ ., data = sprout, family = Gamma(link = "log"))
sumy1 <- summary(glm1)
1 - pchisq(sumy1$deviance, sumy1$df.residual)
par(mfrow = c(2, 2))
plot(glm1)








