sprout <- read.csv("sproutdata.csv", header = TRUE)

sprout$A <- factor(sprout$A)
sprout$B <- factor(sprout$B)
sprout$C <- factor(sprout$C)

fit <- aov(length ~ A + B + C, sprout)
summary(fit)
