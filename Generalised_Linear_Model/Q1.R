# MAS3906 Project Q1

setwd("~/Desktop/MAS3906")

snoring = read.table("snoring-all.txt",header=T)

model = glm(cbind(hyper, total - hyper) ~ factor(sex) + smoking + obesity + snoring, family = binomial, data = snoring)

summary(model)

model.order = glm(cbind(hyper, total - hyper) ~ obesity + snoring + factor(sex) + smoking, family = binomial, data = snoring)
anova(model.order, test = "Chisq")

model.final = glm(cbind(hyper, total - hyper) ~ obesity + snoring, family = binomial, data = snoring)
summary(model.final)
anova(model.final, test = "Chisq")
