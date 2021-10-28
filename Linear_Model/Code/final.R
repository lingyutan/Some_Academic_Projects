library(car)
library(olsrr)


setwd("H:/R/MAS3903/Project")
data = read.table("Beauty.csv", header=T, sep=',')
attach(data)
eva = courseevaluation
stu = students

fitall <- lm(eva ~ tenured + minority + age + female + log(stu) + lower + beauty)
summary(fitall)
vif(fitall)

#remove female
fit1 <- lm(eva ~ tenured + minority + age + log(stu) + lower + beauty)
summary(fit1)
vif(fit1)

par(mfrow=c(3, 2))
plot(fitted.values(fit1), rstandard(fit1))
plot(age, rstandard(fit1))
plot(log(stu), rstandard(fit1))
plot(beauty, rstandard(fit1))
qqnorm(rstandard(fit1)); abline(0, 1)

#remove age
fit2 <- lm(eva ~ tenured + minority + log(stu) + lower + beauty)
summary(fit2)
vif(fit2)

#remove log(stu)
fit3 <- lm(eva ~ tenured + minority  + lower + beauty)
summary(fit3)
vif(fit3)













