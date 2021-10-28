# library(car)

library(olsrr)

setwd("H:/R/MAS3903/Project")
data = read.table("Beauty.csv", header=T, sep=',')
attach(data)
eva = courseevaluation
stu = students

fitall <- lm(eva ~ tenured + minority + age + female + stu + lower + beauty)
# summary(fitall)
# vif(fitall)
ols_mallows_cp(fitall, fitall)

# fitbe = step(fitall, direction = "backward")

fit2 <- lm(eva ~ tenured + minority + age + stu + lower + beauty)
ols_mallows_cp(fit2, fitall)

fit3 <- lm(eva ~ tenured + minority + stu + lower + beauty)
ols_mallows_cp(fit3, fitall)

fit4 <- lm(eva ~ tenured + minority + lower + beauty)
ols_mallows_cp(fit4, fitall)

fit5 <- lm(eva ~ tenured + minority + beauty*lower + lower + beauty)
ols_mallows_cp(fit5, fitall)

fit6 <- lm(eva ~ tenured + minority + log(stu) + beauty*lower + lower + beauty)
ols_mallows_cp(fit6, fitall)

boxplot(eva)
boxplot(age)

#sqrt & log
par(mfrow=c(1,2))
boxplot(sqrt(stu))
boxplot(log(stu))

boxplot(beauty)



fitc1 = lm(eva ~ minority+beauty+lower+tenured+beauty*lower)
summary(fitc1)

fitc2 = lm(eva ~ minority+beauty+lower+tenured+log(stu)+beauty*log(stu))
summary(fitc2)
plot(fitted.values(fitc2), rstandard(fitc2))

plot(fitc2)

# plot(fitc2$coefficients[7]*beauty*log(stu)+fitc2$coefficients[3]*beauty, eva-fitc2$coefficients[1]-fitc2$coefficients[2]*minority-fitc2$coefficients[4]*lower-fitc2$coefficients[5]*tenured-fitc2$coefficients[6]*log(stu), ylab = "y")

library(car)
vif(fitc2)

vif(fitall)
