#Fully Comprehensive attempt with comments step-by-step:

url = "Beauty.csv"
setwd("H:/R/MAS3903/Project")
project = read.table(url, header=T,sep=',')
View(project)

stud = project$students
pulch = project$beauty
eval = project$courseevaluation
low = project$lower
fem = project$female
tenu = project$tenured
min = project$minority
age = project$age

#make data frame and work out correlations and plot for context:

z = data.frame(eval,tenu,min,age,fem,stud,low,pulch)
round(cor(z),2)
plot(z)
#Just continuous:
conz = data.frame(eval,pulch,stud,age)
round(cor(conz),2) #positive between pulch and eval, stud and eval, negative between age and eval.
plot(conz) # say if relationship is linear or strong or whatever

#continuous boxplots including response:

par(mfrow=c(2,2))
boxplot(eval,ylab="Course Evaluation")
boxplot(pulch,ylab="Beauty Rating")
boxplot(stud,ylab="Student Number")
boxplot(age,ylab="Age")

#All look fairly symmetric except student number. So let's do a natural log transform of the student number:

lnstud = log(stud) # perhaps say that natural log provides more symmetric distribution than square root which is
#why it is chosen.

par(mfrow=c(1,2))
boxplot(stud,ylab="Student Number")
boxplot(lnstud,ylab="Natural Log of Student Number")

#lnstud is much more symmetric in the distribution from the box plot than the non-transformed so let's stick with that.

#Let's fit first model in same order as is given in View(project) left to right so:

m1 = lm(eval~tenu+min+age+fem+lnstud+low+pulch)
#check for multicollinearity problems:
library(car)
vif(m1)
#None are particularly high (near 4 or above) so no need to remove variables from a multicollinearity stand point.
summary(m1)


# > plot(sort(eval))
# > lines(sort(fitted.values(m1)))


#fem, age, and lnstud are insignificant but before we remove anything, let's check residuals of all continuous variables
# and fitted values:

par(mfrow=c(2,2))

plot(fitted.values(m1),rstandard(m1),ylab="Studentised Residuals",xlab="Fitted Values")
abline(h=0)
abline(h=-2,col=2,lty=2)
abline(h=2,col=2,lty=2)
plot(age,rstandard(m1),ylab="Studentised Residuals",xlab="Age")
abline(h=0)
abline(h=-2,col=2,lty=2)
abline(h=2,col=2,lty=2)
plot(lnstud,rstandard(m1),ylab="Studentised Residuals",xlab="Natural Log of Student Number")
abline(h=0)
abline(h=-2,col=2,lty=2)
abline(h=2,col=2,lty=2)
plot(pulch,rstandard(m1),ylab="Studentised Residuals",xlab="Beauty Rating")
abline(h=0)
abline(h=-2,col=2,lty=2)
abline(h=2,col=2,lty=2)

#nothing greatly worrying, only few points out of over 151 outside [-2,2] and not by particularly much. even spread of points in all.
#Handful of points near -3 on all the plots so they may need to be looked into. constant variance assumption likely validated.

par(mfrow=c(1,1))
qqnorm(rstandard(m1),ylab="Studentised Residuals")
abline(0,1)

#close to line, expected ripples. Tails could be looked into for outliers.

library(nortest)
ad.test(rstandard(m1))

#p-value of anderson darling normality test is 0.4327 which is quite high and so validates the normality assumption.

#leverages:
plot(hatvalues(m1),ylab="Hat Values")
abline(h=16/151)

#Five points are over the cut-off line, not by a lot but still concerning. They are higher than rest.

#cook:

plot(cooks.distance(m1),ylab="Cook's Distance")

#point 9 is much higher than the rest. point 104 a bit higher as well. Not different enough to take action right now.

#Now let's reorder m1 from most to least significant based on summary and then proceed with manual
#backwards elimination:

m1 = lm(eval~min+pulch+low+tenu+lnstud+age+fem)
summary(m1)
#remove fem as it has highest p-value:
m2 = lm(eval~min+pulch+low+tenu+lnstud+age)
summary(m2)
#R^2 decreased but adjusted R^2 increased so better model. lnstud become less insignificant, age more so.
#Remove age as quite insignificant:
m3 = lm(eval~min+pulch+low+tenu+lnstud)
summary(m3)
#R^2 decreased but adjusted R^2 increased so better model. (put in actual values when writing up.) lnstud becomes
#more insignificant, rest are significant.
#If remove lnstud then:
m4 = lm(eval~min+pulch+low+tenu)
summary(m4)
#low now only approaching significane whereas rest are significant. R^2 decreased and adjusted R^2 decreased by
#a noticeable amount - should have left in?

#Mallow's suggests same as AIC:
library(olsrr)
ols_mallows_cp(m1,m1)
ols_mallows_cp(m2,m1)
ols_mallows_cp(m3,m1)
ols_mallows_cp(m4,m1)
#Try AIC and see what that attains using both ways stepwise method:

lnaic = step(m1,direction="both")

#AIC says to keep lnstud as adjusted R^2 would decrease otherwise, as we noted when done manually.

#So continue on with lnstud so m3:
#Check residual plots:

plot(fitted.values(m3),rstandard(m3),ylab="Studentised Residuals",xlab="Fitted Values")
abline(h=0)
abline(h=2,col=2,lty=2)
abline(h=-2,col=2,lty=2)
#handful of points outside [-2,2]. Some close to -3. Otherwise even spread of points and no sign of changing variance.
#Perhaps look into these points.
qqnorm(rstandard(m3),ylab = "Studentised Residuals")
abline(0,1)
#points close to line, expected ripples, look into tails at ends as they are somewhat worrying.
ad.test(rstandard(m3))
#p-value of anderson darling normality test is 0.1994 which is high enough to say that normality assumption is validated.

#see if taking point 9 out changes coefficients of m3
eval1 = eval[-c(9)]
min1=min[-c(9)]
pulch1 = pulch[-c(9)]
low1 = low[-c(9)]
tenu1 = tenu[-c(9)]
lnstud1 = lnstud[-c(9)]

sans9m3 = lm(eval1~min1+pulch1+low1+tenu1+lnstud1)
summary(sans9m3)

#Not very much difference in the coefficients compared to m3 so may as well keep the point in and carry on.
#Query group as to whether other points should be looked into. Point 9 was worst. What about points from hat values?
#look into past notes and see what Dr Avery did there.

#Now use anova on m3 to test interactions. Start with two-way:
#list of two-way: min*pulch, min*low, min*tenu, min*lnstud,pulch*low,pulch*tenu,pulch*lnstud,low*tenu,low*lnstud,
#tenu*lnstud
#testing one by one using anova:

#min*pulch insignificant with p-value of 0.5547377
#min*low 0.7770274 p-value so insignificant.
#min*tenu p-value of 0.6239936 so insignificant.
#min*lnstud p-value of 0.6315024 so insignificant.
#pulch*low p-value of 0.0230306 so significant at 5% level.
#pulch*tenu p-value of 0.0958556 so insignificant.
#pulch*lnstud p-value of 2.1*10^-7 so highly significant.
#low*tenu p-value of 0.9255426 so insignificant.
#low*lnstud p-value of 0.293696 so insignificant.
#tenu*lnstud p-value of 0.5665938 so insignificant.

#Fit significant ones together with other explanatory variables in order from most to least significant:
anova(lm(eval~min+pulch+low+tenu+lnstud+pulch*lnstud+pulch*low))
#pulch*low insignificant so remove. pulch*lnstud still significant with p-value of 2.298*10^-7 so keep in.

#What about three way interactions:
#list: min*pulch*low, min*pulch*lnstud, min*low*lnstud, min*tenu*lnstud, pulch*low*tenu, pulch*low*lnstud,
#pulch*tenu*lnstud, low*tenu*lnstud

#testing one by one using anova (without two-way written, only those included by R):

#min*pulch*low has p-value of 0.6650705 so not significant.
#min*pulch*lnstud has p-value of 0.55293 so not significant.
#min*low*lnstud has p-value of 0.3949492 so not significant.
#min*tenu*lnstud has p-value of 0.0733662 so approaching but not significant. none of its two-ways are either.
#pulch*low*tenu has p-value of 0.0328570 so significant at the 5% level.
#pulch*low*lnstud has p-value of 0.969588 so not significant at all.
#pulch*tenu*lnstud has p-value of 0.357466 so not significant.
#low*tenu*lnstud has p-value of 0.1446266 so not significant.

#put pulch*low*tenu in model after pulch*lnstud for:
anova(lm(eval~min+pulch+low+tenu+lnstud+pulch*lnstud+pulch*low*tenu))
#three-way and included two-ways no longer significant so just stick with pulch*lnstud for now.

#WHAT ABOUT FOUR-WAY!?!?!

#list: min*pulch*low*lnstud, pulch*low*tenu*lnstud

#min*pulch*low*lnstud has p-value of 0.994775 so not significant.
#pulch*low*tenu*lnstud has p-value of 0.635303 so not significant.

#FIVE WAY!?!?!?

#min*pulch*low*tenu*lnstud has p-value of 0.749672 so not significant.

#okay so final model from all this is:

intm3 = lm(eval~min+pulch+low+tenu+lnstud+pulch*lnstud)
summary(intm3)
#From this all are significant except lnstud but this must be included for pulch*lnstud to be there. ask avery
#if this is just in R? can this be excluded on paper? or?

#R^2 is 0.4028 while adjusted is 0.3779 which is quite good. 6 variables in total, including interaction.

#Unfortunately for intm3, the coefficients for beauty is negative which is contrary to paper and initial correlations.

#Testing model adequacy: #Check cook's distances and leverages at this point as well?

plot(fitted.values(intm3),rstandard(intm3),ylab="Studentised Residuals",xlab="Fitted Values")
abline(h=0)
abline(h=-2,col=2,lty=2)
abline(h=2,col=2,lty=2)

#even spread, no sign of changing variance, some points outside [-2,2], some close to -3 but number is within 5% of 151.
#can assume constant variance assumption has been validated.

qqnorm(rstandard(intm3))
abline(0,1)
#points close to line, ripples expected. tails somewhat concerning.

ad.test(rstandard(intm3))
#p-value is 0.2866 which allows us to say that normality assumption has been validated.



##WHAT IF IGNORED AIC AND MALLOW AND REMOVED LNSTUD BY MANUAL BACKWARDS ELIMINATION?

summary(m4)

plot(fitted.values(m4),rstandard(m4),ylab="Studentised Residuals",xlab="Fitted Values")
abline(h=0)
abline(h=2,col=2,lty=2)
abline(h=-2,col=2,lty=2)

#even spread, no sign of changing variance, some points outside [-2,2] and close to -3.
#constant variance assumption probably right.

qqnorm(rstandard(m4),ylab="Studentised Residuals")
abline(0,1)
#close to line, significant ripples though, tails also worrying

ad.test(rstandard(m4))
#very low p-value at 0.08092 so perhaps normality assumption is in question.

sans9m4 = lm(eval1~min1+pulch1+low1+tenu1)
summary(sans9m4)

#coefficients don't really change so no point in removing point 9.

#Now to check interactions using anova one by one:

#two-way:

#list: min*pulch, min*low, min*tenu, pulch*low, pulch*tenu, low*tenu

#min*pulch has p-value of 0.40034 so not significant.
#min*low has p-value of 0.6651983 so not significant.
#min*tenu has p-value of 0.7368720 so not significant.
#pulch*low has p-value of 0.0116438 so significant at the 5% level.
#pulch*tenu has p-value of 0.0755616 so not significant though approaching.
#low*tenu has p-value of 0.8780817 so definitely not significant.

#Now three-way:

#list: min*pulch*low, pulch*low*tenu

#min*pulch*low has p-value of 0.7127457 so insignificant.
#pulch*low*tenu has p-value of 0.0244253 so significant at the 5% level.

#combine two and three-way significant interactions in order pulch*low, pulch*low*tenu

anova(lm(eval~min+pulch+low+tenu+pulch*low+pulch*low*tenu))
#as two-way is part of three-way, not worth writing. three-way still significant. so is two of two-way including
#solo significant one but low*tenu not significant at all.

#From this, decide to keep three-way of pulch*low*tenu.

#then model is

intm4 = lm(eval~min+pulch+low+tenu+pulch*low*tenu)
summary(intm4)
#Not very many significant in this. adjusted R^2 is 0.3091. all two-way are insignificant using summary.

#Try with just pulch*low:

int2m4= lm(eval~min+pulch+low+tenu+pulch*low)
summary(int2m4)


#adjusted R^2 is significantly lower at 0.2752 but more of variables are significant using summary.

#these based on m4 have positive coefficients for beauty which agrees with paper and initial correlations.

#intm3 simply looks much better than these two. so stick with it? Test residuals of these two if want.

#int2m4 is same as int1newmod4 from naturalattempt.R which was previously considered the best model.
#It has a relatively good adjusted R^2 of 0.2752, has 5 variables which is quite small and the coefficients for pulch
#and pulch interactions are positive as seen in the paper and as expected from initial correlations using data frame.

#residuals of int2m4, should be the same as int1newmod4:

plot(fitted.values(int2m4),rstandard(int2m4),ylab="Studentised Residuals",xlab="Fitted Values")
abline(h=0)
abline(h=-2,col=2,lty=2)
abline(h=2,col=2,lty=2)

#evenly spread only a few outside so within 5% but they are close to -3 so a bit concerning. Not any signs of chaning variance.
#can likely say that constant variance assumption holds.

qqnorm(rstandard(int2m4),ylab="Studentised Residuals")
abline(0,1)

#close to line, minimal ripples are expected, tails concerning.

ad.test(rstandard(int2m4))

#p-value of 0.1969 which backs up normality assumption.