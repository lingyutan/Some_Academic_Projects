# MAS3906 Project Q2

setwd("~/Desktop/MAS3906")

da=read.table("ulcer2.txt",header=T)

da$place = gl(2, 4)
da$case = gl(2, 2, 8)
da$blood = gl(2, 1, 8)

ulcermod = glm(no ~ place + case + blood + place*case + case*blood, data = da, family = poisson)
anova(ulcermod, test = "Chisq")

anova(update(ulcermod, . ~ . + blood*place + place*blood*case), test = "Chisq")

summary(update(ulcermod, . ~ . + blood*place + place*blood*case), test = "Chisq")
