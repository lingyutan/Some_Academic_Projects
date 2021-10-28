# Ignoring censoring, result of nlm

# install.packages("optimization")

library(optimization)        

# install.packages("survival")

library(survival)

data("cancer")

#--------------Start from here---------------#
x = data.frame(cancer$time)

df = data.frame(cancer$time, cancer$status-1)

cvec <- cancer$time

ll = function(alph) { 
  
  -(228*(log(alph[1]) + log(alph[2])) + alph[1]*sum(log(cvec)) - alph[2]*sum((cvec)^(alph[1])))
  
}

nlm(ll, alph <- c(2,2), hessian=TRUE)
survfun = exp(-0.0001999296*((cvec)^1.4628397596))

plot(cvec, survfun, type = "n")

lines(cvec[order(cvec)], survfun[order(cvec)], col = "red", lwd = 2)