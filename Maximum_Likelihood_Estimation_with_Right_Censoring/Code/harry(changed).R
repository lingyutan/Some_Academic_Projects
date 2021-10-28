# Considering censoring, result of nlm


if(!exists("cancer")) library(survival)

data("cancer")

#--------------Start from here---------------#

timex <- cancer$time

#-----------Censored or not-----------#
# Considering Censoring 
statusx <- cancer$status - 1

# Ignoring Censoring
# statusx <- rep(1, length(timex))
#-------------------------------------#

ll = function(alph){ 
  sum = 0
  for (i in 1 : 228)
  {
    # sum = sum - (log((alph[1]*alph[2]*(timex[i]^(alph[1]-1)))^statusx[i] * exp(-alph[2]*timex[i]^alph[1])))
    sum = sum - (statusx[i]*(log(alph[1]) + log(alph[2]) + (alph[1]-1) * log(timex[i])) - alph[2]*(timex[i]^alph[1]))
  }
  return(sum)
}

nlm(ll, alph <- c(2,2), hessian=TRUE)


survfun = exp(-0.000359415*((timex)^1.313612382))

# timex = sort(timex)
# survfun = sort(survfun)
plot(timex, survfun, type = "n", xlab = "time", ylab = "survival rate")

lines(timex[order(timex)], survfun[order(timex)], col = "red", lwd = 2)
