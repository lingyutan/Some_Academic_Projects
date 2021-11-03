data(lung)

if(!exists("optim_nm")) library(optimization)

x = c(1, 1)
sum  = 0;

f = function(x)
{
  sum = 0
  for (i in 1 : 228)
  {
    sum = sum + log(x[1]) + log(x[2]) + (x[1]-1)*log(lung$time[i]) - x[2]*lung$time[i]^x[1]
  }
  return (sum)
}


optim_nm(f, 2)
