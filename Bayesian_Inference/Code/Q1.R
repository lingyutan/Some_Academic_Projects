library(nclbayes)

# Problem a
b1 = 3.3; d1 = 1/0.37^2; p1 = 0.2
b2 = 1.1; d2 = 1/0.47^2; p2 = 0.8
n = 10; tau = 1

xbar = c(2.0, 2.3, 2.4, 2.5, 2.8)

B1 = (d1*b1+n*xbar*tau)/(d1+n*tau)
B2 = (d2*b2+n*xbar*tau)/(d2+n*tau)

D1 = d1+n*tau
D2 = d2+n*tau

temp = (p2*sqrt(D1*d2))/(p1*sqrt(D2*d1))*exp((D2*B2^2-d2*b2^2-D1*B1^2+d1*b1^2)/2)

P1 = 1 / (temp + 1)
P2 = 1 / (1/temp + 1)

e = p1*b1+p2*b2
e2 = p1*(1/d1+b1^2) + p2*(1/d2+b2^2)
sd_miu = sqrt(e2 - e^2)

E = P1*B1+P2*B2
E2 = P1*(1/D1+B1^2) + P2*(1/D2+B2^2)
Sd_miu = sqrt(E2 - E^2)


pr = p1*(1-pnorm(2.5, b1, 1/sqrt(d1))) + p2*(1-pnorm(2.5, b2, 1/sqrt(d2)))
Pr = P1*(1-pnorm(2.5, B1, 1/sqrt(D1))) + P2*(1-pnorm(2.5, B2, 1/sqrt(D2)))

# Problem b
x <- seq(-1, 5, 0.001)
xdensity <- p1*dnorm(x, b1, 1/sqrt(d1)) + p2*dnorm(x, b2, 1/sqrt(d2))
plot(x, xdensity, type = "l", xlim = c(-1, 5), ylim = c(0, 1.6))

# Xdensity <- P1*dnorm(x, B1, 1/sqrt(D1)) + p2*dnorm(x, B2, 1/sqrt(D2))
# lines(x, Xdensity, lty = 2)

Xdensity <- matrix(nrow = 5, ncol = length(x)) # Density Matrix
for (i in 1 : 5)
{
  Xdensity[i, ] <- P1[i]*dnorm(x, B1[i], 1/sqrt(D1)) + P2[i]*dnorm(x, B2[i], 1/sqrt(D2))
  lines(x, Xdensity[i, ], lty = 2)
}

# Problem c

# Observing the sample means has substantially move the whole density and its mode towrds right.
# and the density is more consentrated.
# Also, Probability(miu > 2.5|xvector) has significantly increased.

# Problem d
xd <- seq(0, 30, 0.01)

B1 = (d1*b1+n*xd*tau)/(d1+n*tau)
B2 = (d2*b2+n*xd*tau)/(d2+n*tau)

D1 = d1+n*tau
D2 = d2+n*tau

temp = (p2*sqrt(D1*d2))/(p1*sqrt(D2*d1))*exp((D2*B2^2-d2*b2^2-D1*B1^2+d1*b1^2)/2)

test = exp((D2*B2^2-d2*b2^2-D1*B1^2+d1*b1^2)/2)

Pd1 = 1 / (temp + 1)

plot(xd, Pd1, type = "l")
# plot(xd, temp, type = "l",  ylim = c(0, 1e+10))
# plot(xd, test, type = "l", ylim = c(0, 1e+10))
# plot(xd, B1, type = "l")
# plot(xd, B2, type = "l")
# 2*n*tau*d1*d2*(b2-b1)+2*n^2*tau^2*(b2*d2-b1*d1)
# n^2*tau^2*(d1-d2)



gx = (n^2*tau^2*(d1-d2)*xd^2+(2*n^2*tau^2*(d2*b2-d1*b1)+2*n*tau*d1*d2*(b2-b1))*xd)/(2*(d2+n*tau)*(d1+n*tau))
plot(xd, gx, type = "l", xlab = expression(bar(x)), ylab = expression(g(bar(x))))
lines(c(0, 30), c(0, 0), lty = 3)

# 
# C2 = (p2*sqrt(D1*d2))/(p1*sqrt(D2*d1))*exp(0.5*(d1*b1^2)-0.5*(d2*b2^2)+(d1*d2*(d2*b2^2-d1*b1^2)+n*tau*(d2^2*b2^2-d1^2*b1^2))/(2*(d2+n*tau)*(d1+n*tau)) )
#
# plot(xd, 1/(C2*exp(gx)+1), type = "l", xlab = expression(exp(bar(x))), ylab = expression(g(bar(x))))
# 
# -(2*n*tau*(d2*b2-d1*b1)+2*d1*d2*(b2-b1))/(n*tau*(d1-d2))
