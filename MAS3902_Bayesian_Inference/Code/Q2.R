library(nclbayes)

rm(list=ls())

data(hepatitis)
x = hepatitis$logoc
n = length(x)

# Problem a
qqnorm(logoc)
qqline(logoc)

# Problem b

b = 2.6
c = 1
g = 5
h = 0.4

#miu ~ t_2g(b, h/gc) ~ t_10(2.6, 0.08)
emiu = b
varmiu = (2*h/c)/(2*g-2)
sdmiu = sqrt(varmiu)

#tau ~ Ga(g, h) ~ Ga(5, 0.4)
etau = g/h
vartau = g/h^2
sdtau = sqrt(vartau)

#sigma ~ Inv-Chi(g, h) ~ Inv-Chi(5, 0.4)
esigma = sqrt(h)*gamma(g-0.5)/gamma(g)
varsigma = h/(g-1) - esigma^2
sdsigma = sqrt(varsigma)

# Problem c

B = (b*c+n*mean(x))/(c+n)
C = c + n
G = g + n/2
H = h + (c*n*(mean(x)-b)^2)/(2*(c+n)) + n*var(x)/2

#Miu ~ t_2G(B, H/GC)
eMiu = B
varMiu = (2*H/C)/(2*G-2)
sdMiu = sqrt(varMiu)

#Tau ~ Ga(G, H)
eTau = G/H
varTau = G/(H^2)
sdTau = sqrt(varTau)

#Sigma ~ Inv-Chi(G, H)
eSigma = sqrt(H)*gamma(G-0.5)/gamma(G)
varSigma = H/(G-1) - eSigma^2
sdSigma = sqrt(varSigma)

# Problem d

par(mfrow = c(2, 2))

#For miu

xmiu = seq(1.5, 4, len = 1000)
ymiu = dgt(xmiu, 2*g, b, h/(g*c))
yMiu = dgt(xmiu, 2*G, B, H/(G*C))
plot(xmiu, yMiu, type = "l", col = "blue", xlab = expression(mu), ylab = "density")
lines(xmiu, ymiu, lty = 3)

#For tau

xtau = seq(0, 35, len = 1000)
ytau = dgamma(xtau, g, h)
yTau = dgamma(xtau, G, H)
plot(xtau, yTau, type = "l", col = "blue", xlab = expression(tau), ylab = "density")
lines(xtau, ytau, lty = 3)

#For sigma

xsigma = seq(0.1, 0.7, len = 1000)
ysigma = dinvchi(xsigma, g, h)
ySigma = dinvchi(xsigma, G, H)
plot(xsigma, ySigma, type = "l", col = "blue", xlab = expression(sigma), ylab = "density")
lines(xsigma, ysigma, lty = 3)

#(joint) prior and posterior densities for (miu, tau)
par(mfrow = c(1, 1))

miu = seq(1.8, 3.4, len = 1000)
tau = seq(0, 30, len = 1000)
NGacontour(miu, tau, b, c, g, h, lty = 3)
abline(h = etau, col="red")
abline(v = emiu, col="red")
NGacontour(miu, tau, B, C, G, H, add = T, col = "blue")
abline(h = eTau, col="blue")
abline(v = eMiu, col="blue")


# Problem e

NGacontour(miu, tau, b, c, g, h, p=c(0.95, 0.9, 0.8), lty=3, col="red")
NGacontour(miu, tau, B, C, G, H, p=c(0.95, 0.9, 0.8), add=TRUE, col="blue")

##Zoom in:
NGacontour(miu, tau, b, c, g, h, p=c(0.95, 0.9, 0.8), lty=3, col="red", xlim=c(2.3, 2.7), ylim=c(4, 24))
NGacontour(miu, tau, B, C, G, H, p=c(0.95, 0.9, 0.8), add=TRUE, col="blue")

# Problem f
#Comments omitted

# Problem g

p2.7 = 1 - pgt(2.7, 2*g, b, h/(g*c))
P2.7 = 1 - pgt(2.7, 2*G, B, H/(G*C))

# Problem h

#Ybar| xvector ~ t_2G(B, H*(C+m)/(G*C*m) )

# Problem i

m = 20
# xpred -> Ybar, ypred -> density
xpred = seq(2, 3, len = 1000)
ypred = dgt(xpred, 2*G, B, H*(C+m)/(G*C*m))
plot(xpred, ypred, type = "l", xlab = expression(bar(Y)), ylab = "density")
c(qgt(0.025, 2*G, B, H*(C+m)/(G*C*m)), qgt(0.975, 2*G, B, H*(C+m)/(G*C*m)))


# Problem j
# paperwork


