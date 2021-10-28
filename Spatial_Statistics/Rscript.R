setwd("~/Desktop/UCL/STAT0017 - Selected Topics/Spatial Statistics/ICA2")

library(geoR)

temp <- read.table(file = 'temp.txt', header = T)

# remove.packages("geoR")
# install.packages("devtools")
# require(devtools)
# install_version("geoR", version = "1.7-4.1", repos = "http://cran.r-project.org")
# options(gsubfn.engine = "R")
# library(geoR, verbose = TRUE)

# data(parana)

dta <- as.geodata(temp)

# Plot the variogram
plot(variog(dta, option='bin'), main='bin')

# ml <- likfit(dta, ini=c(0.5, 0.5), fix.nug = TRUE, cov.model='matern',kappa=2)
# 
# reml <- likfit(dta, ini=c(0.5, 0.5), fix.nug = TRUE, cov.model='matern',kappa=2,
# lik.met = 'REML')


coords <- as.matrix(temp[,1:2])


# mesh <- inla.mesh.2d(coords, ## provide locations or domain
#                      max.edge=c(0.3, 0.5), ## mandatory
#                      cutoff=0.3) ## good to have >0

mesh <- inla.mesh.2d(coords, max.edge=c(0.6, 1), cutoff = 0.3)

plot(mesh, asp=1)
points(coords, col='red', cex = 0.5)

# dim(A <- inla.spde.make.A(mesh=mesh, loc=temp[, 1:2]))
# 
# spde <- inla.spde2.matern(mesh=mesh, alpha=1.5)
# 
# stk.e <- inla.stack(tag='est', ## tag
#                     data=list(y=temp[, 3]), ## response
#                     A=list(A, 1), ## two projector matrix
#                     effects=list(## two elements:
#                       s=1:spde$n.spde, ## RF index
#                       data.frame(b0=1, x=temp[, 1:2])))

# Define the projector matrix
A <- inla.spde.make.A(mesh, loc=coords)


spde <- inla.spde2.matern(mesh, alpha=2)

stk <- inla.stack(data=list(resp=temp$t), A=list(A,1),
                   effects=list(i=1:spde$n.spde,
                   m=rep(1,nrow(temp))), tag='est')

res <- inla(resp ~ 0 + m + f(i, model=spde),
             data=inla.stack.data(stk),
             control.predictor=list(A=inla.stack.A(stk)))

pgrid <- inla.mesh.projector(mesh, xlim=137:148, ylim=33:44, dims=c(100,100))

prd.m <- inla.mesh.project(pgrid, res$summary.ran$i$mean)
prd.s <- inla.mesh.project(pgrid, res$summary.ran$i$s)

stkgrid <- inla.stack(data=list(resp=NA), A=list(pgrid$proj$A,1),
                      effects=list(i=1:spde$n.spde,
                      m=rep(1,100*100)), tag='prd.gr')

stk.all <- inla.stack(stk, stkgrid)

resg <- inla(resp ~ 0 + m + f(i, model=spde),
              data=inla.stack.data(stk.all),
              control.predictor=list(A=inla.stack.A(stk.all),
                                     compute=TRUE), quantiles=NULL,
              control.results=list(return.marginals.random=FALSE,
                                   return.marginals.predictor=FALSE))

igr <- inla.stack.index(stk.all, 'prd.gr')$data

require(gridExtra)
require(lattice)

grid.arrange(levelplot(prd.m, col.regions=topo.colors(99), main='latent field mean',
                       xlab='', ylab='', scales=list(draw=FALSE)),
             levelplot(matrix(resg$summary.fitt[igr,1], 100),
                       xlab='', ylab='', main='response mean',
                       col.regions=topo.colors(99), scales=list(draw=FALSE)),
             levelplot(prd.s, col.regions=topo.colors(99), main='latent field SD',
                       xlab='', ylab='', scales=list(draw=FALSE)),
             levelplot(matrix(resg$summary.fitt[igr,2], 100),
                       xlab='', ylab='', main='response SD',
                       col.regions=topo.colors(99), scales=list(draw=FALSE)),
             nrow=2)

temp.mean <- matrix(resg$summary.fitt[igr,1], 100)
temp.sd <- matrix(resg$summary.fitt[igr,2], 100)



apply(temp, 2, min)
apply(temp, 2, max)

apply(SPDEtoy, 2, min)
apply(SPDEtoy, 2, max)
#


























######### test #########

n = 200; coo = matrix(runif(2*n), n)
k <- 10; s2rf <- 0.7 ## RF params.
R <- s2rf*exp(-k*as.matrix(dist(coo)))



mesh <- inla.mesh.2d(temp[, 1:2], ## provide locations or domain
                     max.edge=c(0.3, 0.5), ## mandatory
                     cutoff=0.1) ## good to have >0
plot(mesh, asp=1); points(coo, col='red')



