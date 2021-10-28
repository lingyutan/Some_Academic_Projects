install.packages("nclSLR", repo = "R-Forge.R-project.org")
library(nclSLR)

data(Boston, package = "nclSLR")

set.seed(18053836)
sampid = sample(dim(Boston)[1], 400)
BN = Boston[sampid, ]

#dim(BN)

head(BN, 3)
save(BN, file = "BostonNew.RData")

load("BostonNew.RData")








