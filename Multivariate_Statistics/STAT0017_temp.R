setwd("~/Desktop/UCL/STAT0017 - Selected Topics/Multivariate Statistics/ICA1")

library(CCA)
library(MASS)
# library(ISLR)
# data(Smarket)


rm(list = ls())

dta <- read.csv("icadata.csv", header = T)

# cov(dta[, 1:4])

## Question 1 - PCA

round(apply(dta[, 1:4], 2, var), 2)

PCA <- prcomp(dta[, 1:4])
PCA
# summary(PCA)


round(cov(PCA$x), 5)
round(lambda <- PCA$sdev^2, 5)


sum(diag(cov(PCA$x)))
sum(apply(dta[, 1:4], 2, var))


cov(PCA$x, dta[, 1:4]) 
t(PCA$rotation) * lambda


## Question 2 - CCA

set1 <- dta[, 1:2]
set2 <- dta[, 3:4]


CCA <- cc(set1, set2)

CCA$xcoef
CCA$ycoef
CCA$cor

U <- CCA$scores$xscores
V <- CCA$scores$yscores
cov(cbind(U, V))
round(cov(cbind(U, V)), 4)




## Question 4 - LDA & QDA

train.set <- as.logical(c(rep(1, 40), rep(0, 110)))
test.set <- dta[!train.set, ]

lda.fit <- lda(Class ~ ., data = dta, subset = train.set)

lda.pred <- predict(lda.fit, test.set)

# names(lda.pred)
# lda.pred$posterior[1:10, ]
# lda.pred$class[1:10]

lda.class <- lda.pred$class

dta.class <- test.set[, "Class"]
table(lda.class, dta.class)
mean(lda.class == dta.class)


lda.fit2 <- lda(Class ~ ., data = dta, subset = train.set)
lda.pred2 <- predict(lda.fit2, test.set)
lda.class2 <- lda.pred2$class

table(lda.class2, dta.class)
mean(lda.class2 == dta.class)



qda.fit <- qda(Class ~ ., data = dta, subset = train.set)
qda.pred <- predict(qda.fit, test.set)
qda.class <- qda.pred$class

table(qda.class, dta.class)
mean(qda.class == dta.class)



qda.fit2 <- qda(Class ~ .^2, data = dta, subset = train.set)
qda.pred2 <- predict(qda.fit2, test.set)
qda.class2 <- qda.pred2$class

table(qda.class2, dta.class)
mean(qda.class2 == dta.class)


cov.A <- cov(dta[dta$Class == 'A', 1:4])
cov.B <- cov(dta[dta$Class == 'B', 1:4])

equalCovs(as.matrix(dta[dta$Class == 'A', 1:4]), as.matrix(dta[dta$Class == 'A', 1:4]), 100, 50)

testCov(dta[dta$Class == 'A', 1:4], dta[dta$Class == 'B', 1:4], method = "CLX")



## Question 5 - Extension of QDA

dta2 <- dta                                                   # Replicate dataset

dta2$Y1 <- (t(PCA$rotation[, 1:2]) %*% t(dta2[, 1:4]))[1, ]   # Calculate Y1 using PC1
dta2$Y2 <- (t(PCA$rotation[, 1:2]) %*% t(dta2[, 1:4]))[2, ]   # Calculate Y2 using PC2
test.set2 <- dta2[!train.set, ]                               # Define test set

# Calculate the accuracy
qda.fit3 <- qda(Class ~ Y1 + Y2, data = dta2, subset = train.set)
qda.pred3 <- predict(qda.fit3, test.set2)
qda.class3 <- qda.pred3$class
cat("The classification accuracy rate for QDA Method 1 is", 
    round(mean(qda.class3 == dta.class)*100, 3), "%.")



vars <- c("Class","X1","X2","X3","X4")                      # Store variable names
N <- list(1, 2, 3, 4)                                       # Define element indices
COMB <- sapply(N, function(m) combn(x=vars[2:5], m))        # Generate all combinations of elements
COMB2 <- list()                                             # Store all possible formulas
k = 0                                                       # Iteration number

# Generate all possible formulas
for(i in seq(COMB)){
  tmp <- COMB[[i]]
  for(j in seq(ncol(tmp))){
    k <- k + 1
    COMB2[[k]] <- formula(paste("Class", "~", paste(tmp[,j], collapse=" + ")))
  }
}

# Calculate the accuracy for all possible combinations
res <- vector(mode="list", length(COMB2))
res.pred <- vector(mode="list", length(COMB2))
res.class <- vector(mode="list", length(COMB2))
res.acc <- vector(mode="numeric", length(COMB2))

for(i in seq(COMB2)){
  res[[i]] <- qda(COMB2[[i]], data = dta, subset = train.set)
  res.pred[[i]] <- predict(res[[i]], test.set)
  res.class[[i]] <- res.pred[[i]]$class
  res.acc[i] <- mean(res.class[[i]] == dta.class)
}

opt <- which(res.acc == max(res.acc))           # The optimal subset with largest accuracy
res[opt]                                        # Group means of the selected subset
mean(res.class[[opt]] == dta.class)             # Accuracy of the optimal subset




