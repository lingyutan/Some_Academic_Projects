#------------------#
# MAS3907 Project
#------------------#

library(nclSLR)
library(leaps) # bss
library(glmnet) # LASSO
library(pls) # PLS

data(Boston, package = "nclSLR")

set.seed(18053836)
sampid = sample(dim(Boston)[1], 400)
BN = Boston[sampid, ]

# dim(BN)
# head(BN, 3)

pairs(BN, cex.labels = 2) # Multivariate data

a <- round(cor(BN),3) # Correlation Matrix

plot(BN$disf, BN$lcrim, pch = 1, cex = 0.8) # scatterplot of lcrim & disf

fit <- lm(BN$lcrim ~ BN$disf)
summary(fit)
abline(fit)

n = nrow(BN)
p = ncol(BN) - 1

y <- BN[, 1] # Extract response variable

X1_raw <- BN[, 2:14]
# class(X1_temp)
X1_raw <- as.matrix(X1_raw)
X1 <- scale(X1_raw)

## Create single data frame
BN_data <- data.frame(y, X1)

## First linear model
fit1 <- lm(y ~ ., data = BN_data)
summary(fit1)


#-----------------------------#
set.seed(18053836)
nfolds = 10
fold_index <- sample(nfolds, 400, replace = TRUE)
# head(fold_index)
fold_sizes <- numeric(nfolds)

for (k in 1 : nfolds)
{
  fold_sizes[k] <- length(which(fold_index == k))
}


#------------END--------------#


#---------------Best subset selection--------------#

bss_fit <- regsubsets(y ~ ., data = BN_data, method = "exhaustive", nvmax = p)
(bss_summary <- summary(bss_fit))

best_adjr2 <- which.max(bss_summary$adjr2)
best_cp <- which.min(bss_summary$cp)
best_bic <- which.min(bss_summary$bic)

## Compare three statistics
par(mfrow = c(1, 3))

plot(1:p, bss_summary$adjr2, xlab = "Number of predictors", ylab = "Adjusted Rsq", type = "b")
points(best_adjr2, bss_summary$adjr2[best_adjr2], col = "red", pch = 16)
plot(1:p, bss_summary$cp, xlab = "Number of predictors", ylab = "Mallow's Cp", type = "b")
points(best_cp, bss_summary$cp[best_cp], col = "red", pch = 16)
plot(1:p, bss_summary$bic, xlab = "Number of predictors", ylab = "BIC", type = "b")
points(best_bic, bss_summary$bic[best_bic], col = "red", pch = 16)

par(mfrow = c(1, 1))

## In summary, we may choose the model with 6 indep. variables, then cross-validation
coef(bss_fit, 6)

## Self-defined function
predict.regsubsets = function(object, newdata, id, ...) 
{
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  return(mat[,xvars] %*% coefi) 
}

cv_bss_errors <- matrix(NA, p, nfolds)

for (k in 1 : nfolds)
{
  bss_tmp_fit = regsubsets(y ~ ., data = BN_data[fold_index!=k,], method = "exhaustive", nvmax = p)
  
  for(m in 1 : p) 
  {
    bss_tmp_predict = predict(bss_tmp_fit, BN_data[fold_index==k,], m)
    cv_bss_errors[m, k] = mean((BN_data[fold_index==k,]$y - bss_tmp_predict)^2)
  }
}

bss_mse = numeric(p)

for(m in 1 : p) 
{
  bss_mse[m] = weighted.mean(cv_bss_errors[m,], w=fold_sizes) 
}

best_bss_cv = which.min(bss_mse)

plot(1:p, bss_mse, xlab="Number of predictors", ylab="10-fold CV Error", type="b") 
points(best_bss_cv, bss_mse[best_bss_cv], col="red", pch=16)

#----------------------END-------------------------#


#----------------------LASSO-----------------------#
grid=10^seq(5,-3,length=100)
cv_ridge_errors = matrix(NA, p, nfolds)
ridge_mse = numeric(p)

for(k in 1:nfolds) {
  
  ridge_tmp_fit = glmnet(X[fold_index!=k,], y[fold_index!=k], alpha=0, standardize=FALSE, lambda=grid)
  
  for(m in 1:p) {
    ridge_tmp_predict = predict(ridge_tmp_fit, as.matrix(X[fold_index==k,]), m)
    
    cv_ridge_errors[m, k] = mean((Boston_data[fold_index==k,]$y - ridge_tmp_predict)^2)
  }
}

for(m in 1:p) {
  
  ridge_mse[m] = weighted.mean(cv_ridge_errors[m,], w=fold_sizes)
  
}

best_ridge_mse = min(ridge_mse)

print(best_ridge_mse)




#-----------------------END------------------------#



#------------Compare three models using cv------------#






#-----------------------END---------------------------#


