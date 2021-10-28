# R Script for MAS3907
# author Man Kuan Io, Lingyu Tan
# last-edited 3/4/2020

library(ElemStatLearn)
library(leaps)
library(glmnet)
library(pls)
library(nclSLR)

data(Boston, package="nclSLR")
set.seed(18053836)

sampid = sample(dim(Boston)[1], 400)

BostonNew = Boston[sampid, ]
n = nrow(BostonNew)
p = ncol(BostonNew) - 1

# reponse variable: 
# logarithm of the per capita crime rate (lcrim)
y = BostonNew[, 1]
X_raw = BostonNew[, 2:14]
X = scale(X_raw)

Boston_data = data.frame(y, X)

#********************************************#
#*            Data Exploration			      	*#
#********************************************#
#pairs(BostonNew)
means = colMeans(BostonNew)
covariances = var(BostonNew)
correlations = cor(BostonNew)
round(correlations, 3)

#********************************************#
#*			   Extra functions			          	*#
#********************************************#
predict.regsubsets = function(object, newdata, id, ...) {
	form = as.formula(object$call[[2]])
	mat = model.matrix(form, newdata)
	coefi = coef(object, id=id)
	xvars = names(coefi)
	return(mat[,xvars] %*% coefi)
}
#********************************************#
#*         Identifying Best Model		      	*#
#********************************************#

#############################################
# kfolds variables						            	#
# Only one kfold should be used throughout. #
#############################################
nfolds = 10
set.seed(18053836)
fold_index = sample(nfolds, n, replace=TRUE)
fold_sizes = numeric(nfolds)
fold_list = vector(mode = "list", length = 10)

for(k in 1:nfolds){
	fold_sizes[k] = length(which(fold_index==k))
}

for(i in 1:400)
{
  temp = fold_index[i]
  fold_list[[temp]] = c(fold_list[[temp]], i)
}

#######################################
# Best Subset Selection (mse = 0.597) #
#######################################

bss_fit = regsubsets(y ~., data=Boston_data, method="exhaustive", nvmax=p)
bss_summary = summary(bss_fit)

best_adjr2=which.max(bss_summary$adjr2)
best_cp=which.min(bss_summary$cp)
best_bic=which.min(bss_summary$bic)

par(mfrow=c(1,3))
plot(1:p, bss_summary$adjr2,xlab="Number of predictors",ylab="Adjusted Rsq",type="b")
# text(1:p, bss_summary$adjr2, labels=round(bss_summary$adjr2, 3), cex= 0.7, pos=1)
points(best_adjr2, bss_summary$adjr2[best_adjr2],col="red",pch=16)
plot(1:p, bss_summary$cp,xlab="Number of predictors",ylab="Cp",type="b")
points(best_cp, bss_summary$cp[best_cp],col="red",pch=16)
plot(1:p, bss_summary$bic,xlab="Number of predictors",ylab="BIC",type="b")
points(best_bic, bss_summary$bic[best_bic],col="red",pch=16)

bss_coeff = coef(bss_fit, 6)

# print(bss_coeff)
# print(bss_summary)

# Finding best bss_fit using cross-validation
cv_bss_errors = matrix(NA, p, nfolds)
bss_mse = numeric(p)

for(k in 1:nfolds) {
  bss_tmp_fit = regsubsets(y ~ ., data=Boston_data[fold_index!=k,], method="exhaustive", nvmax=p)
  
  for(m in 1:p) {
    bss_tmp_predict = predict(bss_tmp_fit, Boston_data[fold_index==k,], m)
    
    cv_bss_errors[m, k] = mean((Boston_data[fold_index==k,]$y - bss_tmp_predict)^2)
  }
}

for(m in 1:p) {
  
  bss_mse[m] = weighted.mean(cv_bss_errors[m,], w=fold_sizes)
  
}

best_bss_cv = which.min(bss_mse)
best_bss_mse = min(bss_mse)

print(best_bss_mse)

par(mfrow=c(1, 1))
plot(1:p, bss_mse,xlab="Number of predictors",ylab="10-fold CV Error",type="b")
points(best_bss_cv, bss_mse[best_bss_cv],col="red",pch=16)

####################
# Ridge Regression #
####################

grid=10^seq(5,-3,length=100)

ridge_fit=glmnet(X, y,alpha=0,standardize=FALSE,lambda=grid)
ridge_summary = summary(ridge_fit)
beta1_hat = coef(ridge_fit)

# plot for the effect of the tuning parameter by ridge regression
par(mfrow = c(1,1))

plot(ridge_fit,xvar="lambda",col=1:p,label=TRUE)

# Using cv.glmnet to find lambda_min
ridge_cv_fit=cv.glmnet(X, y, alpha=0,standardize=FALSE,lambda=grid, foldid = fold_index)

plot(ridge_cv_fit)

best_lambda = ridge_cv_fit$lambda.min
best_lambda_idx = which(ridge_cv_fit$lambda==ridge_cv_fit$lambda.min)
best_lambda_coeff = coef(ridge_fit, s = best_lambda)

#########################
# Partial Least Squares #
#########################

pls_fit = plsr(y~.,data=Boston_data,scale=FALSE)
pls_load = loadings(pls_fit)
#pls_summary = summary(pls_fit)

C = unclass(pls_load)
pls_yload = Yloadings(pls_fit)
theta1_hat = unclass(pls_yload)

pls_coeff = coef(pls_fit, intercept = T, ncomp = 2)
# round(pls_coeff, 3) 

# Used to find the optimal number of components
pls_cv_fit = plsr(y ~ ., data = Boston_data, scale = FALSE, validation = "CV", segments = fold_list)

plot(pls_cv_fit,plottype="validation",legend="topright",val.type="MSEP")

pls_msep = MSEP(pls_cv_fit)
