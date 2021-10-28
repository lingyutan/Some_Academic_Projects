# Amend_0

## fold_index - convert to list
fold_list <- vector(mode = "list", length = 10)
for(i in 1:400)
{
  temp <- fold_index[i]
  fold_list[[temp]] <- c(fold_list[[temp]], i)
}


## cv for PLS
plsr_cv_fit <- plsr(y ~ ., data = Boston_data, scale = F, validation = "CV", segments = fold_list)
plot(plsr_cv_fit, plottype = "validation", legend = "topright", val.type = "MSEP")
MSEP(plsr_cv_fit)


## Ridge Regression
grid <- 10^seq(5, -3, length = 100)
ridge_fit <- glmnet(X, y, alpha = 0, standardize = F, lambda = grid)

beta1_hat = coef(ridge_fit)
# beta1_hat[, 1]
# beta1_hat[, 75]
# beta1_hat[, 100]

plot(ridge_fit, xvar = "lambda", col = 1 : p, label = T)

## cv for Ridge
ridge_cv_fit <- cv.glmnet(X, y, alpha = 0, standardize = F, lambda = grid, foldid = fold_index)

# plot(ridge_cv_fit)

(lambda_min <- ridge_cv_fit$lambda.min)
(i = which(ridge_cv_fit$lambda == ridge_cv_fit$lambda.min))

ridge_cv_fit$cvm[i]

# coef(ridge_fit, s = lambda_min)

