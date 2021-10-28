library(corrplot)

x <- seq(0, 100, 1)
# colinear with x
y <- x + 2.3 
# almost colinear with x / some small gaussian noise 
z <- x + rnorm(mean = 0, sd = 5, n = 101)
# uncorrrelated gaussian 
w <- rnorm(mean = 0, sd = 1, n = 101)

# this frame is made to exemplify the procedure
df <- data.frame(x = x, y = y, z = z, w = w)

corr.matrix <- cor(dta[, 1:4])
corrplot.mixed(corr.matrix)



vars <- c("Class","X1","X2","X3","X4")
N <- list(1, 2, 3, 4)
COMB <- sapply(N, function(m) combn(x=vars[2:5], m))
COMB2 <- list()
k=0
for(i in seq(COMB)){
  tmp <- COMB[[i]]
  for(j in seq(ncol(tmp))){
    k <- k + 1
    COMB2[[k]] <- formula(paste("Class", "~", paste(tmp[,j], collapse=" + ")))
  }
}

res <- vector(mode="list", length(COMB2))
for(i in seq(COMB2)){
  res[[i]] <- lm(COMB2[[i]], data=dta)
}

vars <- c("price","model","size","year","color")
N <- list(1,2,3,4)
COMB <- sapply(N, function(m) combn(x=vars[2:5], m))
COMB2 <- list()
k=0
for(i in seq(COMB)){
  tmp <- COMB[[i]]
  for(j in seq(ncol(tmp))){
    k <- k + 1
    COMB2[[k]] <- formula(paste("price", "~", paste(tmp[,j], collapse=" + ")))
  }
}
res <- vector(mode="list", length(COMB2))
for(i in seq(COMB2)){
  res[[i]] <- lm(COMB2[[i]], data=data)
}




