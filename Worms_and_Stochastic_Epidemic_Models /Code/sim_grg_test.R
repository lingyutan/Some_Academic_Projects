set.seed(2021)

get.pairs = function(N)
{
  M = matrix(0, nrow = N * (N - 1)/2, ncol = 2)
  x = 1:N
  k = 1
  
  for (i in head(x, -1))
  {
    for (j in (i + 1):(length(x)))
    {
      M[k, ] = c(i, j)
      k = k +1
    }
  }
  return(M)
}


N = 10000
d = 0.15

rnd.points = matrix(runif(2 * N), ncol = 2)

# hist(rnd.points[,1])
# hist(rnd.points[,2])

perms = get.pairs(N)

# Edges = apply(perms, 1, FUN = function(x){
#   vec.diff = rnd.points[x[1], ] - rnd.points[x[2], ]
#   get.dist(vec.diff)
# })

Edges = dist(rnd.points)

res = cbind(Edges, perms)
colnames(res) = c('E', 'V1', 'V2')
g <- list(M = res, N = N, d = d, pts = rnd.points)  

M = g$M

caption = paste('G(N, d) : (', N, ', ', d, ')', sep = '')
plot(rnd.points, type = 'p', pch = 21, bg = 'red', main = caption, cex = 1)

for (r in 1:nrow(M)){
  if (M[r, 1] <= d){
    v1 = M[r, 2]
    v2 = M[r, 3]
    segments(rnd.points[v1, 1], rnd.points[v1, 2], rnd.points[v2, 1],
             rnd.points[v2, 2], lwd = 0.5, col = "#0000FF")
  }
}




