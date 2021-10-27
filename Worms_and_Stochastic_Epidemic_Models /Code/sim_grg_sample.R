set.seed(2021)

get.dist = function(x)
{
  sqrt(x[1]^2 + x[2]^2)
}

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
  M
}

create.graph = function(N = 100, d = 0.3)
{
  rnd.points = matrix(runif(2 * N), ncol = 2)
  perms = get.pairs(N)
  
  Edges = apply(perms, 1, FUN = function(x){
    vec.diff = rnd.points[x[1], ] - rnd.points[x[2], ]
    get.dist(vec.diff)
  })
  
  res = cbind(Edges, perms)
  colnames(res) = c('E', 'V1', 'V2')
  list(M = res, N = N, d = d, pts = rnd.points)  
}

create.graph.nb = function(N = 100, d = 0.3)
{
  rnd.points = matrix(runif(2 * N), ncol = 2)
  rnd.points.nb = rbind(t(t(rnd.points)+c(-1,-1)), t(t(rnd.points)+c(-1,0)),
                        t(t(rnd.points)+c(-1,1)), t(t(rnd.points)+c(0,1)),
                        t(t(rnd.points)+c(1,1)), t(t(rnd.points)+c(1,0)), 
                        t(t(rnd.points)+c(1,-1)), t(t(rnd.points)+c(0,-1)),
                        rnd.points)
  
  perms.nb = get.pairs(9*N)
  # perms.nb = do.call(rbind, replicate(9, perms, simplify=FALSE))
  
  Edges = apply(perms.nb, 1, FUN = function(x){
    vec.diff = rnd.points.nb[x[1], ] - rnd.points.nb[x[2], ]
    get.dist(vec.diff)
  })
  
  res = cbind(Edges, perms.nb)
  colnames(res) = c('E', 'V1', 'V2')
  graph.obj = list(M = res, N = 9*N, d = d, pts = rnd.points.nb)  
}


connection = function(graph.obj)
{
  N = graph.obj$N / 9
  d = graph.obj$d
  M = graph.obj$M
  
  perms = get.pairs(N)
  con = cbind(perms, 0)
  colnames(con) = c('V1', 'V2', 'C')
  con = as.data.frame(con)
  
  for (r in 1 : nrow(M))
  {
    if (M[r, 1] <= d)
    {
      small = min(c(M[r, 2] %% 100, M[r, 3] %% 100))
      large = max(c(M[r, 2] %% 100, M[r, 3] %% 100))
      con$C[which(con$V1 == small & con$V2 == large)] = 1
    }
  }
  return(con)
}


plot.graph = function(graph.obj)
{
  N = graph.obj$N / 9
  d = graph.obj$d
  M = graph.obj$M
  rnd.points = graph.obj$pts
  
  caption = paste('G(N, d) : (', N, ', ', d, ')', sep = '')
  plot(rnd.points, xlim = c(0,1), ylim = c(0,1), bty = "n",
       type = 'p', pch = 21, bg = 'red', main = caption, cex = 0.5, xaxs="i", yaxs="i")
  for (r in 1:nrow(M))
  {
    if (M[r, 1] <= d)
    {
      v1 = M[r, 2]
      v2 = M[r, 3]
      segments(rnd.points[v1, 1], rnd.points[v1, 2], rnd.points[v2, 1],
               rnd.points[v2, 2], lwd = 0.5, col = "#9900FF")
    }
  }
}

plot.graph.nb = function(graph.obj)
{
  plot.graph(graph.obj)
  rect(0, 0, 1, 1)
}


draw.graph = function(N = 100, d=0.3)
{
  g = create.graph.nb(N, d)
  plot.graph.nb(g)
  return(connection(g))
}


draw.graph(100, 0.15)

