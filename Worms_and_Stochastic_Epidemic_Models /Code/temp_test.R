require(igraph)
require(xlsx)
require(plotly)

# Run the code using multi-cores if applicable
# require(foreach)
# require(doParallel)
# registerDoParallel(3)



### Get the distance of two points using the difference of x&y coordinates
get.dist = function(x, y)
{
  sqrt(x^2 + y^2)
}



### Move a group of points and recalculate edges without R
move = function(g, id.m, R)
{
  if(length(id.m) == 0) return(g)
  
  for (i in 1:length(id.m))
  {
    d <- rnorm(1, 0.3, 0.01)
    angle <- runif(1, 0, 2*pi)
    
    V(g)$x[id.m[i]] <- V(g)$x[id.m[i]] + cos(angle) * d
    V(g)$y[id.m[i]] <- V(g)$y[id.m[i]] + sin(angle) * d
    
    if (V(g)$x[id.m[i]] < 0) V(g)$x[id.m[i]] = V(g)$x[id.m[i]] + 1
    if (V(g)$x[id.m[i]] > 1) V(g)$x[id.m[i]] = V(g)$x[id.m[i]] - 1
    if (V(g)$y[id.m[i]] < 0) V(g)$y[id.m[i]] = V(g)$y[id.m[i]] + 1
    if (V(g)$y[id.m[i]] > 1) V(g)$y[id.m[i]] = V(g)$y[id.m[i]] - 1
  }
  
  g <- g %>% delete_edges(E(g)[.inc(1:num)])
  
  SI <- (1:num)[!(1:num) %in% R]
  
  SI.num <- length(SI)
  
  if (SI.num == 1 | SI.num == 0) return(g) # One last infected individual remained
  
  for (i in 1:(SI.num-1))
  {
    for (j in (i+1) : SI.num)
    {
      dist.x <- abs(V(g)[SI[i]]$x - V(g)[SI[j]]$x)
      dist.y <- abs(V(g)[SI[i]]$y - V(g)[SI[j]]$y)
      
      if (dist.x > 0.5) dist.x <- 1 - dist.x
      if (dist.y > 0.5) dist.y <- 1 - dist.y
      
      if (get.dist(dist.x, dist.y) <= r)
      {
        g <- g %>% add_edges(c(SI[i], SI[j]))
      }
    }
  }
  
  return(g)
}



### Move a group of points and recalculate edges only between S and I
move.simple = function(g, id.m, S, I)
{
  if(length(id.m) == 0) return(g)
  
  for (i in 1:length(id.m))
  {
    d <- rnorm(1, 0.3, 0.01)
    angle <- runif(1, 0, 2*pi)
    
    V(g)$x[id.m[i]] <- V(g)$x[id.m[i]] + cos(angle) * d
    V(g)$y[id.m[i]] <- V(g)$y[id.m[i]] + sin(angle) * d
    
    if (V(g)$x[id.m[i]] < 0) V(g)$x[id.m[i]] = V(g)$x[id.m[i]] + 1
    if (V(g)$x[id.m[i]] > 1) V(g)$x[id.m[i]] = V(g)$x[id.m[i]] - 1
    if (V(g)$y[id.m[i]] < 0) V(g)$y[id.m[i]] = V(g)$y[id.m[i]] + 1
    if (V(g)$y[id.m[i]] > 1) V(g)$y[id.m[i]] = V(g)$y[id.m[i]] - 1
  }
  
  g <- g %>% delete_edges(E(g)[.inc(1:num)])
  
  if(length(S) == 0 | length(I) == 0) return(g)
  
  for (i in 1 : length(S))
  {
    for (j in 1 : length(I))
    {
      dist.x <- abs(V(g)[S[i]]$x - V(g)[I[j]]$x)
      dist.y <- abs(V(g)[S[i]]$y - V(g)[I[j]]$y)
      
      if (dist.x > 0.5) dist.x <- 1 - dist.x
      if (dist.y > 0.5) dist.y <- 1 - dist.y
      
      if (get.dist(dist.x, dist.y) <= r)
      {
        g <- g %>% add_edges(c(S[i], I[j]))
      }
    }
  }
  
  return(g)
}



### Move points without calculating edges
move.noedge = function(g, id.m)
{
  d <- rnorm(1, 0.3, 0.01)
  angle <- runif(1, 0, 2*pi)
  
  V(g)$x[id.m] <- V(g)$x[id.m] + cos(angle) * d
  V(g)$y[id.m] <- V(g)$y[id.m] + sin(angle) * d
  
  if (V(g)$x[id.m] < 0) V(g)$x[id.m] = V(g)$x[id.m] + 1
  if (V(g)$x[id.m] > 1) V(g)$x[id.m] = V(g)$x[id.m] - 1
  if (V(g)$y[id.m] < 0) V(g)$y[id.m] = V(g)$y[id.m] + 1
  if (V(g)$y[id.m] > 1) V(g)$y[id.m] = V(g)$y[id.m] - 1
  
  return(g)
}



### Choose every entry in vector x with probability p
choosep = function(x, p)
{
  res <- numeric(length = 0)
  
  if (length(x) == 0) return(res)
  
  prob <- runif(length(x))
  for (i in 1:length(x)) {
    if (prob[i] < p)
    {
      res <- append(res, x[i])
    }
  }
  return(res)
}



### Remove all the edges across the boundary (for figures showing unbordered edges)
boundary = function(g, r)
{
  temp.data <- as_data_frame(g, "both")
  
  id.remove <- numeric(0)
  
  for (i in 1:length(E(g))) {
    
    dist.x <- V(g)[temp.data$edges$from[i]]$x - V(g)[temp.data$edges$to[i]]$x
    dist.y <- V(g)[temp.data$edges$from[i]]$y - V(g)[temp.data$edges$to[i]]$y
    
    if (get.dist(dist.x, dist.y) > r)
    {
      id.remove <- append(id.remove, i)
    }
  }
  
  g <- g %>% delete_edges(id.remove)
  
  return(g)
}




### Without Movements with tracking I in details
### Only used when analyzing what happened during an epidemic
{
  num = 100  # Number of nodes
  r = 0.13   # Infective distance
  
  beta = 6   # Infection rate
  delta = 2  # Recovery rate
  
  ##################################
  
  time.begin <- Sys.time()
  record = numeric()
  
  # set.seed(2020)
  set.seed(714)
  g1 <- sample_grg(num, r, coords = TRUE)
  
  g9 = g8 = g7 = g6 = g5 = g4 = g3 = g2 = g1
  
  V(g2)$x <- V(g1)$x - 1
  V(g2)$y <- V(g1)$y + 1
  
  V(g3)$x <- V(g1)$x 
  V(g3)$y <- V(g1)$y + 1
  
  V(g4)$x <- V(g1)$x + 1
  V(g4)$y <- V(g1)$y + 1
  
  V(g5)$x <- V(g1)$x - 1
  V(g5)$y <- V(g1)$y 
  
  V(g6)$x <- V(g1)$x + 1
  V(g6)$y <- V(g1)$y 
  
  V(g7)$x <- V(g1)$x - 1
  V(g7)$y <- V(g1)$y - 1
  
  V(g8)$x <- V(g1)$x
  V(g8)$y <- V(g1)$y - 1
  
  V(g9)$x <- V(g1)$x + 1
  V(g9)$y <- V(g1)$y - 1
  
  g <- g1 + g2 + g3 + g4 + g5 + g6 + g7 + g8 + g9
  
  
  ### Connect the points on the boundary
  id <- which((((V(g)$x > -r & V(g)$x < r)|(V(g)$x > 1-r & V(g)$x < 1+r))
               & (V(g)$y > -r & V(g)$y < 1+r))
              | (((V(g)$y > -r & V(g)$y < r)|(V(g)$y > 1-r & V(g)$y < 1+r)))
              & (V(g)$x > -r & V(g)$x < 1+r))
  id_1 <- sum(V(g)[id] <= num)
  
  for (i in 1:id_1)
  {
    for (j in (id_1+1):length(id))
    {
      if (get.dist(V(g)[id[i]]$x - V(g)[id[j]]$x,
                   V(g)[id[i]]$y - V(g)[id[j]]$y) <= r)
      {
        g <- g %>% add_edges(c(id[i], id[j]))
      }
    }
  }
  
  
  temp.data <- as_data_frame(g, what = "both")
  temp.edges <- unique(t(apply((temp.data$edges-1)%%num+1, 1, sort)))
  temp.data$edges <- list(from = temp.edges[, 1], to = temp.edges[, 2])
  
  g0 <- g1
  
  for (i in (length(E(g0))+1) : length(temp.data$edges$from))
  {
    g0 <- g0 %>% add_edges(c(temp.data$edges$from[i], temp.data$edges$to[i]))
  }
  
  
  
  ### Simulation
  
  iter.num = 20
  
  num.I <- matrix(nrow = iter.num, ncol = 2*num)
  
  set.seed(2021)
  
  count = 0
  
  for (iter in 1:iter.num) {
    temp.g <- g0
    
    S <- as.numeric(V(g0))
    I <- sample(1:num, 1)
    S <- S[S != I]
    R <- numeric(length = 0)
    
    num.I[iter, 1] <- 1
    
    times = 0
    
    while (is.null(I) == FALSE) 
    {
      a <- beta * length(E(g0)[S %--% I])
      b <- delta * length(I)
      
      if(a+b == 0) break
      
      event <- sample(c(0, 1), size = 1, prob = c( a/(a+b), b/(a+b)))
      
      ### Infection
      if(event == 0)
      {
        temp.infect.list <- ends(g0, E(g0)[.inc(I)])[!ends(g0, E(g0)[.inc(I)]) %in% I]
        temp.infect.list <- temp.infect.list[!temp.infect.list %in% R]
        
        if (length(temp.infect.list) == 1){
          temp.infect <- temp.infect.list
        }
        else {
          temp.infect <- sample(temp.infect.list, 1)
        }
        
        I <- append(I, temp.infect)
        S <- S[S != temp.infect]
      }
      
      ### Recovery
      if(event == 1)
      {
        if (length(I) == 1) temp.recover = I
        else temp.recover <- sample(I, 1)
        I <- I[I != temp.recover]
        R <- append(R, temp.recover)
      }
      # break
      times = times + 1
      
      count = count + 1
      
      time.end <- Sys.time()
      
      record[count] = time.end - time.begin
      
      num.I[iter, times+1] <- length(I)
      
    }
    cat("End of iteration", iter,"\n")
  }
  
  # sheet.name <- paste("Ratio", beta/delta, "Dist", r, sep = "")
  # 
  # write.xlsx(num.I, 'Data_Infect_Nomove_temp.xlsx', sheetName = sheet.name, col.names = TRUE,
  #            row.names = TRUE, append = TRUE, showNA = FALSE)
  
}








