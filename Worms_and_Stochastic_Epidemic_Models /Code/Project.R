require(igraph)
require(xlsx)
require(plotly)

### Get the distance of two points using the difference of x&y coordinates
get.dist = function(x, y)
{
  sqrt(x^2 + y^2)
}


# move = function(g, id.m)
# {
#   d <- rnorm(1, 0.3, 0.01)
#   angle <- runif(1, 0, 2*pi)
#   
#   V(g)$x[id.m] <- V(g)$x[id.m] + cos(angle) * d
#   V(g)$y[id.m] <- V(g)$y[id.m] + sin(angle) * d
#   
#   if (V(g)$x[id.m] < 0) V(g)$x[id.m] = V(g)$x[id.m] + 1
#   if (V(g)$x[id.m] > 1) V(g)$x[id.m] = V(g)$x[id.m] - 1
#   if (V(g)$y[id.m] < 0) V(g)$y[id.m] = V(g)$y[id.m] + 1
#   if (V(g)$y[id.m] > 1) V(g)$y[id.m] = V(g)$y[id.m] - 1
#   
#   g <- g %>% delete_edges(E(g)[.inc(id.m)])
#   
#   for (i in 1:num)
#   {
#     if (i == id.m) next
#     
#     dist.x <- abs(V(g)[i]$x - V(g)[id.m]$x)
#     dist.y <- abs(V(g)[i]$y - V(g)[id.m]$y)
#     
#     if (dist.x > 0.5) dist.x <- 1 - dist.x
#     if (dist.y > 0.5) dist.y <- 1 - dist.y
#     
#     if (get.dist(dist.x, dist.y) <= r)
#     {
#       g <- g %>% add_edges(c(id.m, id[i]))
#     }
#   }
#   return(g)
# }

# ### Move points and recalculate edges
# move.2 = function(g, id.m)
# {
#   if(length(id.m) == 0) return(g)
#     
#   for (i in 1:length(id.m))
#   {
#     d <- rnorm(1, 0.3, 0.01)
#     angle <- runif(1, 0, 2*pi)
#     
#     V(g)$x[id.m[i]] <- V(g)$x[id.m[i]] + cos(angle) * d
#     V(g)$y[id.m[i]] <- V(g)$y[id.m[i]] + sin(angle) * d
#     
#     if (V(g)$x[id.m[i]] < 0) V(g)$x[id.m[i]] = V(g)$x[id.m[i]] + 1
#     if (V(g)$x[id.m[i]] > 1) V(g)$x[id.m[i]] = V(g)$x[id.m[i]] - 1
#     if (V(g)$y[id.m[i]] < 0) V(g)$y[id.m[i]] = V(g)$y[id.m[i]] + 1
#     if (V(g)$y[id.m[i]] > 1) V(g)$y[id.m[i]] = V(g)$y[id.m[i]] - 1
#   }
#   
#   g <- g %>% delete_edges(E(g)[.inc(1:num)])
#   
#   for (i in 1:(num-1))
#   {
#     for (j in (i+1) : num)
#     {
#       dist.x <- abs(V(g)[i]$x - V(g)[j]$x)
#       dist.y <- abs(V(g)[i]$y - V(g)[j]$y)
#       
#       if (dist.x > 0.5) dist.x <- 1 - dist.x
#       if (dist.y > 0.5) dist.y <- 1 - dist.y
#       
#       if (get.dist(dist.x, dist.y) <= r)
#       {
#         g <- g %>% add_edges(c(i, j))
#       }
#     }
#   }
#   
#   return(g)
# }

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

### Remove all the edges across the boundry
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














for (r in seq(0.165, 0.17, 0.005))
  
{
  
time.begin <- Sys.time()

record2 <- numeric()
count2 <- 1

##################################

num = 100  # Number of nodes
r = 0.14   # Infective distance

beta = 6   # Infection rate
delta = 2  # Recovery rate

##################################

set.seed(2020)
# set.seed(714)
g1 <- sample_grg(num, r, coords = TRUE)
# g1 <- g1 %>% delete_edges(E(g1)[.inc(1:num)])
# plot(g1, vertex.size = 1, vertex.label = NA)# plot.igraph
# plot(temp.g, vertex.size = 1, vertex.label = NA)


# ## Plot layout
# ### 2 3 4 ###
# ### 5 1 6 ###
# ### 7 8 9 ###
# 
# ### g1: graph without boundry
# ### g0: graph with boundry
# 
# g9 = g8 = g7 = g6 = g5 = g4 = g3 = g2 = g1
# 
# V(g2)$x <- V(g1)$x - 1
# V(g2)$y <- V(g1)$y + 1
# 
# V(g3)$x <- V(g1)$x 
# V(g3)$y <- V(g1)$y + 1
# 
# V(g4)$x <- V(g1)$x + 1
# V(g4)$y <- V(g1)$y + 1
# 
# V(g5)$x <- V(g1)$x - 1
# V(g5)$y <- V(g1)$y 
# 
# V(g6)$x <- V(g1)$x + 1
# V(g6)$y <- V(g1)$y 
# 
# V(g7)$x <- V(g1)$x - 1
# V(g7)$y <- V(g1)$y - 1
# 
# V(g8)$x <- V(g1)$x
# V(g8)$y <- V(g1)$y - 1
# 
# V(g9)$x <- V(g1)$x + 1
# V(g9)$y <- V(g1)$y - 1
# 
# g <- g1 + g2 + g3 + g4 + g5 + g6 + g7 + g8 + g9



### Connect the points on the boundary
# id <- which((((V(g)$x > -r & V(g)$x < r)|(V(g)$x > 1-r & V(g)$x < 1+r))
#              & (V(g)$y > -r & V(g)$y < 1+r))
#             | (((V(g)$y > -r & V(g)$y < r)|(V(g)$y > 1-r & V(g)$y < 1+r)))
#             & (V(g)$x > -r & V(g)$x < 1+r))
# id_1 <- sum(V(g)[id] <= num)
# 
# for (i in 1:id_1)
# {
#   for (j in (id_1+1):length(id))
#   {
#     if (get.dist(V(g)[id[i]]$x - V(g)[id[j]]$x,
#                  V(g)[id[i]]$y - V(g)[id[j]]$y) <= r)
#     {
#       g <- g %>% add_edges(c(id[i], id[j]))
#     }
#   }
# }
# 
# 
# 
# temp.data <- as_data_frame(g, what = "both")
# temp.edges <- unique(t(apply((temp.data$edges-1)%%num+1, 1, sort)))
# temp.data$edges <- list(from = temp.edges[, 1], to = temp.edges[, 2])
# 
# g0 <- g1
# 
# for (i in (length(E(g0))+1) : length(temp.data$edges$from))
# {
#   g0 <- g0 %>% add_edges(c(temp.data$edges$from[i], temp.data$edges$to[i]))
# }

# plot(g0, vertex.size = 1, vertex.label = NA) # Connection Plot Check



### Simulation

iter.num = 20

num.I <- matrix(nrow = iter.num, ncol = 2*num)

set.seed(2021)

for (iter in 1:iter.num) {
  # temp.g <- g0
  temp.g <- g1
  
  p1 <- 0.01
  p2 <- 0.9
  
  # number of points moving
  pt.n <- round(p1/(1+p1-p2)*num)
  pt.move <- sample(num, pt.n)
  pt.still <- 1:num
  pt.still <- pt.still[!pt.still %in% pt.move]
  
  # I <- 72
  S <- as.numeric(V(temp.g))
  I <- sample(1:num, 1)
  S <- S[S != I]
  R <- numeric(length = 0)
  
  
  for (i in 1 : length(S))
  {
    for (j in 1 : length(I))
    {
      dist.x <- abs(V(temp.g)[S[i]]$x - V(temp.g)[I[j]]$x)
      dist.y <- abs(V(temp.g)[S[i]]$y - V(temp.g)[I[j]]$y)
      
      if (dist.x > 0.5) dist.x <- 1 - dist.x
      if (dist.y > 0.5) dist.y <- 1 - dist.y
      
      if (get.dist(dist.x, dist.y) <= r)
      {
        temp.g <- temp.g %>% add_edges(c(S[i], I[j]))
      }
    }
  }
  
  num.I[iter, 1] <- 1
  
  times = 0
  
  # while (is.null(S) == FALSE) 
  while (is.null(I) == FALSE) 
  # for (i in 1 : 85)
  {
    # a <- beta * length(E(g0)[S %--% I])
    a <- beta * length(E(temp.g)[S %--% I])
    b <- delta * length(I)
    
    if(a+b == 0) break
    # if(times == 1000) break
    
    # time.int <- floor(rexp(1, 1/(a+b)))
    
    time.int <- rexp(1, a+b) * 160
    
    while (time.int > 0)
    {
      pt.mtos <- choosep(pt.move, 1-p2)
      pt.stom <- choosep(pt.still, p1)
      
      pt.move <- pt.move[!pt.move %in% pt.mtos]
      pt.move <- append(pt.move, pt.stom)
      
      pt.still <- pt.still[!pt.still %in% pt.stom]
      pt.still <- append(pt.still, pt.mtos)
      
      # pt.move.rec <- append(pt.move.rec, pt.move)
      
      for (j in pt.move) {
        temp.g <- move.noedge(temp.g, j)
      }
      
      time.int <- time.int - runif(1, 0.8, 1.2)
    }
    
    pt.mtos <- choosep(pt.move, 1-p2)
    pt.stom <- choosep(pt.still, p1)
    
    pt.move <- pt.move[!pt.move %in% pt.mtos]
    pt.move <- append(pt.move, pt.stom)
    
    # for (j in pt.move) {
    #   temp.g <- move(temp.g, j)
    # }
    
    # temp.g <- move(temp.g, pt.move, R)
    temp.g <- move.simple(temp.g, pt.move, S, I)
    
    ### New graph after points moving has been obtained ###
    
    event <- sample(c(0, 1), size = 1, prob = c( a/(a+b), b/(a+b)))
    
    ### Infection
    if(event == 0)
    {
      temp.infect.list <- ends(temp.g, E(temp.g)[.inc(I)])[!ends(temp.g, E(temp.g)[.inc(I)]) %in% I]
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
    time.end <- Sys.time()
    
    record2[count2] = time.end - time.begin
    count2 <- count2 + 1
    
    
    times = times + 1
    
    num.I[iter, times+1] <- length(I)
  }
  cat("End of iteration", iter,"\n")
}


# write.xlsx(num.I, 'InfectNumRec_4.xlsx', sheetName = "Dist00", col.names = TRUE,
#            row.names = TRUE, append = TRUE, showNA = FALSE)

sheet.name <- paste("Ratio", beta/delta, "Dist", r, sep = "")

# write.xlsx(num.I, 'Data_Infect_4.xlsx', sheetName = sheet.name, col.names = TRUE,
#            row.names = TRUE, append = TRUE, showNA = FALSE)

time.end <- Sys.time()

cat(time.end - time.begin)
  
}






set.seed(130)
temp.g <- sample_grg(100, 0.15, coords = TRUE)
plot(temp.g, vertex.size = 1, vertex.label = NA)
g1.f <- temp.g

g1.f <- boundary(temp.g, r)

g9.f = g8.f = g7.f = g6.f = g5.f = g4.f = g3.f = g2.f = g1.f

V(g2.f)$x <- V(g1.f)$x - 1
V(g2.f)$y <- V(g1.f)$y + 1

V(g3.f)$x <- V(g1.f)$x 
V(g3.f)$y <- V(g1.f)$y + 1

V(g4.f)$x <- V(g1.f)$x + 1
V(g4.f)$y <- V(g1.f)$y + 1

V(g5.f)$x <- V(g1.f)$x - 1
V(g5.f)$y <- V(g1.f)$y 

V(g6.f)$x <- V(g1.f)$x + 1
V(g6.f)$y <- V(g1.f)$y 

V(g7.f)$x <- V(g1.f)$x - 1
V(g7.f)$y <- V(g1.f)$y - 1

V(g8.f)$x <- V(g1.f)$x
V(g8.f)$y <- V(g1.f)$y - 1

V(g9.f)$x <- V(g1.f)$x + 1
V(g9.f)$y <- V(g1.f)$y - 1

g.f <- g1.f + g2.f + g3.f + g4.f + g5.f + g6.f + g7.f + g8.f + g9.f

### Connect the points on the boundary
id <- which((((V(g.f)$x > -r & V(g.f)$x < r)|(V(g.f)$x > 1-r & V(g.f)$x < 1+r))
             & (V(g.f)$y > -r & V(g.f)$y < 1+r))
            | (((V(g.f)$y > -r & V(g.f)$y < r)|(V(g.f)$y > 1-r & V(g.f)$y < 1+r)))
            & (V(g.f)$x > -r & V(g.f)$x < 1+r))
id_1 <- sum(V(g.f)[id] <= num)

for (i in 1:id_1)
{
  for (j in (id_1+1):length(id))
  {
    if (get.dist(V(g.f)[id[i]]$x - V(g.f)[id[j]]$x,
                 V(g.f)[id[i]]$y - V(g.f)[id[j]]$y) <= r)
    {
      g.f <- g.f %>% add_edges(c(id[i], id[j]))
    }
  }
}

g.f <- g.f %>% delete_edges(E(g.f)[.inc(R)]) # Remove Recovery edges
g.f <- g.f %>% delete_edges(E(g.f)[.inc(R+1*num)]) # Remove Recovery edges
g.f <- g.f %>% delete_edges(E(g.f)[.inc(R+2*num)]) # Remove Recovery edges
g.f <- g.f %>% delete_edges(E(g.f)[.inc(R+3*num)]) # Remove Recovery edges
g.f <- g.f %>% delete_edges(E(g.f)[.inc(R+4*num)]) # Remove Recovery edges
g.f <- g.f %>% delete_edges(E(g.f)[.inc(R+5*num)]) # Remove Recovery edges
g.f <- g.f %>% delete_edges(E(g.f)[.inc(R+6*num)]) # Remove Recovery edges
g.f <- g.f %>% delete_edges(E(g.f)[.inc(R+7*num)]) # Remove Recovery edges
g.f <- g.f %>% delete_edges(E(g.f)[.inc(R+8*num)]) # Remove Recovery edges


g.f.data <- as_data_frame(g.f, "both")

id <- which(g.f.data$edges$from <= num | g.f.data$edges$to <= num)
id.i <- which(g.f.data$edges$from %in% I | g.f.data$edges$to %in% I)


tiff("SIR.tiff", units="in", width=5, height=5, res=300)

### Plot the connection considering boundary
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i")
# points(g.f.data$vertices[1:num, ], pch = 21, bg = 'red', cex = 0.5)


for (i in id)
{
  segments(x0 = g.f.data$vertices$x[g.f.data$edges$from[i]],
           y0 = g.f.data$vertices$y[g.f.data$edges$from[i]],
           x1 = g.f.data$vertices$x[g.f.data$edges$to[i]],
           y1 = g.f.data$vertices$y[g.f.data$edges$to[i]],
           lwd = 0.5, col = "#00FF00")
}

for (i in id.i)
{
  segments(x0 = g.f.data$vertices$x[g.f.data$edges$from[i]],
           y0 = g.f.data$vertices$y[g.f.data$edges$from[i]],
           x1 = g.f.data$vertices$x[g.f.data$edges$to[i]],
           y1 = g.f.data$vertices$y[g.f.data$edges$to[i]],
           lwd = 0.5, col = "#FF0000")
}



points(V(temp.g)$x[S], V(temp.g)$y[S], pch = 21, col = '#9900FF', bg = '#9900FF', cex = 0.5) # Blue
points(V(temp.g)$x[I], V(temp.g)$y[I], pch = 21, col = '#FF0000', bg = '#FF0000', cex = 0.5) # Red
points(V(temp.g)$x[R], V(temp.g)$y[R], pch = 21, col = '#7D7D7D', bg = '#7D7D7D', cex = 0.5) # Grey

# points(V(temp.g)$x, V(temp.g)$y, pch = 21, col = '#9900FF', bg = '#9900FF', cex = 0.5)
# points(V(temp.g)$x[pt.move.rec], V(temp.g)$y[pt.move.rec], pch = 21, col = '#FF0000', bg = '#FF0000', cex = 0.5)

rect(-1, -1, 0, 2, col = "white", border = NA)
rect(-1, 1, 2, 2, col = "white", border = NA)
rect(1, -1, 2, 2, col = "white", border = NA)
rect(-1, -1, 2, 0, col = "white", border = NA)
rect(r, r, 1-r, 1-r, lwd = 0.5, lty = 2)
axis(1)
axis(2)
# title(main="The Overall Title")
# title(xlab="An x-axis label")
# title(ylab="A y-axis label")
box()


dev.off()


count <- 0

for (i in 1:num)
{
  if (V(temp.g)[i]$x < r | V(temp.g)[i]$x > 1-r | V(temp.g)[i]$y < r | V(temp.g)[i]$y > 1-r )
    count <- count + 1
}






tiff("fig_eg.tiff", units="in", width=5, height=5, res=300)
set.seed(129)
g.test <- sample_grg(100, 0.15, coords = TRUE)
plot(g.test, vertex.size = 1, vertex.label = NA)
dev.off()


tiff("connect_eg.tiff", units="in", width=5, height=5, res=300)
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i")
points(c(0.1, 0.9), c(0.3, 0.5) , pch = 21, col = '#9900FF', bg = '#9900FF', cex = 1.5)
segments(0.1, 0.3, -0.1, 0.5, lwd = 2)
segments(1.1, 0.3, 0.9, 0.5, lwd = 2)
segments(0.1, 0.3, 0.9, 0.5, lty = 2)
points(0.5, 0.4, pch = 4, cex = 2, col = "#FF0000", lwd = 2)
rect(-1, -1, 0, 2, col = "white", border = NA)
rect(-1, 1, 2, 2, col = "white", border = NA)
rect(1, -1, 2, 2, col = "white", border = NA)
rect(-1, -1, 2, 0, col = "white", border = NA)
axis(1)
axis(2)
# title(main="The Overall Title")
# title(xlab="An x-axis label")
# title(ylab="A y-axis label")
box()
dev.off()













### Without Movements with tracking I

{
  
  time.begin <- Sys.time()
  ##################################
  
  num = 100  # Number of nodes
  r = 0.13   # Infective distance
  
  beta = 6   # Infection rate
  delta = 2  # Recovery rate
  
  ##################################
  
  # set.seed(2020)
  set.seed(714)
  g1 <- sample_grg(num, r, coords = TRUE)
  
  
  ## Plot layout
  ### 2 3 4 ###
  ### 5 1 6 ###
  ### 7 8 9 ###
  
  ### g1: graph with boundry
  ### g0: graph without boundry
  
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
  
  for (iter in 1:iter.num) {
    temp.g <- g0
    
    S <- as.numeric(V(temp.g))
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
      
      num.I[iter, times+1] <- length(I)
      
    }
    cat("End of iteration", iter,"\n")
  }
  
  
  sheet.name <- paste("Ratio", beta/delta, "Dist", r, sep = "")
  
  write.xlsx(num.I, 'Data_Infect_Nomove_1.xlsx', sheetName = sheet.name, col.names = TRUE,
             row.names = TRUE, append = TRUE, showNA = FALSE)
  
  time.end <- Sys.time()
  
  cat(time.end - time.begin)
  
}































### Without Movements with tracking only average
{

r.start <- 0.07
r.end <- 0.20
r.step <- 0.01

R0.start <- 0.4
R0.end <- 0.4
R0.step <- 0.05

r.range <- seq(r.start, r.end, r.step)
R0.range <- seq(R0.start, R0.end, R0.step)

AverI <- matrix(nrow = length(r.range), ncol = length(R0.range))

time.begin <- Sys.time()

for (r in r.range)
# foreach (r = r.range) %dopar%
{
  for (R0 in R0.range)
  {
    ##################################
    
    #r = 0.088
    # R0 = 1
    
    num = 100  # Number of nodes
    
    beta = 2 * R0   # Infection rate
    delta = 2  # Recovery rate
    
    ##################################
    
    set.seed(2020)
    # set.seed(714)
    # iter.graph.num = 3
    iter.graph.num = 5
    tempAverI <- numeric(iter.graph.num)
    
    for (i.g in 1:iter.graph.num)
    {
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
      
      # iter.num = 50
      iter.num = 100
      
      num.I.overall <- numeric(iter.num)
      
      # set.seed(2021)
      set.seed(714)
      
      for (iter in 1:iter.num) {
        temp.g <- g0
        
        S <- as.numeric(V(g0))
        I <- sample(1:num, 1)
        S <- S[S != I]
        R <- numeric(length = 0)
        
        
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
        }
        num.I.overall[iter] = (times + 1) / 2
        
        # cat("End of iteration", iter,"\n")
      }
      
      tempAverI[i.g] = mean(num.I.overall)
    }
    
    AverI[round((r-r.start)/r.step)+1, round((R0-R0.start)/R0.step)+1] = mean(tempAverI)
      
    cat("End of","r", r, "R0", R0, "\n")
    
  }
}

time.end <- Sys.time()

# time.end - time.begin

}

write.xlsx(AverI, 'Data_Infect_Nomove.xlsx', sheetName = "AverI_add", col.names = TRUE,
           row.names = TRUE, append = TRUE, showNA = FALSE)






# for (i in 1:29)
# {
#   for (j in 1:51)
#   {
#     AverI[i, j] = i + j + rnorm(1, 0, 1)
#   }
# }


require(plotly)
{
  
r.start <- 0.07
r.end <- 0.2
r.step <- 0.01

R0.start <- 0.4
R0.end <- 3.0
R0.step <- 0.1

r.range <- seq(r.start, r.end, r.step)
R0.range <- seq(R0.start, R0.end, R0.step)


AverI <- read.xlsx("Data_Infect_Nomove.xlsx", 1, header = TRUE)[, -1]
AverI <- as.matrix(AverI)
# AverI <- as.matrix(AverI[seq(1, 27, 2), seq(1, 59, 2)]) # Try big steps

# contour(x = r.range, y = R0.range, AverI)
# 
# fig <- plot_ly(
#   x = r.range, 
#   y = R0.range,
#   z = AverI, 
#   type = "contour" 
# )

fig <- plot_ly(
  type = "contour",
  x = R0.range,
  y = r.range,
  z = AverI, 
  # autocontour = TRUE, 
  contours = list(
    end = 90,
    size = 5,
    start = 5,
    coloring = 'heatmap',
    showlabels = TRUE),
  # line = list(smoothing = 0.9)
) %>% layout(xaxis = list(title = 'R0'), yaxis = list(title = 'r'))

fig <- fig %>% colorbar(title = "Average \nProportion \n(%)")

# fig <- fig %>% add_surface()

fig

}







r1 <- read.xlsx("Data_Infect_Nomove_1.xlsx", 5, header = TRUE)
r2 <- read.xlsx("Data_Infect_Nomove_1.xlsx", 6, header = TRUE)
r3 <- read.xlsx("Data_Infect_Nomove_1.xlsx", 7, header = TRUE)
r4 <- read.xlsx("Data_Infect_Nomove_1.xlsx", 8, header = TRUE)
r5 <- read.xlsx("Data_Infect_Nomove_1.xlsx", 9, header = TRUE)


par(xpd=FALSE)
plot(r1, type = "l", xlim = c(0.08, 0.3), col = "#000000")
lines(r2, col = "#FF0000")
lines(r3, col = "#00FF00")
lines(r4, col = "#0000FF")
lines(r5, col = "#777777")

abline(h = 5, lty = 2, lwd = 0.5, col = "#BBBBBB")
abline(h = 50, lty = 2, lwd = 0.5, col = "#BBBBBB")
abline(h = 90, lty = 2, lwd = 0.5, col = "#BBBBBB")
text(0.2, 5, "5%", col = "#999999", adj = c(0, -.1), cex = 0.8)
text(0.2, 50, "50%", col = "#999999", adj = c(0, -.1), cex = 0.8)
text(0.08, 90, "90%", col = "#999999", adj = c(0, -.1), cex = 0.8)


legend("bottomright",
       legend = c("R0 = 0.5", "R0 = 1.0", "R0 = 1.5", "R0 = 2.0", "R0 = 3.0"),
       col = c("#000000", "#FF0000", "#00FF00", "#0000FF", "#777777"),
       lty = 1, cex = 0.8, lwd = 2)







num.I <- read.xlsx("Data_Infect.xlsx", 51, header = TRUE)[1, -1]
num.I <- as.vector(as.matrix(num.I))
num.I[is.na(num.I)] <- 0
num.S <- 99
num.R <- 0
for (i in 1:199) {
  if(num.I[i+1] - num.I[i] == 1) {
    num.S <- append(num.S, num.S[i] - 1)
    num.R <- append(num.R, num.R[i])
  }
  else {
    num.S <- append(num.S, num.S[i])
    num.R <- append(num.R, num.R[i] + 1)
  }
}

plot(0:199, num.I, type = "l", ylab = "SIR populations", xlab = "times",
     ylim = c(0, 100), col = "#FF0000")
lines(0:199, num.S, col = "#9900FF")
lines(0:199, num.R, col = "#7D7D7D")

legend("right",
       legend = c("S", "I", "R"),
       col = c("#FF0000", "#9900FF", "#7D7D7D"),
       lty = 1, cex = 0.8, lwd = 2)














{
  
num = 100
r = 0.14

set.seed(2020)
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

# plot(g0, vertex.size = 1, vertex.label = NA) # Connection Plot Check



set.seed(2021)

temp.g <- g0

p1 <- 0.01
p2 <- 0.9

# number of points moving
pt.n <- round(p1/(1+p1-p2)*num)
pt.move <- sample(num, pt.n)
pt.still <- 1:num
pt.still <- pt.still[!pt.still %in% pt.move]

S <- as.numeric(V(temp.g))
I <- sample(1:num, 1)
S <- S[S != I]
R <- numeric(length = 0)


pt.move.rec <- NULL
  
time.int <- 6 # Assume

move.time = 1

while (time.int > 0)
{
  pt.mtos <- choosep(pt.move, 1-p2)
  pt.stom <- choosep(pt.still, p1)
  
  pt.move <- pt.move[!pt.move %in% pt.mtos]
  pt.move <- append(pt.move, pt.stom)
  
  pt.still <- pt.still[!pt.still %in% pt.stom]
  pt.still <- append(pt.still, pt.mtos)
  
  pt.move.rec <- append(pt.move.rec, pt.move)
  
  temp.g <- move(temp.g, pt.move, R)
  
  
  
  ###########################################################################
  g1.f <- temp.g
  
  g1.f <- boundary(temp.g, r)
  
  g9.f = g8.f = g7.f = g6.f = g5.f = g4.f = g3.f = g2.f = g1.f
  
  V(g2.f)$x <- V(g1.f)$x - 1
  V(g2.f)$y <- V(g1.f)$y + 1
  
  V(g3.f)$x <- V(g1.f)$x 
  V(g3.f)$y <- V(g1.f)$y + 1
  
  V(g4.f)$x <- V(g1.f)$x + 1
  V(g4.f)$y <- V(g1.f)$y + 1
  
  V(g5.f)$x <- V(g1.f)$x - 1
  V(g5.f)$y <- V(g1.f)$y 
  
  V(g6.f)$x <- V(g1.f)$x + 1
  V(g6.f)$y <- V(g1.f)$y 
  
  V(g7.f)$x <- V(g1.f)$x - 1
  V(g7.f)$y <- V(g1.f)$y - 1
  
  V(g8.f)$x <- V(g1.f)$x
  V(g8.f)$y <- V(g1.f)$y - 1
  
  V(g9.f)$x <- V(g1.f)$x + 1
  V(g9.f)$y <- V(g1.f)$y - 1
  
  g.f <- g1.f + g2.f + g3.f + g4.f + g5.f + g6.f + g7.f + g8.f + g9.f
  
  ### Connect the points on the boundary
  id <- which((((V(g.f)$x > -r & V(g.f)$x < r)|(V(g.f)$x > 1-r & V(g.f)$x < 1+r))
               & (V(g.f)$y > -r & V(g.f)$y < 1+r))
              | (((V(g.f)$y > -r & V(g.f)$y < r)|(V(g.f)$y > 1-r & V(g.f)$y < 1+r)))
              & (V(g.f)$x > -r & V(g.f)$x < 1+r))
  id_1 <- sum(V(g.f)[id] <= num)
  
  for (i in 1:id_1)
  {
    for (j in (id_1+1):length(id))
    {
      if (get.dist(V(g.f)[id[i]]$x - V(g.f)[id[j]]$x,
                   V(g.f)[id[i]]$y - V(g.f)[id[j]]$y) <= r)
      {
        g.f <- g.f %>% add_edges(c(id[i], id[j]))
      }
    }
  }
  
  
  g.f.data <- as_data_frame(g.f, "both")
  
  id <- which(g.f.data$edges$from <= num | g.f.data$edges$to <= num)
  
  move.name <- paste("Move_Process", move.time, ".png", sep = "")
  
  png(move.name, units="in", width=5, height=5, res=300)
  
  ### Plot the connection considering boundary
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i")
  # points(g.f.data$vertices[1:num, ], pch = 21, bg = 'red', cex = 0.5)
  
  
  for (i in id)
  {
    segments(x0 = g.f.data$vertices$x[g.f.data$edges$from[i]],
             y0 = g.f.data$vertices$y[g.f.data$edges$from[i]],
             x1 = g.f.data$vertices$x[g.f.data$edges$to[i]],
             y1 = g.f.data$vertices$y[g.f.data$edges$to[i]],
             lwd = 0.5, col = "#00FF00")
  }
  
  pt.move.rec <- unique(pt.move.rec)
  
  points(V(temp.g)$x, V(temp.g)$y, pch = 21, col = '#9900FF', bg = '#9900FF', cex = 0.5)
  points(V(temp.g)$x[pt.move.rec], V(temp.g)$y[pt.move.rec], pch = 21, col = '#FF0000', bg = '#FF0000', cex = 0.5)
  
  rect(-1, -1, 0, 2, col = "white", border = NA)
  rect(-1, 1, 2, 2, col = "white", border = NA)
  rect(1, -1, 2, 2, col = "white", border = NA)
  rect(-1, -1, 2, 0, col = "white", border = NA)
  axis(1)
  axis(2)
  title(main = paste("movement times:", move.time))
  # title(xlab="An x-axis label")
  # title(ylab="A y-axis label")
  box()
  
  
  dev.off()
  
  
  
  
  ###########################################################################
  time.int <- time.int - runif(1, 0.8, 1.2)
  
  move.time = move.time + 1
}


}











plot(g.f, vertex.size = 1, vertex.label = NA)












### TIME:

8.75 + 59.85 + 38.32 + 37.27 + 43.40 + 55.82 + 33.42 + 57.75 + 23.45 + 39.37 + 
35.17 + 43.22 + 22.57 + 94.15 + 85.57 + 44.45 + 46.20 + 81.02

