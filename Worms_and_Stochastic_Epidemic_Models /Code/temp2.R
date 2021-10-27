require(igraph)
require(ggplot2)

get.dist = function(x, y)
{
  sqrt(x^2 + y^2)
}

# get.dist.min = function(x, y)
# {
#   min(x^2+y^2, )
# }

move = function(g, id.m)
{
  d <- rnorm(1, 0.3, 0.01)
  angle <- runif(1, 0, 2*pi)
  
  V(g)$x[id.m] <- V(g)$x[id.m] + cos(angle) * d
  V(g)$y[id.m] <- V(g)$y[id.m] + sin(angle) * d
  
  if (V(g)$x[id.m] < 0) V(g)$x[id.m] = V(g)$x[id.m] + 1
  if (V(g)$x[id.m] > 1) V(g)$x[id.m] = V(g)$x[id.m] - 1
  if (V(g)$y[id.m] < 0) V(g)$y[id.m] = V(g)$y[id.m] + 1
  if (V(g)$y[id.m] > 1) V(g)$y[id.m] = V(g)$y[id.m] - 1
  
  g <- g %>% delete_edges(E(g)[.inc(id.m)])
  
  for (i in 1:num)
  {
    if (i == id.m) next
    
    dist.x <- abs(V(g)[i]$x - V(g)[id.m]$x)
    dist.y <- abs(V(g)[i]$y - V(g)[id.m]$y)
    
    if (dist.x > 0.5) dist.x <- 1 - dist.x
    if (dist.y > 0.5) dist.y <- 1 - dist.y
    
    if (get.dist(dist.x, dist.y) <= r)
    {
      g <- g %>% add_edges(c(id.m, id[i]))
    }
  }
  return(g)
}

temp.g <- move(g0, 1)

plot(g0, vertex.size = 1, vertex.label = NA)
plot(temp.g, vertex.size = 1, vertex.label = NA)


par(mfrow = c(1, 2))



num = 1000  # Number of nodes
r = 0.05   # Infective distance

set.seed(2020)
g1 <- sample_grg(num, r, coords = TRUE)

# plot(g1, vertex.size = 1, vertex.label = NA)# plot.igraph
# plot(g1, vertex.size = 1, label.cex = 0.4) # plot.igraph


## Plot layout
### 2 3 4 ###
### 5 1 6 ###
### 7 8 9 ###

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


# plot(g, vertex.size = 1, vertex.label = NA)# plot.igraph


g.data <- as_data_frame(g, "both")
id <- which(g.data$edges$from <= num | g.data$edges$to <= num)

# ### Plot the connection considering boundary
# plot.new()
# plot.window(xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i")
# points(g.data$vertices[1:num, ], pch = 21, bg = 'black', cex = 0.5)
# # points(V(g0)$x, V(g0)$y, pch = 21, bg = 'black', cex = 0.5)
# for (i in id)
# {
#   segments(x0 = g.data$vertices$x[g.data$edges$from[i]],
#            y0 = g.data$vertices$y[g.data$edges$from[i]],
#            x1 = g.data$vertices$x[g.data$edges$to[i]],
#            y1 = g.data$vertices$y[g.data$edges$to[i]],
#            lwd = 0.5, col = "#9900FF")
# }
# rect(-1, -1, 0, 2, col = "white", border = NA)
# rect(-1, 1, 2, 2, col = "white", border = NA)
# rect(1, -1, 2, 2, col = "white", border = NA)
# rect(-1, -1, 2, 0, col = "white", border = NA)
# axis(1)
# axis(2)
# title(main="The Overall Title")
# title(xlab="An x-axis label")
# title(ylab="A y-axis label")
# box()



temp.data <- as_data_frame(g, what = "both")

# temp.data$vertices <- list(x = temp.data$vertices[1:num, 1], y = temp.data$vertices[1:num, 2])

# temp.edges <- unique((temp.data$edges-1)%%num+1)
temp.edges <- unique(t(apply((temp.data$edges-1)%%num+1, 1, sort)))
temp.data$edges <- list(from = temp.edges[, 1], to = temp.edges[, 2])

g0 <- g1

for (i in (length(E(g0))+1) : length(temp.data$edges$from))
{
  g0 <- g0 %>% add_edges(c(temp.data$edges$from[i], temp.data$edges$to[i]))
  # print(c(temp.data$edges$from[i], temp.data$edges$to[i]))
}

# plot(g0, vertex.size = 1, vertex.label = NA) # Connection Plot Check


### Simulation
beta <- 1
delta <- 2

S <- as.numeric(V(g0))
I <- sample(1:num, 1)
S <- S[S != I]
R <- numeric(length = 0)
num.I <- 1


times = 0

# while (is.null(S) == FALSE) 
while (is.null(I) == FALSE) 
# for (i in 1 : 300)
{
  a <- beta * length(E(g0)[S %--% I])
  b <- delta * length(I)
  
  if(a+b == 0) break
  # if(times == 1000) break
  
  event <- sample(c(0, 1), size = 1, prob = c( a/(a+b), b/(a+b)))
  
  ### Infection
  if(event == 0)
  {
    # will get infected twice...
    temp.infect.list <- ends(g0, E(g0)[.inc(I)])[!ends(g0, E(g0)[.inc(I)]) %in% I]
    # temp.infect.list <- S
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
  
  num.I <- append(num.I, length(I))
}

# plot(num.I, type = "l")


### Plot S.I.R. 
id <- which(g.data$edges$from <= num | g.data$edges$to <= num)
id.i <- which(g.data$edges$from %in% I | g.data$edges$to %in% I)
### Plot the connection considering boundary
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i")
# points(g.data$vertices[1:num, ], pch = 21, bg = 'red', cex = 0.5)


for (i in id)
{
  segments(x0 = g.data$vertices$x[g.data$edges$from[i]],
           y0 = g.data$vertices$y[g.data$edges$from[i]],
           x1 = g.data$vertices$x[g.data$edges$to[i]],
           y1 = g.data$vertices$y[g.data$edges$to[i]],
           lwd = 0.5, col = "#00FF00")
}

for (i in id.i)
{
  segments(x0 = g.data$vertices$x[g.data$edges$from[i]],
           y0 = g.data$vertices$y[g.data$edges$from[i]],
           x1 = g.data$vertices$x[g.data$edges$to[i]],
           y1 = g.data$vertices$y[g.data$edges$to[i]],
           lwd = 0.5, col = "#FF0000")
}

points(V(g0)$x[S], V(g0)$y[S], pch = 21, col = '#9900FF', bg = '#9900FF', cex = 0.5) # Blue
points(V(g0)$x[I], V(g0)$y[I], pch = 21, col = '#FF0000', bg = '#FF0000', cex = 0.5) # Red
points(V(g0)$x[R], V(g0)$y[R], pch = 21, col = '#7D7D7D', bg = '#7D7D7D', cex = 0.5) # Grey

rect(-1, -1, 0, 2, col = "white", border = NA)
rect(-1, 1, 2, 2, col = "white", border = NA)
rect(1, -1, 2, 2, col = "white", border = NA)
rect(-1, -1, 2, 0, col = "white", border = NA)
axis(1)
axis(2)
title(main="The Overall Title")
title(xlab="An x-axis label")
title(ylab="A y-axis label")
box()


p1 = 0.01
p2 = 0.9

temp.g <- g0

# 91 points moving

pt.n <- round(p1/(1+p1-p2)*num)

pt.move <- sample(num, pt.n)
pt.still <- 1:num
pt.still <- pt.still[!pt.still %in% pt.move]

for (i in pt.move) {
  temp.g <- move(temp.g, i)
}


par(mfrow = c(1, 2))
plot(g0, vertex.size = 1, vertex.label = NA)
plot(temp.g, vertex.size = 1, vertex.label = NA)

par(mfrow = c(1, 1))










# p1 <- ggplot(g0.data$vertices[1:num, ], aes(x,y)) + geom_point(size = 0.1)
# 
# for (i in 1:length(g0.data$edges$from))
# {
#   p1 <- p1 + geom_segment(x = g0.data$vertices$x[g0.data$edges$from[i]],
#                         y = g0.data$vertices$y[g0.data$edges$from[i]],
#                         xend = g0.data$vertices$x[g0.data$edges$to[i]],
#                         yend = g0.data$vertices$y[g0.data$edges$to[i]],
#                         size = 0.02, col = "#9900FF")
# }
# 
# p1 + scale_x_continuous(limits = c(0, 1), expand = c(0,0)) +
#   scale_y_continuous(limits = c(0, 1), expand = c(0,0))












