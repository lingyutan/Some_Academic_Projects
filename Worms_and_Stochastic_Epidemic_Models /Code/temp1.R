require(igraph)
require(ggplot2)

get.dist = function(x, y)
{
  sqrt(x^2 + y^2)
}


num = 100  # Number of nodes
r = 0.15   # Infective distance

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

g = g1 + g2 + g3 + g4 + g5 + g6 + g7 + g8 + g9


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

# ### Plot the connection
# g.data <- as_data_frame(g, "both")
# 
# id <- which(g.data$edges$from <= num | g.data$edges$to <= num)
# 
# p <- ggplot(g.data$vertices[1:num, ], aes(x,y)) + geom_point(size = 0.1)
# 
# for (i in id)
# {
#   p <- p + geom_segment(x = g.data$vertices$x[g.data$edges$from[i]],
#                         y = g.data$vertices$y[g.data$edges$from[i]],
#                         xend = g.data$vertices$x[g.data$edges$to[i]],
#                         yend = g.data$vertices$y[g.data$edges$to[i]],
#                         size = 0.02, col = "#9900FF")
# }
# 
# p + scale_x_continuous(limits = c(0, 1), expand = c(0,0)) + 
#   scale_y_continuous(limits = c(0, 1), expand = c(0,0))

# plot.igraph(g, vertex.size = 1, vertex.label = NA)# plot.igraph


g.data <- as_data_frame(g, "both")
id <- which(g.data$edges$from <= num | g.data$edges$to <= num)

### Plot the connection considering boundary
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i")
points(g.data$vertices[1:num, ], pch = 21, bg = 'red', cex = 0.5)
for (i in id)
{
  segments(x0 = g.data$vertices$x[g.data$edges$from[i]],
           y0 = g.data$vertices$y[g.data$edges$from[i]],
           x1 = g.data$vertices$x[g.data$edges$to[i]],
           y1 = g.data$vertices$y[g.data$edges$to[i]],
           lwd = 0.5, col = "#9900FF")
}
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

# plot(g.data$vertices[1:num, ], xlim = c(0,1), ylim = c(0,1), bty = "n",
#      type = 'p', pch = 21, bg = 'red', cex = 0.5, xaxs="i", yaxs="i")
# 
# 
# for (i in id)
# {
#   segments(x0 = g.data$vertices$x[g.data$edges$from[i]],
#            y0 = g.data$vertices$y[g.data$edges$from[i]],
#            x1 = g.data$vertices$x[g.data$edges$to[i]],
#            y1 = g.data$vertices$y[g.data$edges$to[i]],
#            lwd = 0.5, col = "#9900FF")
# }
# rect(0,0,1,1)



### Store g0
# g0 <- g1
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
#       g0 <- g0 %>% add_edges(c((id[i]-1)%%num+1, (id[j]-1)%%num+1))
#     }
#   }
# }

test.data <- as_data_frame(g, what = "both")

test.data$vertices <- list(x = test.data$vertices[1:100, 1], y = test.data$vertices[1:100, 2])

# temp.edges <- unique((test.data$edges-1)%%num+1)
temp.edges <- unique(t(apply((test.data$edges-1)%%num+1, 1, sort)))
test.data$edges <- list(from = temp.edges[, 1], to = temp.edges[, 2])

g0 <- g1

for (i in (length(E(g0))+1) : length(test.data$edges$from))
{
  g0 <- g0 %>% add_edges(c(test.data$edges$from[i], test.data$edges$to[i]))
  # print(c(test.data$edges$from[i], test.data$edges$to[i]))
}

# plot(g0, vertex.size = 1, vertex.label = NA) # Connection Plot Check

# class(test.data$edges$to)
# 
# g0.data <- as_data_frame(g0, "both")
# g0.data$edges <- unique(g0.data$edges)
# 
# g0 <- graph_from_data_frame(g0.data$edges, directed = FALSE, g0.data$vertices)



beta <- 3
delta <- 2

S <- as.numeric(V(g0))
I <- sample(1:100, 1)
S <- S[S != I]
R <- numeric(length = 0)

k = 0
while (is.null(I) == FALSE) {
  a <- beta * length(E(g0)[S %--% I])
  b <- delta * length(I)
  event <- sample(c(0, 1), size = 1, prob = c( a/(a+b), b/(a+b)))
  
  ### Infection
  if(event == 0)
  {
    
  }
  
  ### Recovery
  else
  {
    
  }
  
  break
}



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












