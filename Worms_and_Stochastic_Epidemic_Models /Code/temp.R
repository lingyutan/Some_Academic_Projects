require(igraph)
require(ggplot2)

get.dist = function(x, y)
{
  sqrt(x^2 + y^2)
}


num = 100
r = 0.15

set.seed(2)
g1 <- sample_grg(num, r, coords = TRUE)
# plot(g1, vertex.size = 1, vertex.label = NA)# plot.igraph

# plot(g1, vertex.size = 1, label.cex = 0.4) # plot.igraph

# plot(cbind(V(g1)$x, V(g1)$y))

## Plot layout
### 2 3 4 ###
### 5 1 6 ###
### 7 8 9 ###

g0 = g9 = g8 = g7 = g6 = g5 = g4 = g3 = g2 = g1

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


# plot(g, vertex.size = 1, label.size = 0.5)# plot.igraph

### Connection order 
###   1   2   ###
### 4 + 4 + 4 ###
###   1   2   ###
### 3 + 3 + 3 ###
###   1   2   ###
# 
# id <- which(V(g)$x > -r & V(g)$x < r)
# for (i in 1:(length(id)-1))
# {
#   for (j in (i+1):length(id))
#   {
#     if (floor((id[i]-1)/num) == floor((id[j]-1)/num))
#     {
#       break
#     }
#     else if (get.dist(V(g)[id[i]]$x - V(g)[id[j]]$x,
#                       V(g)[id[i]]$y - V(g)[id[j]]$y) <= r)
#     {
#       g <- g %>% add_edges(c(id[i], id[j]))
#     }
#   }
# }
# 
# 
# id <- which(V(g)$x > -r+1 & V(g)$x < r+1)
# for (i in 1:(length(id)-1))
# {
#   for (j in (i+1):length(id))
#   {
#     if (floor((id[i]-1)/num) == floor((id[j]-1)/num))
#     {
#       break
#     }
#     else if (get.dist(V(g)[id[i]]$x - V(g)[id[j]]$x,
#                       V(g)[id[i]]$y - V(g)[id[j]]$y) <= r)
#     {
#       g <- g %>% add_edges(c(id[i], id[j]))
#     }
#   }
# }
# 
# id <- which(V(g)$y > -r & V(g)$y < r)
# for (i in 1:(length(id)-1))
# {
#   for (j in (i+1):length(id))
#   {
#     if (floor((id[i]-1)/num) == floor((id[j]-1)/num))
#     {
#       break
#     }
#     else if (get.dist(V(g)[id[i]]$x - V(g)[id[j]]$x,
#                       V(g)[id[i]]$y - V(g)[id[j]]$y) <= r)
#     {
#       g <- g %>% add_edges(c(id[i], id[j]))
#     }
#   }
# }
# 
# id <- which(V(g)$y > -r+1 & V(g)$y < r+1)
# for (i in 1:(length(id)-1))
# {
#   for (j in (i+1):length(id))
#   {
#     if (floor((id[i]-1)/num) == floor((id[j]-1)/num))
#     {
#       break
#     }
#     else if (get.dist(V(g)[id[i]]$x - V(g)[id[j]]$x,
#                        V(g)[id[i]]$y - V(g)[id[j]]$y) <= r)
#     {
#       g <- g %>% add_edges(c(id[i], id[j]))
#     }
#   }
# }
# 

### Connect the points on the boundary
id <- which(((V(g)$x > -r & V(g)$x < r)|(V(g)$x > 1-r & V(g)$x < 1+r))
            & ((V(g)$y > -r & V(g)$y < r)|(V(g)$y > 1-r & V(g)$y < 1+r)))
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

### Plot the connection
g.data <- as_data_frame(g, "both")

id <- which(g.data$edges$from <= 100 | g.data$edges$to <= 100)

p <- ggplot(g.data$vertices[1:num, ], aes(x,y)) + geom_point(size = 0.1)

for (i in id)
{
  p <- p + geom_segment(x = g.data$vertices$x[g.data$edges$from[i]],
                        y = g.data$vertices$y[g.data$edges$from[i]],
                        xend = g.data$vertices$x[g.data$edges$to[i]],
                        yend = g.data$vertices$y[g.data$edges$to[i]],
                        size = 0.02, col = "#9900FF")
}
  
p + scale_x_continuous(limits = c(0, 1), expand = c(0,0)) + 
  scale_y_continuous(limits = c(0, 1), expand = c(0,0))

# plot.igraph(g, vertex.size = 1, vertex.label = NA, xaxs="i", yaxs="i")# plot.igraph






# ### Store g0
# 
# id <- which(V(g)$x > -r & V(g)$x < -r+1 & V(g)$y > -r & V(g)$y < r)
# for (i in 1:(length(id)-1))
# {
#   for (j in (i+1):length(id))
#   {
#     if (floor((id[i]-1)/num) == floor((id[j]-1)/num))
#     {
#       break
#     }
#     else if (get.dist(V(g)[id[i]]$x - V(g)[id[j]]$x,
#                       V(g)[id[i]]$y - V(g)[id[j]]$y) <= r)
#     {
#       g0 <- g0 %>% add_edges(c((id[i]-1)%%100+1, (id[j]-1)%%100+1))
#     }
#   }
# }
# 
# id <- which(V(g)$x > -r+1 & V(g)$x < r+1 & V(g)$y > r & V(g)$y < -r+1)
# for (i in 1:(length(id)-1))
# {
#   for (j in (i+1):length(id))
#   {
#     if (floor((id[i]-1)/num) == floor((id[j]-1)/num))
#     {
#       break
#     }
#     else if (get.dist(V(g)[id[i]]$x - V(g)[id[j]]$x,
#                       V(g)[id[i]]$y - V(g)[id[j]]$y) <= r)
#     {
#       g0 <- g0 %>% add_edges(c((id[i]-1)%%100+1, (id[j]-1)%%100+1))
#     }
#   }
# }
# 
# # plot(g0, vertex.size = 1, vertex.label = NA)# plot.igraph


id <- which(((V(g)$x > -r & V(g)$x < r)|(V(g)$x > 1-r & V(g)$x < 1+r))
            & ((V(g)$y > -r & V(g)$y < r)|(V(g)$y > 1-r & V(g)$y < 1+r)))
id_1 <- sum(V(g)[id] <= num)

for (i in 1:id_1)
{
  for (j in (id_1+1):length(id))
  {
    if (get.dist(V(g)[id[i]]$x - V(g)[id[j]]$x,
                 V(g)[id[i]]$y - V(g)[id[j]]$y) <= r)
    {
      g0 <- g0 %>% add_edges(c((id[i]-1)%%100+1, (id[j]-1)%%100+1))
    }
  }
}

g0.data <- as_data_frame(g0, "both")
g0.data$edges <- unique(g0.data$edges)

p <- ggplot(g0.data$vertices[1:num, ], aes(x,y)) + geom_point(size = 0.1)

for (i in 1:length(g0.data$edges$from))
{
  p <- p + geom_segment(x = g0.data$vertices$x[g0.data$edges$from[i]],
                        y = g0.data$vertices$y[g0.data$edges$from[i]],
                        xend = g0.data$vertices$x[g0.data$edges$to[i]],
                        yend = g0.data$vertices$y[g0.data$edges$to[i]],
                        size = 0.02, col = "#9900FF")
}

p + scale_x_continuous(limits = c(0, 1), expand = c(0,0)) + 
  scale_y_continuous(limits = c(0, 1), expand = c(0,0))













# 
# g1
# 
# 
# g2 <- sample_grg(100, 0.05, coords = TRUE, torus = TRUE)
# # comps <- components(g2)$membership
# # colbar <- rainbow(max(comps)+1)
# # V(g2)$color <- colbar[comps+1]
# 
# plot(g2, layout=layout_with_fr, vertex.size=5, vertex.label=NA)
# 
# plot(g2, vertex.size = 5, vertex.label = NA)
# 
# 
# g3 <- sample_grg(100, 0.15, torus = FALSE, coords = TRUE)
# plot(g3, vertex.size = 5, vertex.label = NA)
# # plot(g3)
# 
# as_adj(g1)
