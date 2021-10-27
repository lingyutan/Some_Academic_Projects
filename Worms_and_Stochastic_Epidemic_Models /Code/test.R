largest_comp <- function(graph) {
  cl <- components(graph)
  V(graph)[which.max(cl$csize) == cl$membership]
}
g <- sample_(gnp(100, 1/100),
             with_vertex_(size = 3, label = ""),
             with_graph_(layout = layout_with_fr)
)
giant_v <- largest_comp(g)
E(g)$color <- "orange"
E(g)[giant_v %--% giant_v]$color <- "blue"
plot(g)


E(g1)[1, 2, .inc(2)]

E(g1)[1, 2, .to(2)]


g2 <- make_ring(10) %>%
  set_vertex_attr("name", value = letters[1:10])
E(g2)


g <- make_empty_graph() %>%
  add_vertices(3, color = "red") %>%
  add_vertices(2, color = "green") %>%
  add_edges(c(1,2, 2,3, 3,4, 4,5))
g
V(g)[[]]
plot(g)


g <- make_star(10)
layout_as_star(g)


set.seed(42)
g <- make_ring(10)
plot(g, layout=layout_with_gem)


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


actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))














