
library(igraph)

g1 <- sample_grg(100, 0.05, torus = FALSE)

plot.igraph(g1, axes = TRUE)

tkplot(g1)


g <- make_ring(10)
tkplot(g)

## Saving a tkplot() to a file programatically
g <- make_star(10, center=10) 
E(g)$width <- sample(1:10, ecount(g), replace=TRUE)
lay <- layout_nicely(g)

id <- tkplot(g, layout=lay)
canvas <- tk_canvas(id)
tcltk::tkpostscript(canvas, file="/tmp/output.eps")
tk_close(id)

## Setting the coordinates and adding a title label
g <- make_ring(10)
id <- tkplot(make_ring(10), canvas.width=450, canvas.height=500)

canvas <- tk_canvas(id)
padding <- 20
coords <- norm_coords(layout_in_circle(g), 0+padding, 450-padding,
                      50+padding, 500-padding)
tk_set_coords(id, coords)

width <- as.numeric(tkcget(canvas, "-width"))
height <- as.numeric(tkcget(canvas, "-height"))
tkcreate(canvas, "text", width/2, 25, text="My title",
         justify="center", font=tcltk::tkfont.create(family="helvetica",
                                                     size=20,weight="bold"))