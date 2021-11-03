res <- ecdf(cancer$time)
r <- range(cancer$time)
curve(1 - res(x), from=r[1], to=r[2], col="red", xlim=r)
