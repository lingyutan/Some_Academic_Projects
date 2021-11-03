x <- seq(0, 1000, 1)

plot(x, y = 1 - plnorm(x, meanlog = 5.648811, sdlog = 1.084365), type ="l", col ="blue", lwd = 2)

lines(x, y = 1 - plnorm(x, meanlog = 5.422651, sdlog = 0.9050944), type ="l", col ="blue", lwd = 2)

