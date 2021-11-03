shape = 1.466944
scale = 336.699601

y <- rweibull(1000, shape, scale)

curve(1-pweibull(x, shape, scale), from = -5, to = 1000)

