library(geoscale)

geoscalePlot(seq(0, 60, length.out = 100), rnorm(100, 49, 15), type = "l",
             cex.ts = 1, cex.age = 1, age.lim = c(60, 0), direction = "vertical")

