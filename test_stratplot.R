# generate data
set.seed(1)
var1  <- rnorm(100, 1:100, rnorm(100, 20))
varNA <- c(rnorm(n = 50, mean = 1:50, sd = rnorm(50, 20)), NA,
           rnorm(49, mean = 52:100, sd = rnorm(49, 20)))
age <- 1:100
ageNA <- c(1:40, NA, 42:100)
error <- rnorm(100, 20, 10)
errorNA <- error; errorNA[10] <- NA
errordf <- data.frame(error, error + 50)
errordfNA <- errordf; errordf[10, 1] <- NA

StratPlot(var1, age)
stratPlot(varNA, age)
stratPlot(var1, ageNA)
stratPlot(var1, age, agedir = "v")
stratPlot(var1, age, pb = "PB", pol0 = par("usr")[3]) 
# currently doesn't bound NA yet!
stratPlot(varNA, age, pb = "PB", pol0 = par("usr")[3]) 
stratPlot(var1, ageNA, pb = "PB", pol0 = par("usr")[3])

# errors
stratPlot(var1, age, error = 60)
stratPlot(varNA, age, error = 60)
stratPlot(var1, age, error = error)
stratPlot(varNA, age, error = error)
stratPlot(var1, age, error = errordf)
stratPlot(varNA, age, error = errordf)
stratPlot(var1, age, error = 60, errortype = "area")
stratPlot(varNA, age, error = 60, errortype = "area")
stratPlot(var1, age, error = error, errortype = "area")
stratPlot(varNA, age, error = error, errortype = "area")
stratPlot(var1, age, error = errordf, errortype = "area")
stratPlot(varNA, age, error = errordf, errortype = "area")


