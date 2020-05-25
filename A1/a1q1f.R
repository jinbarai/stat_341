set.seed(341)
pop <- rnorm(10000)

sc = function(y.pop, y, attr, ...) {
  N <- length(y.pop) + 1
  sapply(y, function(y.new) {
    N * (attr(c(y.new, y.pop), ...) - attr(y.pop, ...))
  })
}

spread <- function (pop) {
  (max(pop)-min(pop))
}

y <- seq(-7, 7, by = 0.01)

plot(y, 
     sc(y.pop = pop, y = y, attr = spread), 
     type = "l", 
     lwd = 2, 
     main = "Sensitivity curve for Spread",
     ylab = "sensitivity")

abline(h = 0, v = 0, col = "grey")


