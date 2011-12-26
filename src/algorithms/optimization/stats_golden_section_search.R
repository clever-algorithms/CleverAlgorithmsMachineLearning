# stats_golden_section_search.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# basin function in 1-dimension, optima at f(0)=0
basin <- function(x) {
	x[1]^2
}

# search the function using gradient descent
bounds <- c(-5, 5)
precision <- .Machine$double.eps^0.25
xmin <- optimize(basin, bounds, tol=precision)

# display the results
xmin$minimum # x
xmin$objective # f(x)

# plot the function 
x <- seq(bounds[1], bounds[2], length.out=100)
y <- basin(expand.grid(x))
plot(x, y, xlab="x",ylab="f(x)", type="l")
# plot the solution as a point
points(xmin$minimum, xmin$objective, col="black", pch=19)
# draw a square around the optima to highlight it
rect(xmin$minimum-0.3, xmin$objective-0.7, xmin$minimum+0.3, xmin$objective+0.7, lwd=2)

