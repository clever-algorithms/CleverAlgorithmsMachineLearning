# stats_golden_section_search.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# define a 1D basin function, optima at f(0)=0
basin <- function(x) {
	x[1]^2
}

# # locate the minimum of the function using a Golden Section Line Search
result <- optimize( 
	basin,			# the function to be minimized
	c(-5, 5),		# the bounds on the function paramter
	maximum=FALSE, 	# we are concerned with the function minima
	tol=1e-8)		# the size of the final bracketing

# display the results
print(result$minimum)	# function parameter
print(result$objective)	# function response

# plot the function 
x <- seq(-5, 5, length.out=100)
y <- basin(expand.grid(x))
plot(x, y, xlab="x",ylab="f(x)", type="l")
# plot the solution as a point
points(result$minimum, result$objective, col="red", pch=19)
# draw a square around the optima to highlight it
rect(result$minimum-0.3, result$objective-0.7, result$minimum+0.3, 
	 result$objective+0.7, lwd=2)

