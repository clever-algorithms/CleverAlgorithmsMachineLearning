# stat_nelder_mead.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# definition of the 2D Rosenbrock function, optima is at (1,1)
rosenbrock <- function(v) {   
	(1 - v[1])^2 + 100 * (v[2] - v[1]*v[1])^2
}

# locate the minimum of the function using the Nelder-Mead method
result <- optim(
	c(runif(1,-3,3), runif(1,-3,3)), # start at a random position
	rosenbrock, # the function to minimize
	NULL, # no function gradient 
	method="Nelder-Mead", # use the Nelder-Mead method
	control=c( # configure Nelder-Mead
		maxit=100, # maximum iterations of 100
		reltol=1e-8, # response tolerance over-one step
		alpha=1.0, # reflection factor
		beta=0.5, # contraction factor
		gamma=2.0)) # expansion factor
		
# summarise results
print(result$par) # the coordinate of the minimim
print(result$value) # the function response of the minimum
print(result$counts) # the number of function calls performed

# dispaly the function as a contour plot
x <- seq(-3, 3, length.out=100)
y <- seq(-3, 3, length.out=100)
z <- rosenbrock(expand.grid(x, y))
contour(x, y, matrix(log10(z), length(x)), xlab="x",ylab="y")
# draw the optima as a point
points(result$par[1], result$par[2], col="red", pch=19)
# draw a square around the optima to highlight it
rect(result$par[1]-0.2, result$par[2]-0.2, result$par[1]+0.2, 
	 result$par[2]+0.2, lwd=2)
