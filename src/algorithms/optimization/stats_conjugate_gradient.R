# stat_conjugate_gradient.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2013 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# definition of the 2D Rosenbrock function, optima is at (1,1)
rosenbrock <- function(v) {   
	(1 - v[1])^2 + 100 * (v[2] - v[1]*v[1])^2
}

# definition of the gradient of the 2D Rosenbrock function
derivative <- function(v) {
	c(-400 * v[1] * (v[2] - v[1]*v[1]) - 2 * (1 - v[1]), 
	  200 * (v[2] - v[1]*v[1]))
}

# locate the minimum of the function using the Conjugate Gradient method
result <- optim(
	c(runif(1,-3,3), runif(1,-3,3)), # start at a random position
	rosenbrock, # the function to minimize
	derivative, # no function gradient 
	method="CG", # use the Conjugate Gradient method
	control=c( # configure Conjugate Gradient
		maxit=100, # maximum iterations of 100
		reltol=1e-8, # response tolerance over-one step
		type=2)) # use the Polak-Ribiere update method
		
# summarise results
print(result$par) # the coordinate of the minimim
print(result$value) # the function response of the minimum
print(result$counts) # the number of function calls performed

# dispaly the function as a contour plot
x <- seq(-3, 3, length.out=100)
y <- seq(-3, 3, length.out=100)
z <- rosenbrock(expand.grid(x, y))
contour(x, y, matrix(log10(z), length(x)), xlab="x", ylab="y")
# draw the optima as a point
points(result$par[1], result$par[2], col="red", pch=19)
# draw a square around the optima to highlight it
rect(result$par[1]-0.2, result$par[2]-0.2, result$par[1]+0.2, 
	 result$par[2]+0.2, lwd=2)
