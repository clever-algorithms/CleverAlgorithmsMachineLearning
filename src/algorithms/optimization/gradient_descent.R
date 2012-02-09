# gradent_descent.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# define a 2D basin function, optima is at (0,0)
basin <- function(x) {
	x[1]^2 + x[2]^2
}

# define the derivative for a 2D basin function
derivative <- function(x) {
	c(2*x[1], 2*x[2])
}

# definition of the gradient descent method in 2D
gradient_descent <- function(func, derv, start, step=0.05, tol=1e-8) {
	pt1 <- start
	grdnt <- derv(pt1)
	pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
	while (abs(func(pt1)-func(pt2)) > tol) {
		pt1 <- pt2
		grdnt <- derv(pt1)
		pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
		print(func(pt2)) # print progress
	}
	pt2 # return the last point
}

# locate the minimum of the function using the Gradient Descent method
result <- gradient_descent(
	basin, # the function to optimize
	derivative, # the gradient of the function
	c(runif(1,-3,3), runif(1,-3,3)), # start point of the search			
	0.05, # step size (alpha)
	1e-8) # relative tolerance for one step

# display a summary of the results
print(result) # coordinate of fucntion minimum
print(basin(result)) # response of fucntion minimum

# dispaly the function as a contour plot
x <- seq(-3, 3, length.out=100)
y <- seq(-3, 3, length.out=100)
z <- basin(expand.grid(x, y))
contour(x, y, matrix(z, length(x)), xlab="x",ylab="y")
# draw the optima as a point
points(result[1], result[2], col="red", pch=19)
# draw a square around the optima to highlight it
rect(result[1]-0.2, result[2]-0.2, result[1]+0.2, 
	result[2]+0.2, lwd=2)
