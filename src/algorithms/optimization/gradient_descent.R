# gradent_descent.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# 2d basin function, optima is at (0,0)
basin <- function(x) {
	x[1]^2 + x[2]^2
}

# 2d derivative for basin function
derivative <- function(x) {
	c(2*x[1], 2*x[2])
}

# definition of the gradient descent method in 2-dimensions
gradient_descent <- function(func, derv, start, step=0.05, precision=1e-6) {
	pt1 <- start
	grdnt <- derv(pt1)
	pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
	while (abs(func(pt1)-func(pt2)) > precision) {
		pt1 <- pt2
		grdnt <- derv(pt1)
		pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
		print(func(pt2)) # print progress
	}
	pt2 # return the last point
}

# select a random starting position
pt <- c(runif(1, -3, 3), runif(1, -3, 3))
# perform a gradient descent
rs <- gradient_descent(basin, derivative, pt)

# dispaly the function as a contour plot
x <- seq(-3, 3, length.out=100)
y <- seq(-3, 3, length.out=100)
z <- basin(expand.grid(x, y))
contour(x, y, matrix(z, length(x)))
# draw the optima as a point
points(rs[1], rs[2], col="black", pch=19)
# draw a square around the optima to highlight it
rect(rs[1]-0.2, rs[2]-0.2, rs[1]+0.2, rs[2]+0.2, lwd=2)
