# stat_conjugate_gradient.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# 2d Rosenbrock function, optima is at (1,1)
rosenbrock <- function(v) {   
	(1 - v[1])^2 + 100 * (v[2] - v[1]*v[1])^2
}

# 2d gradient for Rosenbrock function
gradient <- function(v) {
	c(-400 * v[1] * (v[2] - v[1]*v[1]) - 2 * (1 - v[1]), 
	  200 * (v[2] - v[1]*v[1]))
}


# prepare a random starting position in the domain
start <- c(runif(1, -3, 3), runif(1, -3, 3))
print(start) # display the starting position
# set the Conjugate_Gradient update method to 2 (Polak-Ribiere)
ctrl <- list(type=2)
# solve using optim with the Conjugate_Gradient method 
rs <- optim(start, rosenbrock, gradient, method="CG", control=ctrl)

# summarise results
rs$par # best coordinate
rs$value # best value
rs$counts # function calls

# dispaly the function as a contour plot
x <- seq(-3, 3, length.out=100)
y <- seq(-3, 3, length.out=100)
z <- rosenbrock(expand.grid(x, y))
contour(x, y, matrix(log10(z), length(x)))
# draw the optima as a point
points(rs$par[1], rs$par[2], col="black", pch=19)
# draw a square around the optima to highlight it
rect(rs$par[1]-0.2, rs$par[2]-0.2, rs$par[1]+0.2, rs$par[2]+0.2, lwd=2)
