# stat_nelder_mead.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# 2d Rosenbrock function, optima is at (1,1)
rosenbrock <- function(v) {   
	(1 - v[1])^2 + 100 * (v[2] - v[1]*v[1])^2
}

# what is ci?

# solve using constrOptim with the Nelder-Mead method
start <- c(2,2)
rs = optim(start, rosenbrock, NULL, method="Nelder-Mead")

# summarise results
rs$par # best coordinate
rs$value # best value
rs$counts # function calls
rs$convergence # final status of convergence, 0 is ok, 10 is failure

# plot the function
x <- seq(-2.048, 2.048, length.out=100)
y <- seq(-2.048, 2.048, length.out=100)
z <- rosenbrock(expand.grid(x, y))
contour(x, y, matrix(log10(z), length(x)))
# plot the optima
points(rs$par[1], rs$par[2], col="black", pch=19)
rect(rs$par[1]-0.2, rs$par[2]-0.2, rs$par[1]+0.2, rs$par[2]+0.2, border="black", lwd=2)