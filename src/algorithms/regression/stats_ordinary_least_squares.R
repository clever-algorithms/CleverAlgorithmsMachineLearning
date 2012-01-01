# stats_ordinary_least_squares.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# define 2D regression problem of 50 samples
regression <- function() {
	x <- runif(50, 0, 10)
	y <- x + rnorm(50)
	data.frame(x, y)	
}


# prepare the dataset
data <- regression()
# linear model using ordinary least squares
model = lm(y~x, data)
# summarize the results
summary(model)

# plot the data
plot(data)
# plot the line of best fit
abline(model$coef, lty=5)
