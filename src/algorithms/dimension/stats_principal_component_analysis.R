# stats_principal_component_analysis.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2013 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# define a function that creates a 3d dataset
dataset <- function() {   
	x <- c(rnorm(50, mean=0), rnorm(50, mean=4))
	y <- c(rnorm(50, mean=4), rnorm(50, mean=8))
	z <- c(rnorm(50, mean=8), rnorm(50, mean=0))
	data <- data.frame(x, y, z)
}

# get the data 
data <- dataset()
# construct a model using principal component analysis
model <- prcomp(data, scale=TRUE)
# summarize the model
summary(model)
# plot the projection
biplot(model)
