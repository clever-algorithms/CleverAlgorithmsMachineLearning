# mass_sammon_mapping.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2013 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the MASS package before you work with it
install.packages("MASS")
# load the MASS package
library(MASS)

# define a function that creates a 3d dataset
dataset <- function() {   
	x <- c(rnorm(50, mean=0), rnorm(50, mean=4))
	y <- c(rnorm(50, mean=4), rnorm(50, mean=8))
	z <- c(rnorm(50, mean=8), rnorm(50, mean=10))
	data <- data.frame(x, y, z)
}

# get the data 
data <- dataset()
# create a distance matrix
distances <- dist(data)
# construct a model using sammon mapping
model <- sammon(distances)
# plot the model
plot(model$points)
