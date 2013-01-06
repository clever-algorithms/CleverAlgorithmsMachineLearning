# mclust_expectation_maximization.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2013 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the mda package before you work with it
install.packages("mclust")

# load the 'mclust' package
library(mclust)

# define a function that creates a 2d dataset
dataset <- function() {   
	x <- c(rnorm(50, mean=0), rnorm(50, mean=4))
	y <- c(rnorm(50, mean=4), rnorm(50, mean=0))
	data <- data.frame(x, y)
}

# get the data 
data <- dataset()
# construct a model using kmeans
model <- Mclust(data)

# plot the model
plot(model, data)
