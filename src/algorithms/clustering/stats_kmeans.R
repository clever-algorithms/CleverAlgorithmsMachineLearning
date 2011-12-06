# stats_kmeans.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# define a function that creates a 2d dataset
classification <- function() {   
	x <- c(rnorm(50, mean=0), rnorm(50, mean=4))
	y <- c(rnorm(50, mean=4), rnorm(50, mean=0))
	data <- data.frame(x, y)
}

# get the data 
data <- classification()
# construct a model using kmeans
model <- kmeans(data, 2)
# summarize the model
summary(model)
# summarize the results
table(model$cluster)

# visualize the data and the cluster centres
plot(data)
points(model$centers, pch=19)

