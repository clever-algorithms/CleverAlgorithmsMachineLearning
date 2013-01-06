# mass_linear_discriminant_analysis.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2013 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the MASS package before you work with it
# install.packages("MASS")

# load the MASS package
library(MASS)

# define test data with classes 0 and 1
x <- c(runif(25, 0, 5), runif(25, 6, 10))
y <- c(x+rnorm(25))
z <- c(rep("a", 25), rep("b", 25))
data <- data.frame(x, y, z)

# perform the linear discriminant analysis
model <- lda(z~x+y, data=data)

# display model details
model

# plot the data
plot(x,y)
# plot the class means
points(model$means[1], model$means[3], col="black", pch=19)
points(model$means[2], model$means[4], col="black", pch=19)
