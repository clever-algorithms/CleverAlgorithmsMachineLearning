# mda_mars.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the mda package before you work with it
# install.packages("earth")

# load the MASS package
library(earth)

# define the data
x <- runif(50, 0, 10)
y <- x^2 + rnorm(50)
data <- data.frame(x, y)

# create the Multivariate Adaptive Regression Splines model
model <-earth(y~x, data=data)
# summarize the model
summary(model)
# summarize the importance of input variables
evimp(model)

# plot the data
plot(data)
# plot the model fit
curve(predict(model, x, type="resp"), add=TRUE)

# plot diagnostics of the model
plot(model)
