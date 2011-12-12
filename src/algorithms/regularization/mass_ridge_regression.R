# mass_ridge_regression.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the MASS package before you work with it
# install.packages("MASS")

# load the MASS package
library(MASS)

# define sample data where most features are related to x
x <- runif(50, 0, 10)
y <- 2*x + rnorm(50)
a <- 3*x - rnorm(50)
b <- 6*x + rnorm(50)
data <- data.frame(x, y, a, b)

# perform a ridge regression with a large sequence of lambda values
model <- lm.ridge(y~x+a+b, data, lambda=seq(0, 0.1, 0.001))

# display the HKB, L-W, and GCV selected values of lambda
select(model)
# plot HKB, L-W, and GCV lambda values against the cost function
plot(model)
