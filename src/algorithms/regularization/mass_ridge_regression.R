# mass_ridge_regression.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2013 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

#  install the 'MASS' package (if needed)
install.packages("MASS")
# load the MASS package
library(MASS)

# define 4 variable regression problem with 100 samples
regression_dataset <- function() {   
	x1 <- runif(100, 0, 10)
	x2 <- rnorm(100)
	x3 <- 6*x1 + rnorm(100)
	y  <- x1 + rnorm(100)
	data <- data.frame(x1, x2, x3, y)
}

# get the data 
data <- regression_dataset()
# split data in to train and test (67%/33%)
training_set <- sample(100,67)
train <- data[training_set,]
test <- data[(1:100)[-training_set],]

# perform a Ridge Regression with a large sequence of lambda values
model <- lm.ridge(
	y~x1+x2+x3, # predict y given x1,x2,x3
	train, # training dataset
	lambda=seq(0, 0.1, 0.001)) # sequence of lambda values

# use a number of methods to select an optimal lambda value
select(model)

# plot the final coefficient values of each model for the given lambda values
matplot(model$lambda, t(model$coef), type="p", xlab="lambda", ylab="coef")
# draw a line at an estimated optimal lambda value
abline(v=0.08) # L-W estimator

# find lambda with the minumum GCV
which.min(model$GCV) # approximatly the 40'th row

# make predictions for the test data
predictions <- scale(test[,1:3], center=F, 
	scale=model$scales)%*%model$coef[,40]+mean(test$y)
# compute mean squared error
print(mean((test$y-predictions)^2))
