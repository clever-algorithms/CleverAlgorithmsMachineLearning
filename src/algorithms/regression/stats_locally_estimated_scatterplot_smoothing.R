# stats_locally_estimated_scatterplot_smoothing.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# define 2D regression problem of 100 samples
regression <- function() {   
	x <- runif(100, 0, 10)
	y <- x^2 + rnorm(100)
	data.frame(x, y)	
}

# get the data 
data <- regression()
# split data in to train and test (67%/33%)
training_set <- sample(100,67)
train <- data[training_set,]
test <- data[(1:100)[-training_set],]

# create a model using Locally Estimated Scatterplot Smoothing
model <- loess(
	y~x, # the formular for prediction
	train, # the training dataset
	span=0.75, # degree of smoothing (proprtion of the dataset per model)
	degree=2, # order of polynomial (1=linear,2=quadratic)
	family="gaussian") # fit using least squares

# summarize the model
summary(model)

# order the test data by x so we can make a nice graph
test <- test[with(test, order(x,y)),]
# make predictions for the test data
predictions <- predict(model, test[,1])
# compute mean squared error
print(mean((test$y-predictions)^2))
# plot the loess curve
plot(test)
lines(test$x, predictions, lty=2, lwd=2)
