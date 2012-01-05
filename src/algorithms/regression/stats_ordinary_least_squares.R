# stats_ordinary_least_squares.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# define 2D regression problem of 100 samples
regression <- function() {   
	x <- runif(100, 0, 10)
	y <- x + rnorm(100)
	data.frame(x, y)	
}

# get the data 
data <- regression()
# split data in to train and test (67%/33%)
training_set <- sample(100,67)
train <- data[training_set,]
test <- data[(1:100)[-training_set],]

# create a linear regression model using ordinary least squares
model <- lm(
	y~x,			# predict Y given X
	train,			# training dataset
	NULL,			# no weighting on the variables
	NULL,			# no action on missing values
	method="qr")	# QR decomposition (efficient matrix calculation method)

# summarize the create linear regression model
summary(model)
# regression model diagnostic plots with the training data
par(mfrow=c(2,2))
plot(model)

# plot the training data and the line of best fit
plot(train)
abline(model$coef, lty=5, col="red")

# make predictions for the test data
predictions <- predict.lm(model, test[,1:2])
# compute mean squared error
print(mean((test$y-predictions)^2))
