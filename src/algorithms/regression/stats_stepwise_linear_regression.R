# stats_stepwise_linear_regression.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# define 5 variable regression problem of 100 samples
regression <- function() {   
	x1 <- runif(100, 0, 10)
	x2 <- runif(100, 1, 2) 	# random
	x3 <- runif(100, 2, 3) 	# random
	y <- x1 + rnorm(100) 	# dependent on x1
	data <- data.frame(x1, x2, x3, y)	
}

# get the data 
data <- regression()
# split data in to train and test (67%/33%)
training_set <- sample(100,67)
train <- data[training_set,]
test <- data[(1:100)[-training_set],]

# create a linear regression model using ordinary least squares
base_model <- lm(
	y~x1+x2+x3,		# predict Y given X
	train,			# training dataset
	NULL,			# no weighting on the variables
	NULL,			# no action on missing values
	method="qr")	# QR decomposition (efficient matrix calculation method)
	
# apply the Stepwise Regression procedure	
selected_model <- step(
	base_model, 	# the model on which to operate
	y~x1+x2+x3,		# parameter relationships
	0, 				# estimate the sale for the AIC statistic
	"both",			# use forward and backward selection
	1, 				# provide debug information during the execution
	NULL, 			# no filter function for models
	1000, 			# maximum steps to execute
	2) 				# Use AIC as the test criterion (use log(n) for BIC)

# summarize the selected linear regression model
summary(selected_model)
# display the selected variables
names(selected_model$model)
# regression model diagnostic plots with the training data
par(mfrow=c(2,2))
plot(selected_model)

# plot the training data and the line of best fit
plot(selected_model$model)
abline(selected_model$coef, lty=5, col="red")

# make predictions for the test data
predictions <- predict.lm(selected_model, test[,1:3])
# compute mean squared error
print(mean((test$y-predictions)^2))