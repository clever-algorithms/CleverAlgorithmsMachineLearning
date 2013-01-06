# earth_multivariate_adaptive_regression_splines.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2013 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# install the 'earth' package (if needed)
install.packages("earth")
# load the 'earth' package
library(earth)

# define 2D regression problem of 100 samples
regression_dataset <- function() {   
	x <- runif(100, 0, 10)
	y <- x^2 + rnorm(100)
	data.frame(x, y)	
}

# get the data 
data <- regression_dataset()
# split data in to train and test (67%/33%)
training_set <- sample(100,67)
train <- data[training_set,]
test <- data[(1:100)[-training_set],]

# create a model using Multivariate Adaptive Regression Splines
model <- earth(
	y~x, # the formular for prediction
	train, # the training dataset
	trace=1, # provide overview information during model building
	nk=20, # the maximum number of terms
	degree=1, # the maximum number of interaction (degrees of freedom)
	penalty=2, # penality per knot for GCV during pruning
	thresh=0.001, # minimum change in SSR in forward stage
	minspan=1, # minimum distance between knots in the model
	fast.k=0, # disable Fast MARS adding multiple terms per forward step
	fast.beta=0, # aging coefficient used in Fast MARS
	pmethod="backward") # pruning method during backward pass

# summarize the model
summary(model)
# summarize the importance of input variables
evimp(model)
# plot diagnostics of the model
plot(model)
# plot the line of best fit for the training data
plotmo(model)

# make predictions for the test data
predictions <- predict(model, test[,1])
# compute mean squared error
print(mean((test$y-predictions)^2))
