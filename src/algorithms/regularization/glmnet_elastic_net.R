# glmnet_elastic_net.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the 'glmnet' package before you work with it
install.packages("glmnet")
# load the 'glmnet' package
library(glmnet)

# regression problem, y depends on x1, x5 and x6 correlate with x1
regression_dataset <- function() {   
	x1 <- runif(100, 0, 10)
	x2 <- rnorm(100)
	x3 <- rnorm(100)
	x4 <- rnorm(100)
	x5 <- 0.1*x1 + rnorm(100)
	x6 <- 0.5*x1 + rnorm(100)
	y  <- x1 + rnorm(100)
	data <- data.frame(x1,x2,x3,x4,x5,x6,y)
}

# get the data 
data <- regression_dataset()
# split data in to train and test (67%/33%)
training_set <- sample(1:100, 67, FALSE)
train <- data[training_set,]
test <- data[-training_set,]

# create a matrix from the inputs
matrix <- model.matrix(~x1+x2+x3+x4+x5+x6, train)
# preapre a model using Elastic Net method and Coordinate Descent
model <- glmnet(
	matrix, # the matrix of preditors
	train$y, # the response variable
	family="gaussian", # gaussian function (linear regression)
	alpha=0.5, # Elastic Net mixing parameter (1=LASSO)
	nlambda=100, # number of lambda values to test
	lambda.min.ratio=0.0001, # used in selecting lambda's to test
	lambda=NULL, # sequence of lambda's to test (NULL=automatic)
	standardize=TRUE, # standardize data prior to fitting
	thresh=1E-7, # stop condition for coordinate descent
	type.gaussian="covariance") # efficientcy when number of vars<500
	
# plot the L1-norm of the coefficients
plot(model, xvar="norm", label=TRUE)


# extract coefficients
# coef(model, 0.005)





# create a matrix from the test data to make predictions
# matrix <- model.matrix(~x1+x2+x3+x4, test)
# # make predictions using the model
# predictions <- predict(model, matrix, type="class", s=0.01)
# # cunfusion matrix of the results
# table(predictions, test$z)
