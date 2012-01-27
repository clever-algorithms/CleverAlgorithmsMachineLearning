# lars_lasso_regression.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the 'lars' package
install.packages("lars")
# load the 'lars' package
library(lars)

# regression problem where y is dependent on x1
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

# create a matrix from the inputs
matrix <- model.matrix(~x1+x2+x3, train)
# preapre a model using lasso
model <- lars(
	matrix, # the matrix of preditors
	train$y, # the response variable
	type="lasso", # perform lasso
	trace=TRUE, # display progress
	normalize=TRUE, # standrize variables
	intercept=TRUE) # intercept is included and not penalized

# summarize the model
summary(model)
# plot the model
plot(model, breaks=TRUE)

# select a good step when the model had a minimum RSS
step <- model$df[which.min(model$RSS)]

# extract the selected coefficients from the fitted model
coef <- predict(model, matrix, s=step, type="coef")$coefficients
# print the selected variables
colnames(matrix)[which(coef!=0)]

# create a matrix from the test data to make predictions
matrix <- model.matrix(~x1+x2+x3, test)
# make predictions using the model
predictions <- predict(model, matrix, s=step, type="fit")$fit
# compute mean squared error
print(mean((test$y-predictions)^2))
