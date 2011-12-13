# lars_lasso_regression.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the 'lars' package before you work with it
install.packages("lars")
# load the 'lars' package
library(lars)

# regression problem where y is dependent on x and the other variables are unrelated
regression <- function() {   
	a <- runif(100, 1, 2)
	b <- runif(100, 2, 3)
	x <- runif(100, 0, 10)
	y <- x + rnorm(100)
	data.frame(a, b, x, y)
}

# get the data 
dataset <- regression()
# split data in to train and test (67%/33%)
training_set <- sample(1:100, 67, FALSE)
train <- dataset[training_set,]
test <- dataset[-training_set,]

# create a matrix from the inputs
matrix = model.matrix(~a+b+x, train)
# preapre a model using lasso
model <- lars(matrix, train$y, type="lasso", trace=TRUE)

# summarize the model
summary(model)
# plot the model
plot(model, breaks=TRUE)

# select a good step
step <- model$df[which.min(model$RSS)]

# extract the selected coefficients from the fitted model
coef <- predict(model, matrix, s=step, type="coef")$coefficients
# print the selected variables
colnames(matrix)[which(coef!=0)]

# create a matrix from the test data to make predictions
matrix = model.matrix(~a+b+x, test)
# make predictions using the model
predictions <- predict(model, matrix, s=step, type="fit")$fit
# compute mean squared error
print(mean((test$y-predictions)^2))
