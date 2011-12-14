# glmnet_elastic_net.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the 'glmnet' package before you work with it
install.packages("glmnet")
# load the 'glmnet' package
library(glmnet)

classification <- function() {
	a <- runif(100, 1, 2)
	b <- runif(100, 2, 3)
	x <- c(rnorm(50, mean=0), rnorm(50, mean=4))
	y <- c(rnorm(50, mean=4), rnorm(50, mean=0))
	z <- c(rep("1", 50), rep("0", 50))
	data.frame(a, b, x, y, z)
}

# get the data 
dataset <- classification()
# split data in to train and test (67%/33%)
training_set <- sample(1:100, 67, FALSE)
train <- dataset[training_set,]
test <- dataset[-training_set,]

# create a matrix from the inputs
matrix <- model.matrix(~a+b+x+y, train)
# preapre a model using lasso
model <- glmnet(matrix, train$z, family="binomial")

# plot the model
par(mfrow=c(2,2))
plot(model, xvar="lambda", label=TRUE)
plot(model, xvar="norm", label=TRUE)
plot(model, xvar="dev", label=TRUE)

# extract coefficients
coef(model, 0.005)

# create a matrix from the test data to make predictions
matrix <- model.matrix(~a+b+x+y, test)
# make predictions using the model
predictions <- predict(model, matrix, type="class", s=0.01)
# cunfusion matrix of the results
table(predictions, test$z)
