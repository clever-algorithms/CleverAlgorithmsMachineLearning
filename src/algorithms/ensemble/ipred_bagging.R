# gbm_gradient_boosting.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the 'ipred' package before you work with it
install.packages("ipred")
# load the 'ipred' package
library(ipred)

# define a function that creates a 2d classification problem
classification <- function() {   
	x <- c(rnorm(50, mean=0), rnorm(50, mean=4))
	y <- c(rnorm(50, mean=4), rnorm(50, mean=0))
	z <- c(rep("1", 50), rep("0", 50))
	data.frame(x, y, z)
}

# get the data 
data <- classification()
# split data in to train and test (67%/33%)
training_set <- sample(1:100, 67, FALSE)
train <- data[training_set,]
test <- data[-training_set,]

# preapre a model using bagging
model <- bagging(z~x+y, data=train, nbagg=5, coob=TRUE)
# summarize the model
print(model)

# make predictions using the model
predictions <- predict(model, newdata=test[,1:2])
# confusion matrix of predictions
table(predictions, test$z)
