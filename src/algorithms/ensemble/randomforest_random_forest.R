# randomforest_random_forest.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the 'randomForest' package before you work with it
install.packages("randomForest")
# load the 'randomForest' package
library(randomForest)

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

# construct a model using Random Forest
model <- randomForest(z~., data=train)
# summarize the model
print(model)

# plot the model (trees vs error)
plot(model)

# make predictions
predictions <- predict(model, test[,1:2])
# confusion matrix of predictions
table(predictions, test$z)


