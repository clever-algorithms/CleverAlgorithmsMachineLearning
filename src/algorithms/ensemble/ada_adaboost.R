# ada_adaboost.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the 'ada' package before you work with it
install.packages("ada")
# load the 'ada' package
library(ada)

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

# preapre a model using adaboost
model <- ada(z~., train, iter=20, loss="e", nu=1, type="discrete")
# summarize the model
print(model)

# add the test data
model <- addtest(model, test[,-3], test[,3])

# plot the model
plot(model, TRUE, TRUE)

# make predictions using the model
predictions <- predict(model, test[,-3])
# confusion matrix of predictions
table(predictions, test[,3])
