# e1071_support_vector_machine.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the mda package before you work with it
install.packages("e1071")

# load the 'e1071' package
library(e1071)

# define a function that creates a 2d classification problem
classification <- function() {   
	x <- c(rnorm(50, mean=0), rnorm(50, mean=4))
	y <- c(rnorm(50, mean=4), rnorm(50, mean=0))
	z <- c(rep("1", 50), rep("0", 50))
	data <- data.frame(x, y, z)
}

# get the data 
data <- classification()
# split data in to train and test (67%/33%)
training_set = sample(100,67)
train <- data[training_set,]
test <- data[(1:100)[-training_set],]

# construct a model using SVM
model <- svm(z~., data=train, method="C", kernel="radial")
# summarize the model
summary(model)

# plot the model and the decision boundary
plot(model, train, x~y, slice=list(x=3,y=4))

# make predictions
pred <- predict(model, test, decision.values=TRUE)
# summarize the predictions
table(pred)
