# rpart_cart.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2013 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the mda package before you work with it
install.packages("rpart")

# load the 'rpart' package
library(rpart)

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

# construct a model using CART
model <- rpart(z~x+y, data=train)
# summarize the model
summary(model)

# plot the model
plot(model)
text(model, use.n=TRUE)
