# gbm_gradient_boosting.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# you may have to install the 'gbm' package before you work with it
install.packages("gbm")
# load the 'gbm' package
library(gbm)

# define a z~x+y regression problem
regression <- function() {   
	x <- c(rnorm(50, mean=0), rnorm(50, mean=4))
	y <- c(rnorm(50, mean=4), rnorm(50, mean=0))
	z <- c(rep(1, 50), rep(0, 50))
	data.frame(x, y, z)
}

# get the data 
dataset <- regression()
# split data in to train and test (67%/33%)
training_set <- sample(1:100, 67, FALSE)
train <- dataset[training_set,]
test <- dataset[-training_set,]

# preapre a model using adaboost
model <- gbm(z~., data=train, distribution="bernoulli", n.trees=1000)
# summarize the model
summary(model)
# plot the model
plot(model)
# plot the performance over the iterations using 50% heldout test
gbm.perf(model, method="test")

# make predictions using the model
predictions <- predict(model, test[,-3], 1000)
# summarize the predictions
table(predictions, test$z)
# plot the summary of predictions
hist(predictions)
# compute sum squared error
print(sum((test$z-predictions)^2))
