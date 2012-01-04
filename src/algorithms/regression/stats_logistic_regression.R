# stats_logistic_regression.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# define a function that creates a 2d classification problem
classification <- function() {   
	x <- c(rnorm(50, mean=0), rnorm(50, mean=4))
	y <- c(rnorm(50, mean=4), rnorm(50, mean=0))
	z <- c(rep("a", 50), rep("b", 50))
	data <- data.frame(x, y, z)
}

# get the data 
data <- classification()
# split data in to train and test (67%/33%)
training_set = sample(100,67)
train <- data[training_set,]
test <- data[(1:100)[-training_set],]

# create the discriminator using Logistic Regression
model <- glm(
	z~x+y, 										# model formula, z give x and y
	binomial(link="logit"),		# binomial using a logit link function
	train) 										# the training dataset

# summarize the fitted model
summary(model)

# plot the training dataset
plot(train$x, train$y, col=train$z, xlab="x", ylab="y")
# plot the model's decision boundary
slope <- coef(model)[2]/(-coef(model)[3])
intercept <- coef(model)[1]/(-coef(model)[3]) 
abline(intercept, slope, lty=5)

# regression model diagnostic plots with the training data
par(mfrow=c(2,2))
plot(model)

# make predictions
probabilities <- predict.glm(model, test[,1:2], type="response")
# convert probabilities into class values
predictions <- cut(probabilities, c(-Inf,0.5,Inf), labels=c("a","b"))
# summarize the predictions as a confusion matrix
table(predictions, test$z)
