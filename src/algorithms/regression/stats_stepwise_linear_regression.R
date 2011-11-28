# stats_stepwise_linear_regression.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# data where y is dependent on x and the other variables are unrelated
x <- runif(50, 0, 10)
y <- x + rnorm(50)
a <- runif(50, 1, 2)
b <- runif(50, 2, 3)
data <- data.frame(x, y, a, b)

# build a linear model using ordinary least squares
base_model = lm(y~x+a+b, data)
# perform stepwise linear regression to select a model
selected_model <- step(base_model)

# summarize the selected model
summary(selected_model)
selected_model$anova

# plot the data
plot(selected_model$model)
# plot the line of best fit
abline(selected_model$coef, lty=5)
