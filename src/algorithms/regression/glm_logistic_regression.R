# glm_logistic_regression.R
# 
# The Clever Algorithms Project: http://www.CleverAlgorithms.com
# (c) Copyright 2011 Jason Brownlee. Some Rights Reserved. 
# This work is licensed under a Creative Commons Attribution-Noncommercial-Share Alike 2.5 Australia License.

# generate random health score values
health=sort(rnorm(20, 0, 50))
# <0 not likely to survive
survive=c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
# two column data
mydata=as.data.frame(cbind(health,survive))
# create fit
model=glm(survive~health, family=binomial(logit), mydata)
# display summary of the fit
summary(model)
# plot the points
plot(health, survive, xlab="Health Statistic",ylab="P(survival)")
# draw the prediction curve on the plot
curve(predict(model,data.frame(health=x),type="resp"),add=TRUE)
# draw the prediction points (P(survival) for each health)
points(health, fitted(model), pch=20)