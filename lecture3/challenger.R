## lecture 3: Introduction to logistic regression
# require(datasets) ## R library for built-in datasets 
# require(stats) ## R statistical functions
# require(visreg) ## Visualization of regression models
# require(broom) ## Data wrangling tool 
# require(dplyr) ## Data wrangling tool 
require(psych)
require(faraway) ## R package that comes with the book

## Faraway version of the Challenger data.
## Another version (with more columns) can also be 
## found in 'Challeng' dataset from {alr4} package

## simple data view
View(faraway::orings)

## plot response and predictor
plot(damage/6 ~ temp, orings,
     xlim=c(25,85), ylim = c(0,1),
     xlab="Temperature", 
     ylab="Prob of damage",
     pch = 19, col='red',
     main = 'fitting SLR to O-rings data')
## build simple linear model 
lmod <- lm(damage/6 ~ temp, orings)
abline(lmod)

summary(lmod)

## what are some issues that may arise?

## 1. predicted values outside of the range of probability
predict(lmod, newdata = data.frame(temp=80))
predict(lmod, newdata = data.frame(temp=10))
## 2. distribution of the probability is not normal, 
## leading to poor fit
## see histogram of the fitted y vs actual y
hist(lmod$fitted.values)
hist(orings$damage)

plot(lmod$fitted.values,lmod$residuals, pch = 19)

