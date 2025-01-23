## lecture 4: More logistic regression examples
# require(datasets) ## R library for built-in datasets 
# require(stats) ## R statistical functions
# require(visreg) ## Visualization of regression models
# require(broom) ## Data wrangling tool 
# require(dplyr) ## Data wrangling tool 
require(psych)
require(faraway)


## Faraway version of the Challenger data.
## Another version (with more columns) can also be 
## found in 'Challeng' dataset from {alr4} package

## simple data view
View(faraway::orings)



## let's build a generalized linear model with logit as the link function
glmod1 <- glm( formula = damage/6 ~ temp, 
              data = orings,
              family = binomial(link='logit'))


summary(glmod1)


glmod2 <- glm( formula = damage/6 ~ temp, 
               data = orings,
               family = binomial(link='probit'))


summary(glmod2)

## Let's put the data together
## plot response and predictor
plot(damage/6 ~ temp, orings,
     xlim=c(25,85), ylim = c(0,1),
     xlab="Temperature", 
     ylab="Prob of damage",
     pch = 19, col='red',
     main = 'fitting GLM to O-rings data')
x <- seq(25,85,1)
lines(x,ilogit(coef(glmod1)[1]+coef(glmod1)[2]*x),lty = 1)
lines(x,ilogit(coef(glmod2)[1]+coef(glmod2)[2]*x),lty = 2)
legend(70,1,legend = c('logit link','probit link'), lty = 1:2)





## 1. predicted values outside of the range of probability
predict(lmod, newdata = data.frame(temp=80))
predict(lmod, newdata = data.frame(temp=10))
## 2. distribution of the probability is not normal, 
## leading to poor fit
## see histogram of the fitted y vs actual y
hist(lmod$fitted.values)
hist(orings$damage)

plot(lmod$fitted.values,lmod$residuals, pch = 19)

