## SDS4392 lecture 1
## basic review of linear models and R 
require(datasets) ## R library for built-in datasets 
require(stats) ## R statistical functions
require(visreg) ## Visualization of regression models

## let's try out one of the datasets, trees
## It collects the Diameter, Height and Volume for Black Cherry Trees
#data <- tibble(datasets::trees)
data <- datasets::trees
colnames(data)<-c('x1','x2','y')

## fitting a basic linear regression model
fit <- lm(y ~ x1 + x2 ,
          data=data)

## useful helper functions:

summary(fit) ## model summary 

coef(fit) ## model coefficient (beta)

fitted(fit) ## estimated response (yhat)

predict(object = fit,
        newdata = data.frame(x1=9.0,x2=68.1)) ## predicted response

## visualization residuals:

visreg(fit, 'x1')
visreg(fit, 'x2')

## visualizing model fit

plot(data$y~predict(fit,type = 'response'), main='simple linear regression')

