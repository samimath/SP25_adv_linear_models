## GLM example with Poisson regression 
library(faraway)
library(ggplot2)
data(gala, package="faraway")
# remove the endemic species response for the purpose of the exercise 

gala <- gala[,-2]

## visualization of the data

## distribution of some of the response variable of interest:

hist(gala$Species, 
     main = '# of plant species', 
     col = 'forestgreen')


## we can start by trying the normal regression model.
mod1 <- lm(Species~., data = gala)
## In the model diagnostics plot, we can see a clear 
## violation of constant variance 
plot(mod1,1)

## experimenting with variable transformation (e.g. sqrt)
mod2 <- lm(sqrt(Species)~.,data = gala)
## this provides a better overall fit, but the results are harder to interpret 
## due to the transformation
summary(mod2)

## we can try Poisson regression:
modp <- glm(Species~. , 
            family = poisson, 
            data = gala)
summary(modp)

## MODEL INFERENCE

## Let's check some model statistics:

## Deviance:
pchisq(modp$deviance,modp$df.residual, lower.tail = F)

## Pearson's chi2:
pearson_chi <- sum(residuals(modp,type = 'pearson')^2)
pchisq(pearson_chi,modp$df.residual, lower.tail = F)

## both shows that the model fit is not the best 

## In addition, we can estimate the % deviance explained by the model 
dev_explained <- 1 - modp$deviance/modp$null.deviance

## How about plotting model fit vs response?
plot(gala$Species,
     fitted(modp), 
     main = 'Model fit vs Response')

plot(log(gala$Species),
     log(fitted(modp)),
     main = 'Model fit vs Response on log scale')

## Another way to investigate model 
## inference is checking for dispersion.
## In here, we can approximate variance by (y-\mu)^2
## Side note: expression() is an R command that generates 
## math expression using LaTeX input
plot(log(fitted(modp)), 
     log((gala$Species-fitted(modp))^2), 
     xlab = expression(hat(mu)),
     ylab = expression((y-hat(mu))^2))
abline(0,1)

## Implementing quasi-Poisson model 
## in the presence of over-dispersion
modd <- glm(Species ~ ., 
            family=quasipoisson, gala)

## Testing the significance of each of
## the predictors relative to the full model:
drop1(modd,test="F")

## Interpreting the model
## Since P(Y_i =y) follows a Poisson dist.
## We only need the following:
## eta_i = linear predictors 
## y = variable value for the response

## As an example,
## consider the observation for Wolf:
modp$linear.predictors[30]

modp$coefficients
modp$data[30,]

beta<-as.numeric(modp$coefficients)
xi <- as.numeric(cbind(1,modp$data[30,-1]))
eta_i<-beta%*%xi

## mu_i = 2.894143
## P(Y_i =y)

p_Wolf <- function(y){ 
  exp(-exp(2.894143))*(exp(2.894143)^y)/(factorial(y))
}

plot(0:100,p_Wolf(0:100), 
     type = 'l',
     lty = 2,
     lwd = 2,
     xlab = 'y',
     ylab ='P(Y_30=y)',
     main = 'Probability estimate for Wolf island')


## Generalizing:
p_yi_gala <- function(i,y){
  eta <- modp$linear.predictors[i]
  mu <- exp(eta)
  p_yi <- (exp(-mu)*mu^y)/(factorial(y))
  return(p_yi)
}

plot(0:100,p_yi_gala(1,0:100), 
     type = 'l',
     lty = 2,
     lwd = 2,
     xlab = 'y',
     ylab ='P(Y_1=y)',
     main = 'Probability estimate for Baltra island')
