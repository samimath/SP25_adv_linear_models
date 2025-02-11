library(faraway)
library(dplyr)
library(broom)

## Binomial regression example with Challenger data

data(orings)

## data manipulation:
## add a new column for O-rings data to 
## address 'success' and 'failure'
orings <- orings  %>%
  mutate(
    undamaged=6-damage, 
    pdamage = damage/6) 

summary(orings)

## Let's visualize the proportion of 
## damage as a function of temp
ggplot(orings, aes(x=temp, y=pdamage)) +
  geom_point(size = 2, col = 'red') +
  xlab("Temperature") + 
  ylab("Prop. of damage") + theme_bw()


## Originally we built a logistic 
## regression model where
## y = prob. of damage (0,1):
glmod0 <- glm(pdamage ~ temp,
             family=binomial, orings)
summary(glmod0)




## In the case of binomial 
## regression, we create a 
## matrix of success and failures:
glmod <- glm(cbind(damage,undamaged) ~ temp,
            family=binomial, orings)
## Let's take a look at model summary:
summary(glmod)

## Let's also take a look at the model data:
head(glmod$data)

## Test for model fit using deviance. 
## Note: Setting lower.tail = False gives 
## the area under the curve to the right of 
## the value being evaluated. In here we can conclude 
## that temp. is statistically significant
pchisq(deviance(glmod),
       df.residual(glmod),lower=FALSE)


## Test for model fit using Pearson's chi square 
## (should be similar to deviance). 
## This can be computed by specifying 
## the 'pearson' residuals of the GLM object:
pearson_chi <- sum(residuals(glmod, type = "pearson")^2)
pchisq(pearson_chi, df_glmod,lower.tail = FALSE)




## Confidence intervals for binomial model 
confint(glmod)

## Confidence intervals for binary model 
confint(glmod0)


# Look for overdispersion

summary(glmod)
# Dev (16.9) < m-p (21) --> no signs of overdispersion
# But here is how to deal with it if needed:

glmod2 <- glm(cbind(damage,undamaged) ~ temp, 
             family=quasibinomial, orings)
summary(glmod2)

## Compare the confidence intervals between the two models 
## the model with quasibinomial likelihood has a narrower CI
confint(glmod2)


## Checking mode assumption 

plot(glmod1)

