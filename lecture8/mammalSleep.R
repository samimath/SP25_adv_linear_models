## mammal sleep data exercise (modeling proportional response)
require(psych)
require(faraway)
data(mammalsleep, package="faraway")
mammalsleep$pdr <- with(mammalsleep, dream/sleep)
summary(mammalsleep$pdr)

pairs.panels(mammalsleep[,c('body','brain',
                            'exposure','danger')], 
             method = "pearson", # correlation method
             hist.col = "blue",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


## based on the initial data visualization, 
## we can see quite a few predictor 
## variables are heavily skewed,
## use log transformation in that case

modl <- glm(pdr ~ log(body) + 
              log(lifespan) + danger, 
            family=quasibinomial, mammalsleep ) 
summary(modl)

## Based on the deviance stats, 
## the model fit is not that great 
1 - pchisq(2.5-1.73,44-41)

## Let's try Beta regression instead.
## This requires the library mgcv
library(mgcv)
modb <- gam(pdr ~ log(body)+
              log(lifespan)+
              danger,
            family=betar(), 
            mammalsleep)
summary(modb)

## Compare fitted values of the two models:


plot(fitted(modl), modl$y,
     xlab = 'Quasi-binomial',
     ylab='Observed values',
     col='red',
     main = 'comparing fitted values of quasi-binomial vs Observed values')

plot(fitted(modb), modl$y,
     xlab = 'Beta-regression',
     ylab='Observed values',
     col='blue',
     main = 'comparing fitted values of beta-regression vs Observed values')

