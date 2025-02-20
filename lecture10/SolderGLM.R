### NB regression example with ATT data

data(solder, package="faraway")
## Let's start with fitting a Poisson model:
modp <- glm(skips ~ . , family=poisson, data=solder)
## Compare deviance and degrees of freedom of the model:
## the result (1829 on 882) indicates a poor fit
c(deviance(modp), df.residual(modp))
## This is also supported by a super low p-value:
pchisq(deviance(modp),df.residual(modp),lower=FALSE)

## Introduce interaction terms to see if the fit increases
modp2 <- glm(skips ~ (Opening +Solder + Mask + PadType + Panel)^2,
             family=poisson, data=solder)
deviance(modp2)
## p-value is improved, but not enough 
pchisq(deviance(modp2),df.residual(modp2),lower=FALSE)

library(MASS)
## This is a negative binomial with k=1 as an illustration
## Note, to use the negative.binomial function we need to call the MASS library
modn <- glm(skips ~ .,negative.binomial(1),solder)
modn

## Alternatively, we could also estimate k 
## as part of the MLE i.e. let k vary 
## and see which one results in the maximum likelihood:
modn <- glm.nb(skips ~ .,solder)
summary(modn)

## now let's try k=4 instead:

modn2 <- glm(skips ~ .,negative.binomial(4),solder)
summary(modn2)

## model inference:
pchisq(deviance(modn2),df.residual(modn2),lower=FALSE)


