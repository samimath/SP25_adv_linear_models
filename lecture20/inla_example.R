## INLA method for fitting Bayesian mixed effects model
## install the package locally:
# install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)
library(INLA)
library(faraway)
library(lme4)
library(ggplot2)
data(pulp)
## iid: expect no correlation between operators 
## includes fixed effect by default 
## specify how the operator enters the model as a random effect using the f() function
formula <- bright ~ f(operator, model = 'iid') 

inla.mod1 <- inla(formula, family = 'gaussian', 
                data = pulp)
## precision here means 1/variance, to estimate random effects need to take sqrt of the inverse
## issue here: INLA introduces a non-informative prior by default

summary(inla.mod1)

## let a random effect have standard dev sigma, where sigma follows an exponential prior such that P(sigma > U) = alpha. 
## we set U = 3*SD(Y), meaning we are allowing it to have a small prob. of being 3 times larger than the SD of the data


## implement penalized complexity prior
sdres <- sd(pulp$bright)
pcprior <-list(prec = list(prior = 'pc.prec',
                           param = c(3*sdres,0.01)))
formula <- bright ~ f(operator, model = 'iid', hyper = pcprior) 
inla.mod2 <- inla(formula, family = 'gaussian', data = pulp)


## check marginal distribution:

sigma.epsilon <- inla.tmarginal(function(x) 1/sqrt(exp(x)),
                                inla.mod2$internal.marginals.hyperpar[[1]])

sigma.alpha <- inla.tmarginal(function(x) 1/sqrt(exp(x)),
                             inla.mod2$internal.marginals.hyperpar[[2]])

## plot the estimation
## build data frame
ddf <- data.frame(rbind(sigma.epsilon,sigma.alpha),
                  errterm= gl(2,nrow(sigma.alpha)))

ggplot(data = ddf) + aes(x,y, col = errterm)+ 
  geom_line(lwd = 2)+ xlab('sigma') + ylab('density') + theme_bw()

## show the result of operator effect:

rdf <- do.call(rbind.data.frame, 
               inla.mod2$marginals.random$operator)
rdf <- cbind(operator=gl(4,nrow(rdf)/4,labels=letters[1:4]),rdf)

ggplot(rdf)+aes(x,y,col= operator ) + geom_line(lwd = 2) + theme_bw()


## finally, taking a look at the overall summary:
## inla.zmarginal provides summary statistics
restab <- sapply(inla.mod2$marginals.fixed,
                 function(x)inla.zmarginal(x,silent = TRUE))
restab <- cbind(restab, inla.zmarginal(sigma.alpha,silent=TRUE))
restab <- cbind(restab, inla.zmarginal(sigma.epsilon,silent=TRUE))
restab <- cbind(restab, sapply(inla.mod2$marginals.random$operator,
                               function(x) inla.zmarginal(x, silent=TRUE)))
colnames(restab) = c("mu","sigma.alpha","sigma.epsilon","a","b","c","d")
resdf <- data.frame(restab)
print(resdf,digits=3)

## compare with lme implementation 
me.mod <- lmer(bright ~ 1+(1|operator), pulp,
             REML = FALSE)
me.s.alpha=data.frame(VarCorr(mmod))[1,5]
me.s.epsilon=data.frame(VarCorr(mmod))[2,5]
me.summary<-data.frame(mu = fixef(me.mod)[1], 
                       sigma.alpha =me.s.alpha,
                       sigma.epsilon = me.s.epsilon,
                       a = ranef(me.mod)$operator[[1]][1],
                       b = ranef(me.mod)$operator[[1]][2],
                       c = ranef(me.mod)$operator[[1]][3],
                       d = ranef(me.mod)$operator[[1]][4])

rbind(resdf,me.summary)