## IRLS example with binomial:

library(faraway)
library(broom)
library(dplyr)

View(bliss)
y <- bliss$dead/30;  
mu <- y
## eta = logit(mu) = X*beta
eta <- logit(mu)
z <- eta + (y-mu)/(mu*(1-mu))
w <- 30*mu*(1-mu)

## There are two ways to solve this:

## Method 1: use the matrix form
## weight matrix:
W <- diag(as.vector(w),length(y))
## X matrix
X <- cbind(rep(1,length(bliss$conc)),bliss$conc)
## estiamte beta_0 with initial estimates:
beta_0 <- solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%z
beta_i = beta_0
for(i in 1:5){
  ## update eta = X\beta 
  eta <-  X%*%beta_i
  ## update mu = ilogit(eta)
  mu <- ilogit(eta)
  ## update adjusted response:
  z <- eta + (y-mu)/(mu*(1-mu))
  ## update w:
  w <- 30*mu*(1-mu)
  ## update W:
  W <- diag(as.vector(w),length(y))
  ## update beta_i
  beta_i <- solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%z
  cat(i,beta_i,"\n")
}

 

## Method 2: use the output of the lm function (same as the book)
lmod <- lm(z  ~ conc, weights=w, bliss)
coef(lmod)


## Check model diagnostics:

augment(lmod) %>% select(.hat)