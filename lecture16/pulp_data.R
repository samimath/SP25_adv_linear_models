## hypothesis testing with mixed effects model - pulp data 
library(faraway)
library(lme4)
library(ggplot2)

## visualizing the data:
ggplot(pulp)+
  geom_point(aes(x=operator, 
                 y=bright,
                 col=operator),
             size = 3)+theme_bw()+
  geom_abline(slope = 0,
              intercept = mean(pulp$bright))


## model 0: null model:
nullmod <- lm(bright~1, pulp)
## model 1: mixed effects model:
mmod <- lmer(bright ~ 1+(1|operator), pulp,
             REML = FALSE)



## compute test statistic:
lrtstat <- as.numeric(2*(logLik(mmod)-logLik(nullmod)))
summary(mmod)
## compute p-value, the degrees of freedom is 1 here:
pvalue <- pchisq(lrtstat,1,lower=FALSE)
data.frame(lrtstat, pvalue)

## for comparison, consider a regular lm 
## that uses operator as fixed effect:
lmod0 <- lm(bright~1, data = pulp)
lmod <- lm(bright~operator, data = pulp)
## computet test statistic:
lrtstat_lm <- as.numeric(2*(logLik(lmod)-logLik(lmod0)))
## compute p-value, the degrees of freedom is 1 here:
pvalue_lm <- pchisq(lrtstat_lm,1,lower=FALSE)
data.frame(lrtstat_lm, pvalue_lm)

## as we can see the p-value from lme is questionable, seems inflated 
anova(lmod)


## Parametric bootstrap:
## Fit full model and null model to the data 

## Compute test statistic 

## Simulate pseudo-data from the null model
y <- simulate(nullmod)
sim_num = 1000
lrstat <- numeric(sim_num) ## run 1000 scenarios
set.seed(123)
for(i in 1:sim_num){
  y <- unlist(simulate(nullmod))
  bnull <- lm(y ~ 1) ## null model 
  balt <- lmer(y ~ 1 + (1|operator), 
               pulp, REML=FALSE) ## full model
  lrstat[i] <- as.numeric(2*(logLik(balt)-logLik(bnull))) ## test stat
}
## find the proportion of times the LRT test stat is close to zero
zero_prop <- mean(lrstat < 0.0001)

## what is the probability, given the H0 is true, that the observed LRT value is 2.568.
p_val <- mean(lrstat > 2.58)



## For curiosity, how does the result change as number of simulation changes?

LRT_bootstrap<-function(sim_num){
  
  y <- simulate(nullmod)
  #sim_num = 1000
  lrstat <- numeric(sim_num) ## run 1000 scenarios
  set.seed(123)
  for(i in 1:sim_num){
    y <- unlist(simulate(nullmod))
    bnull <- lm(y ~ 1) ## null model 
    balt <- lmer(y ~ 1 + (1|operator), 
                 pulp, REML=FALSE) ## full model
    lrstat[i] <- as.numeric(2*(logLik(balt)-logLik(bnull))) ## test stat
  }
  
  zero_prop <- mean(lrstat < 0.00001)
  p_val <- mean(lrstat > 2.586)
  
return(rbind(zero_prop,p_val))
}

sim_list<-c(3,50,100,500,1000,5000)

output <- unlist(lapply(sim_list, function(v){LRT_bootstrap(v)}))

plot(sim_list,output[seq(1, length(output), 2)],
     type='b',lwd=3,col='blue', main = '% of 0 LRT',
     xlab = '# of simulations', ylab = 'value')
plot(sim_list,output[seq(2, length(output), 2)],
     type='b',lwd=3,col='purple', main =  'estimated p-val',
     xlab = '# of simulations', ylab = 'value')


## comparing random effects estimate vs fixed effects 

mmod <- lmer(bright ~ 1+(1|operator), pulp) # random effects model
ranef(mmod)$operator
## using this to 're-code' the operator levels 
## so they are not just showing the marginal effects
options(contrasts=c("contr.sum", "contr.poly"))
lmod <- aov(bright ~ operator, pulp) # fixed effects model
unique(predict(lmod,pulp)-mean(pulp$bright))


vc  <-VarCorr(mmod)  # extract variance component estimates
varcomps<-c(unlist( lapply(vc, diag) ),  attr(vc,"sc")^2)
varcomps[1]/ (varcomps[1] + varcomps[2]/5) # shrinkage factor
