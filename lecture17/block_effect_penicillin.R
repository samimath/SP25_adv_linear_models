### block effects example 
library(ggplot2)
library(faraway)
library(lme4)
data("penicillin")

penicillin$Blend <- gl(5,4) ## generate 5 factor levels each with 4 reps

## visualize the data: treatment by Blend
ggplot(penicillin)+geom_point(aes(x=treat,y=yield,
                                  shape=Blend,col=Blend),size = 3)+theme_bw()
## visualize the data: blend by treatment
ggplot(penicillin)+geom_point(aes(x=Blend,y=yield,
                                  shape=treat,col=treat),size = 3)+theme_bw()

## setting sum contrast options to reset the levels so it's easier to compare with random effects
options(contrasts=c("contr.sum", "contr.poly"))


lmod <- aov(yield ~ treat + blend , penicillin)
summary(lmod)
coef(lmod)

## creating a mixed effects model using blend as random effect
mmod <- lmer(yield ~ treat + (1|blend), penicillin)
sumary(mmod)


## model inference - how to tell if the treatment effect is significant?

## Method 1: using the AOV function by specifying the random effects from blend:

amod <- aov(yield ~ treat + Error(blend), penicillin)
summary(amod)

## Method 2: same as 1, but using ANOVA on the linear mixed model:

mmod <- lmer(yield ~ treat + (1|blend), penicillin)
anova(mmod)


## Method 3: More generalizable method (Kenward-Roger F test)
library(pbkrtest)
nmod <- lmer(yield ~ 1 + (1|blend), penicillin, REML=FALSE) ## random effects null model
amod <- lmer(yield ~ treat + (1|blend), penicillin, REML=FALSE) ## alternative model
## Estimate F-statistics using the K-R approach which is more accurate
KRmodcomp(amod, nmod)
# The pbkrtest package offers a convenient way to perform the parametric boot-
## strap for fixed effect terms:
pmod <- PBmodcomp(amod, nmod)
summary(pmod)

## this should be equivalent to the following:
deviance_val<- as.numeric(2*(logLik(amod)-logLik(nmod)))
pchisq(deviance_val,lower.tail = FALSE,df = 3)

lrstat <- numeric(1000)
for(i in 1:1000){
  ryield <- unlist(simulate(nmod))
  nmodr <- refit(nmod, ryield)
  amodr <- refit(amod, ryield)
  lrstat[i] <- 2*(logLik(amodr)-logLik(nmodr))
}

est_p_val<- mean(lrstat > deviance_val)
