## GLMM with binary response
library(faraway)
library(lme4)
data("ctsib")
ctsib$stable <- ifelse(ctsib$CTSIB==1,1,0)

summary(ctsib)

## inspect the response variable :

xtabs(stable ~ Surface + Vision, ctsib)/80

ggplot(ctsib)+geom_bar(aes(y=stable/80, 
                           x = Surface, 
                           fill=Vision), 
                       stat = 'identity')+facet_wrap(.~Vision)+theme_bw()

## we can also summarize the data by some other attributes 
library(dplyr)
subsum <- ctsib %>% group_by(Subject) %>% group_by(Height,Weight,Age,Sex) %>%summarise(
                                                    stable=mean(stable))
ggplot(subsum, aes(x=Height,y=stable))+geom_point()
ggplot(subsum, aes(x=Weight,y=stable))+geom_point()
ggplot(subsum, aes(x=Age,y=stable))+geom_point()


gf1 <- glm(stable ~ Sex+Age+Height+Weight+Surface+Vision,
          family = binomial,
          data= ctsib)
sumary(gf1)


gf2 <- glm(stable ~ Sex+Age+Height+Weight+Surface+Vision+factor(Subject),
           family = binomial,
           data= ctsib)
sumary(gf2)

## implement random effects GLM:
library(MASS)
## in this implementation, parameters are estimated 
## based on a PQL function for a linearized model for y
modpql <- glmmPQL(stable ~ Sex + Age + Height + Weight + 
                    Surface +Vision,
                  random=~1|Subject, 
                  family=binomial,data=ctsib) 
summary(modpql)

## in this implementation, parameters are estimated 
## using numerical integration with Laplace approximation 
modlap <- glmer(stable ~ Sex + Age + Height + Weight + 
                  Surface + Vision + (1|Subject), 
                family=binomial, data=ctsib)

summary(modlap)

## compare with a simpler model
modlap2 <- glmer(stable ~ Surface + Vision + (1|Subject), 
                 family=binomial, data=ctsib)
anova(modlap, modlap2)





## 
modlap2 <- glmer(stable ~  Age + Height + Weight + 
                 Surface +Vision + (1|Subject),
               family=binomial, data=ctsib)

