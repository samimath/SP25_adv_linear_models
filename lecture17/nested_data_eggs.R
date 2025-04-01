## hierarchical linear mixed model 
## example with eggs data
library(ggplot2)
library(faraway)
library(lme4)
data("eggs")
ggplot(eggs) + geom_boxplot(aes(x=Sample,y=Fat),fill='orange')+
  theme_bw()

ggplot(eggs) + 
  geom_boxplot(aes(x=Sample,y=Fat,fill=Technician))+
  facet_wrap(.~Lab, nrow=2) +
  theme_bw()

mod1 <- lmer(Fat ~ 1 + (1|Lab) + ## random effect from lab
               (1|Lab:Technician) +  ## nested random effect technician
               (1|Lab:Technician:Sample), ## nested random effect Sample
             data=eggs)


## Model inference:

## set initial model:
mod0 <- lmer(Fat ~ 1 + (1|Lab) + (1|Lab:Technician), data=eggs)
lrstat <- numeric(1000)
for(i in 1:1000){
  rFat <- unlist(simulate(mod0)) ## sample response from initial null model
  nmod <- lmer(rFat ~ 1 + (1|Lab) + (1|Lab:Technician), data=eggs) ## fit null model
  amod <- lmer(rFat ~ 1 + (1|Lab) + (1|Lab:Technician) + 
                 (1|Lab:Technician:Sample), data=eggs) ## fit alt model
  lrstat[i] <- 2*(logLik(amod)-logLik(nmod)) ## compare LRT
}
## estimate p-value :
est_p_val<-mean(lrstat > 2*(logLik(mod1)-logLik(mod0)))


pmod <- PBmodcomp(mod1, mod0)
summary(pmod)
