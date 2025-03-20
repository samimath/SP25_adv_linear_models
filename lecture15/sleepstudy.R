## sleep study example with lmer:
library(lme4)
library(lattice)
data(sleepstudy)
#View(sleepstudy)


xyplot(Reaction ~ Days | Subject, 
       data=sleepstudy,
       type = c("g","p","r"),
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)", 
       aspect = "xy",layout=c(9,2))


fm1 <- lmer(Reaction ~ Days + (Days | Subject),
            sleepstudy,REML=FALSE)

## visualizing the effects by Subject (group)
ranef(fm1)

## distribution of random effects on the intercept
hist(ranef(fm1)[[1]][,1], 
     main = 'Distribution of random effect on model intercept',
     breaks = 10, col = 'blue',
     xlab = 'random effect on model intercept',
     ylab = ' frequency')

## distribution of random effects on the slope
hist(ranef(fm1)[[1]][,2],
     main = 'Distribution of random effect on model slope',
     breaks = 10, col='forestgreen',
     xlab = 'random effect on model slope',
     ylab = ' frequency')

fm2 <- lmer(Reaction ~ Days + (Days | Subject),
            sleepstudy,REML=TRUE)

fm2