library(pscl)
library(DataExplorer)
## modeling count data with excess zero counts:

data <- pscl::bioChemists

## 1. let's work on some data exploration:

## given there is a mix of categorical and continuous variables,
## we can use DataExplorer to check out the data too:
DataExplorer::create_report(data=data)

## 2. now let's try creating a basic Poisson regression model
## using 'art' as a response :
modp <- glm(art ~ ., data=bioChemists, family=poisson)

summary(modp)

## to investigate the data and the model predictions,
## we can summarize the response by counts
## observed count:
ocount <- as.numeric(table(bioChemists$art)[1:8])
## predicted count (predprob tells us P(Y=y))
## summing them up gives the expected value at each count:
pcount <- colSums(predprob(modp)[,1:8])
countdf<-data.frame(ocount=ocount,pcount=pcount)
## plotting them together 

plot(countdf$pcount,countdf$ocount,
     xlab="Predicted",
     ylab="Observed",
     main='Predicted vs Observed, Poisson regression',
     type = 'n', ## remove any markings since we'll annotate with text
     xlim = c(0,300),
     ylim = c(0,300))
text(pcount,ocount, 0:7,font = 10)
text(pcount[1],ocount[1], 0,col='red')
abline(0,1)



## From the above diagram we can see that 
## there are lots of students with zero publications 
## that were not picked up by the model

## 3. let's try the hurdle model (from the pscl package)
## to address the excess 0's:
?hurdle

## notice that in the hurdle model
## we can choose different models for the 0 portion
## and non-zero portion of the data
## by default, it uses Possion for the non-zero distribution
## and binomial for zero distribution with logit link function
modh <- hurdle(art ~ ., data=bioChemists)
summary(modh)


## 4. next let's try the zero-inflated model 
## (also from pscl package):

modz <- zeroinfl(art ~ ., data=bioChemists)
summary(modz)

## How to choose between the hurdle and zip model? 
## We can't really use deviance as these two are not nested models
## One way to do that is plot the fitted values to compare the fits:

plot(fitted(modh), fitted(modz), 
     xlab="Hurdle predictions", 
     ylab="ZIP predictions") 
abline(0,1)


## in the above diagram, we can see they are almost identical


## 5. Alternatively, we can also create another ZIP model 
## with a simpler set of predictors.
## In here we specify a different X components for Y>0 vs Y=0, 
## the two model construction is separated by | in the syntax below:

modz2 <- zeroinfl(art ~ fem+kid5+ment | ment, data=bioChemists)
summary(modz2)

## Now we can use deviance to compare the two models
## 
D <- 2 * (modz$loglik - modz2$loglik)
df_mod <- modz2$df.residual - modz$df.residual
pchisq(D,df_mod,lower.tail = F)

## The moderate p-value suggests that the simpler 
## zip model is likely to work just as well

## 7. Interpret model results -- suppose we decide to go with 
## the simpler ZIP model. Since Poisson model uses the log-link, 
## we should exponetiate the coeficients to interpret their effects:

exp(coef(modz2))

## 8. Checking predictions:

newman <- data.frame(fem="Men",mar="Married",kid5=0,ment=7)
predict(modz2, newdata=newman, type="prob")[,0:8]

predict(modz2, newdata=newman, type="zero")

