## More regression spline example:

## piece-wise linear splines:
library(faraway)

lins <- function(x, c){
  ifelse(x>c, (x-c), 0)
}

quad<-function(x,c){
  ifelse(x>c, (x-c)^2, 0)
}
## show the plot:
curve(quad(x,0.5),0,1)


## now define some knots to show what the linear spline looks
## like as a set of basis function
exa <- faraway::exa
knots<- c(0,0.4,0.9,0.95)

## create an outer product array to evaluate the rhs function
dm <- outer(exa$x , knots, lins)
matplot(exa$x, dm, type = 'l', col = 1, xlab = 'x', ylab = '')


dm2 <- outer(exa$x , knots, quad)
matplot(exa$x, dm2, type = 'l', col = 1, xlab = 'x', ylab = '')

## once the knots are defined, this becomes a regression problem:

lmod <- lm(exa$y~dm2)
plot(y~x, data = exa, col = gray(0.75))
lines(exa$x, predict(lmod), lwd = 2)
## question here: can we adjust the knot placement?

## let's try example B (with an outlier)
exb <- faraway::exb
knots_b <- seq(-1,11,0.01)
dm_b <- outer(exb$x , knots_b, quad)
lmod_b <- lm(exb$y~dm_b)
plot(y~x, data = exb, col = gray(0.75))
lines(exb$x, predict(lmod_b))



## instead of piece-wise linear, 
## let's try cubic splines (these are called B-splines)
library(splines)
## specify 10 knots over 100 sample points:
matplot(bs(seq(0,1,length=100),df=10),
        type="l",ylab="",col='blue',lwd=2)

# repeat the process of fitting a spline regression:
nk = 5
bs_plot<-function(nk,data){
lmod <- lm(y ~ bs(x,nk),data)
plot(y ~ x, exa, col=gray(0.75),
     main = paste('num. of knots :',nk))
lines(m ~ x, data)
lines(predict(lmod) ~ x, data, lty=2, col='red')
}

par(mfrow = c(2,2))

bs_plot(nk = 4, data = exa)
bs_plot(nk = 10, data = exa)
bs_plot(nk = 20, data = exa)
bs_plot(nk = 50, data = exa)
confint(lm(y ~ bs(x,nk),data),level = 0.95)
## how to construct confidence interval?
## because B-splines uses regression method to get the coef,
## we can use that to derive confidence interval

ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  geom_line(aes(x=x,y=m),col='red')+
  geom_smooth(aes(x=x,y=y), 
              method = 'gam',
              level = 0.99,
              formula = y~s(x,k=8))+theme_bw()


## Another way to implement B-splines:
## natural spline example:
library(splines)
## df = K+3 
fit <- lm(waiting ~ ns(eruptions, df=6), faithful)

ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful (Natural splines, 6 df)") + 
  geom_line(aes(x=eruptions, y=fitted(fit)), col='blue')+
  theme_bw()

fit2 <- lm(waiting ~ ns(eruptions, df=10), faithful)

ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful (Natural splines, 10 df)") + 
  geom_line(aes(x=eruptions, y=fitted(fit2)), col='purple')+
  theme_bw()


## local polynomial:
smr <- loess(waiting ~ eruptions, 
             span=0.75, ## window size
             degree=1,  ## locally linear
             family="gaussian", ## fitting by least square
             faithful #data 
)

ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful (Loess, span=0.75)") + 
  geom_line(aes(x=eruptions, y=fitted(smr)), col='purple')+theme_bw()

