## Compare effect of kernel estimators 
## generate some data for testing purposes:
set.seed(123)
n <- 100
eps <- rnorm(n, sd = 1)
fx <- function(x) {x^2*cos(x)}
X <- rnorm(n, sd = 1)
Y <- fx(X) + eps
x_grid<-seq(-5,5,0.1)
plot(X,Y, lwd=3)
lines(x_grid,fx(x_grid),col='forestgreen')

## ksmooth:
box_sm <- ksmooth(x=X,y=Y,kernel = 'box',bandwidth = 0.5)
gauss_sm <- ksmooth(x=X,y=Y,kernel = 'normal',bandwidth = 0.5)

plot(X,Y,lwd=2)
lines(box_sm$x,box_sm$y,col='blue',lwd=2)
lines(gauss_sm$x,gauss_sm$y,col='red',lwd=2)
lines(x_grid,fx(x_grid),col='forestgreen',lwd=2)



