## Kernel smoothing functions
library(ggplot2)
library(KernSmooth)
library(faraway)

## example of smoothing spline:


x<-rnorm(100,3,1)
y = x*sin(x^2) + rnorm(100,0,1)
smr0 <- smooth.spline(x,y,lambda = 0.1)

plot(x,y)
z<-seq(0,5,l=100)
lines(z,z*sin(z^2),col='blue',lwd=1)
lines(smr0$x,smr0$y,col='red',lwd=2)



## effect of bandwidth
h <- seq(-15,2,l = 6)
smr <- as.data.frame(matrix(NA,nrow=NROW(faithful), 
                            ncol=length(h)))
est_plot<-list()

for(i in seq_along(h))
{
  smr <- smooth.spline(faithful$eruptions, 
                       faithful$waiting, 
                       lambda=exp(h[i]))
  smr <- data.frame(x=smr$x,y=smr$y)
  
  est_plot[[i]]<-ggplot(faithful) +
    geom_point(aes(x=eruptions,y=waiting),col='grey') +
    ggtitle(paste("Old Faithful (Smoothing splines, 
                  log(lambda)=",h[i],")", sep="")) +
    geom_line(data=smr, 
              aes(x=x, y=y), col='red')+theme_bw()
  
  
}

library(egg)
ggarrange(est_plot[[1]],
          est_plot[[2]],
          est_plot[[3]],
          est_plot[[4]],
          est_plot[[5]],
          est_plot[[6]],
          ncol = 3, nrow = 2)

## demonstration of smoothing splines for each of the examples:

par(mfrow = c(1,3))
with(faithful,{
  plot(waiting ~ eruptions, col=gray(0.75),main = 'Old Faithful')
  lines(smooth.spline(eruptions,waiting),lty=2, col = 'red')
})
with(exa,{
  plot(y ~ x, col=gray(0.75), main = 'Example A')
  lines(x,m)
  lines(smooth.spline(x,y),lty=2, col = 'red')
})
with(exb,{
  plot(y ~ x, col=gray(0.75), main = 'Example B')
  lines(x,m)
  lines(smooth.spline(x,y),lty=2, col = 'red')
})

