## multivariate spline regression 
library(faraway)
library(splines)
library(rgl)


data(savings, package="faraway")
## visualize the data in 3D:
plot3d(x = savings$pop15, 
       y = savings$ddpi, 
       z = savings$sr,size = 5)

ggplot(savings)+geom_point(aes(x=pop15,
                                y=ddpi,
                                col=sr,
                               size = 2*sr))

y <- savings$sr
x <- cbind(savings$pop15,savings$ddpi)

## a few different methods for smoothing:
## spline regression:
smr<-sm.regression(x,y,
                   h=c(1,1), ## bandwidth
              xlab="pop15",
              ylab="growth",
              zlab="savingsrate")

## bivariate smoothing method
lomod <- loess(sr ~ pop15 + ddpi, data=savings)

xg <- seq(21,48,len=20)
yg <- seq(0,17,len=20)
zg <- expand.grid(pop15=xg,ddpi=yg)
persp(xg, yg, predict(lomod, zg), 
      theta=-35, ticktype="detailed", 
      xlab ="pop15", 
      ylab="growth", 
      zlab="savings rate",
      col=heat.colors(500))




## thin-plate fit (spline surface) using the mgcv package
library(mgcv)
amod <- gam(sr ~ s(pop15,ddpi), data=savings)
vis.gam(amod, col="gray", 
        ticktype="detailed",
        theta=-35)

