### logistic regression overview:
name_of_function <- function(arg){
y = arg**2
return(y)
}
g_inv<-function(x){
  return(exp(x)/(1+exp(x)))
  }

x <- seq(-5,5,by=0.1)
plot(x,g_inv(x), type = 'l',xlab = 'x',ylab = 'p', 
     main = 'inverse logit function plot (maps R to (0,1]')

