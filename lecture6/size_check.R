size_check<-function(q){
  s<-0
  for(k in 1:q){
    s <- s+ choose(q,k)
    
  }
  
  return(s)
}


vec <- 1:10

output<- unlist(lapply(vec, function(s){size_check(s)}))

plot(vec,output, type = 'b', lwd=4)