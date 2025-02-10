subset_size<-function(q){
  s<-0
  for(k in 1:q){
    
    s <- s + choose(q,k)
  }
  
  return(s)
}

vec <- 1:10
output <- unlist(lapply(vec, function(v){subset_size(v)}))