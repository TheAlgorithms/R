# K folds cross validation is essential for machine learning
# createFolds function in package caret is easy to use
# here we write our own function

get_k_folds<-function(y = c(),k = 10, isList = TRUE, seed = 123){
  set.seed(seed)
  folds<-sample(1:length(y), length(y))
  every_n<-ceiling(length(y)/k)
  matFolds<-suppressWarnings(matrix(folds, ncol=every_n, byrow = T))
  
  if(isList){
    value<-NULL
    rownames(matFolds)<-paste("Folds",1:k,sep="")
    value<-lapply(1:k, function(x){
      if(x == k){
        return(matFolds[x,][1:(length(y)-every_n*(k-1))])
      }else{
        return(matFolds[x,])
      }
    })
  }else{
    value<-c()
    for(i in 1:length(y)){
      value[i]<-ceiling(i/every_n)
    }
  }
  
  return(value)
}
