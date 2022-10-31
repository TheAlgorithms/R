custonKmeans<-function(dataset=NA,k=NA){
  if(is.na(dataset) || is.na(k)){
    stop("You must input valid parameters!")
  }
  Eudist<-function(x,y){
    distance<-sqrt(sum((x-y)^2))
    return (distance)
  }
  
  rows.dataset<-nrow(dataset)
  continue.change=TRUE
  initPoint<-dataset[sample.int(rows.dataset,size = k),]
  formerPoint<-initPoint
  iterPoint<-matrix(0,nrow = k,ncol = ncol(dataset))
  
  #记录每一个点到每一个类的距离
  error.matrix<-matrix(0,nrow=rows.dataset,ncol=k)
  while(continue.change){
    #记录每个点所属的类是哪一个
    cluster.matrix<-matrix(0,nrow=rows.dataset,ncol=k)
    for(i in 1:rows.dataset){#计算每个点到三个初始中心点的距离
      for(j in 1:k){
        error.matrix[i,j]<-Eudist(dataset[i,],formerPoint[j,])
      }
    }
    #将每一个点所属的类计算出来
    for(i in 1:rows.dataset){
      cluster.matrix[i,which.min(error.matrix[i,])]<-1
    }
    
    #更新新的质心位置
    for(i in 1:k){
      iterPoint[i,]<-apply(dataset[which(cluster.matrix[,i] == 1),],2,"mean")
    }
    all.true<-c()
    for(i in 1:k){
      if(all(formerPoint[i,] == iterPoint[i,]) == T){
        all.true[i]<-TRUE
      }
    }
    formerPoint = iterPoint
    continue.change=ifelse(all(all.true) == T,F,T)
  }
  colnames(iterPoint)<-colnames(dataset)
  out=list()
  out[["centers"]]<-iterPoint
  out[["distance"]]<-error.matrix
  out[["cluster"]]<-rep(1,rows.dataset)
  for(i in 1:rows.dataset){
    out[["cluster"]][i]<-which(cluster.matrix[i,] == 1)
  }
  return(out)
}
