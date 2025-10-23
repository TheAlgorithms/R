library(neuralnet)
concrete<-read.csv(file = "concrete.txt",stringsAsFactors = F)#get the data
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete<-as.data.frame(lapply(concrete, normalize))
concrete_train<-concrete[1:773,]
concrete_test<-concrete[774:1030,]
concrete_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train,hidden = 5)
model_res<-compute(concrete_model,concrete_test[,1:8])
x=model_res$net.result
y=concrete_test$strength
cor(x,y)
plot(concrete_model)
