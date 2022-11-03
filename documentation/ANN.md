

```r
library(neuralnet)
```

```
## Error in library(neuralnet): there is no package called 'neuralnet'
```

```r
concrete<-read.csv(file = "concrete.txt",stringsAsFactors = F)#get the data
```

```
## Warning in file(file, "rt"): cannot open file 'concrete.txt': No such file or
## directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete<-as.data.frame(lapply(concrete, normalize))
```

```
## Error in lapply(concrete, normalize): object 'concrete' not found
```

```r
concrete_train<-concrete[1:773,]
```

```
## Error in eval(expr, envir, enclos): object 'concrete' not found
```

```r
concrete_test<-concrete[774:1030,]
```

```
## Error in eval(expr, envir, enclos): object 'concrete' not found
```

```r
concrete_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train,hidden = 5)
```

```
## Error in neuralnet(strength ~ cement + slag + ash + water + superplastic + : could not find function "neuralnet"
```

```r
model_res<-compute(concrete_model,concrete_test[,1:8])
```

```
## Error in compute(concrete_model, concrete_test[, 1:8]): could not find function "compute"
```

```r
x=model_res$net.result
```

```
## Error in eval(expr, envir, enclos): object 'model_res' not found
```

```r
y=concrete_test$strength
```

```
## Error in eval(expr, envir, enclos): object 'concrete_test' not found
```

```r
cor(x,y)
```

```
## Error in is.data.frame(y): object 'y' not found
```

```r
plot(concrete_model)
```

```
## Error in plot(concrete_model): object 'concrete_model' not found
```

