linear_search<-function(vector, search_value){ #made a function named linear_search having two parameters that are an array and a value to be searched
  for(i in 1:length(vector)){ 
    if(vector[i]==search_value){ #comparing each value of array with the value to be searched
      return (i)
    }
  }
  return (-1)
}

user_vec<- c(10,20,30,40,50,60) #input array (hard code)
user_val<-30 #input value to be searched (hard code)

result<-linear_search(user_vec,user_val) #linear_seach function calling

if(result!=-1){
  cat("Searched value", user_val, "found at index", result-1) #displaying the index at which value is found (if any)
}else{
  cat("Searched value does not exist in array")
}
