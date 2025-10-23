shorten.vector <- function(vector,by){
  # get last elements
  vec_new <- vector |> tail(by) 
  
  # get index of last elements
  index <- c()
  for(i in vec_new){
    values <- which(vector == i) 
    index <- c(index,values)
  }
  
  # delete index from vector
  final_vec <- vector[-c(index)]
  
  # return final output
  return(final_vec)
  
  
}

fruits <- c("Pear","Banana","Peach","Grape","Apple","Orange")

shorten.vector(fruits,by = 1)
