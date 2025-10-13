rearrangeWays <- function(string, as_report = TRUE){
  
  if(as_report){ # conditional statement
    
    # split the string into letters
    string_split <- c(unlist(strsplit(string,split = "")))
    
    # get the factorial of the letters vector
    possible_ways <- factorial(length(string_split))
    
    # create the answer text
    answer <- paste(string, "can be rearranged in", possible_ways, "possible ways")
    
    
    return(noquote(answer))
    
    
  }else{
    
    # split the string into letters
    string_split <- c(unlist(strsplit(string,split = "")))
    
    # get the factorial of the letters vector
    possible_ways <- factorial(length(string_split))
    
    return(possible_ways)
    
  }
  
  
}

rearrangeWays(string = "straight")
