# Armstrong Number (also know as narcissistic numbers) checking in R

isArmstrong <- function(integer){
  
  digitAmount <- if(integer > 0){ 
    floor(log10(integer))+1
  } else if(integer == 0){
    1
  } else return(FALSE)
  
  temp <- integer
  Armstrong <- 0
  
  while(temp > 0){
    Armstrong <- Armstrong + (temp %% 10)^digitAmount
    temp <- floor(temp / 10)
  }
  
  if(Armstrong == integer){
    return(TRUE)
  } else return(FALSE)
  
}

isArmstrong(5) #returns TRUE
isArmstrong(29) #returns FALSE
isArmstrong(370) #returns TRUE
