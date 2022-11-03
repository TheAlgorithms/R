
First_n_Fibonacci <- function(n)
{
  # creating empty array of size n
  Fibonacci <- numeric(n)
  
  # assigning first 2 fibonacci values
  Fibonacci[1] <- 0
  Fibonacci[2] <- 1
  
  # finding the remaining fibonacci numbers using a for loop ranging from 3 to n
  for (i in 3:n) 
  {
    Fibonacci[i] <- Fibonacci[i - 2] + Fibonacci[i - 1]
  }  
  
  # printing the result
  print(Fibonacci)
}

First_n_Fibonacci(10) #returns 0 1 1 2 3 5 8 13 21 34
First_n_Fibonacci(15) #returns 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 



