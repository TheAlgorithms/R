# Bisection method
library(roxygen2)
library(docstring)

NMAX = 100 # maximum number of iterations
EPSILON = 1e-4 # a small positive quantity


func <- function(x) {
  #' Continuous function for which we want to find the root
  #' @param x Real input variable
  #' @returns The evaluation result of the function using the input value
  x^3 + 2.0*x - 10.0
}


bisection <- function(x_left, x_right, tolerance) {
  #' Bisection method is a root-finding method that applies to any continuous 
  #' function for which one knows two values with opposite signs.
  #' @description Finds the root value of a continuous function.
  #' @param x_left Float
  #' @param x_right Float
  #' @param tolerance Float
  #' @returns Root value
  #' @usage bisection(x_left, x_right, tolerance)
  #' @details The method consists of repeatedly bisecting the interval defined 
  #' by the two values and then selecting the subinterval in which the function 
  #' changes sign, and therefore must contain a root. It is a very simple and 
  #' robust method, but it is also relatively slow. Because of this, it is 
  #' often used to obtain a rough approximation to a solution which is then 
  #' used as a starting point for more rapidly converging methods.
  #' @references https://en.wikipedia.org/wiki/Bisection_method
  #' @author Aybars Nazlica https://github.com/aybarsnazlica
  
  n = 1 # step counter
  
  while(n <= NMAX) {
    middle = (x_left + x_right) / 2 # midpoint
    error = middle - x_left
    
    if (abs(func(middle)) < EPSILON || error < tolerance) {
      return(middle)
    }
    
    if (prod(sign(c(func(middle), func(x_left)))) > 0) { # if sign is positive
      x_left = middle # new lower endpoint
    } else {
      x_right = middle # new upper endpoint
    }
    
    n = n + 1 # increase step counter
  }
  print("Maximum number of steps exceeded!") # method failed
}


print(abs(bisection(1.0, 2.0, 1e-3) - 1.84668) < EPSILON) # returns TRUE
print(abs(bisection(100.0, 250.0, 1e-3) - 249.9994) < EPSILON) # returns TRUE
