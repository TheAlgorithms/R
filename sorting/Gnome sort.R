# Gnome sort in R:

# Appy gnome sort algorithm
gnome.sort <- function(elements.vec) {
 index <- 1 # Create index variable and set value to 1
 j <- 1 # Create j variable and set value to 1
 while(index < length(elements.vec)) { # Return while loop if index lower then element count
   if(elements.vec[index] <= elements.vec[index + 1]) { # If current item lower or equals to next item
     index <- j # Set index to value of j
     j <- j + 1 # Increase j by one
    }
    else { # If not
      temp <- elements.vec[index] # Get current element
      elements.vec[index] <- elements.vec[index + 1] # Set current element to next element
      elements.vec[index + 1] <- temp # Set next element to current element
      index <- index - 1 # Decrease j by one
      if(index == 0) { # If index is first
        index <- j # Set index to value of j
        j <- j + 1 # Increase j by one
      } 
    }
  }
  return(elements.vec) # Return elements
}

# Example:
# gnome.sort(sample(1:100, 10))
# [1] 2 43 58 52 56 60 65 70 86 99
