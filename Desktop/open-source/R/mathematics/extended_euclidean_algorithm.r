# Extended Euclidean Algorithm
#
# The Extended Euclidean Algorithm not only finds the Greatest Common Divisor (GCD)
# of two integers a and b, but also finds integers x and y such that:
# ax + by = gcd(a, b) (Bézout's identity)
#
# This is particularly useful in modular arithmetic, RSA cryptography, and 
# finding modular multiplicative inverses.
#
# Time Complexity: O(log(min(a, b)))
# Space Complexity: O(log(min(a, b))) due to recursion, O(1) for iterative version
#
# Input: Two integers a and b
# Output: A list containing gcd, and coefficients x, y such that ax + by = gcd(a, b)

# Recursive implementation of Extended Euclidean Algorithm
extended_gcd_recursive <- function(a, b) {
  # Base case
  if (b == 0) {
    return(list(
      gcd = abs(a),
      x = sign(a),  # Coefficient for a
      y = 0         # Coefficient for b
    ))
  }
  
  # Recursive call
  result <- extended_gcd_recursive(b, a %% b)
  
  # Update coefficients using the relation:
  # gcd(a, b) = gcd(b, a mod b)
  # If gcd(b, a mod b) = x1*b + y1*(a mod b)
  # Then gcd(a, b) = y1*a + (x1 - floor(a/b)*y1)*b
  x <- result$y
  y <- result$x - (a %/% b) * result$y
  
  return(list(
    gcd = result$gcd,
    x = x,
    y = y
  ))
}

# Iterative implementation of Extended Euclidean Algorithm
extended_gcd_iterative <- function(a, b) {
  # Store original values for final adjustment
  orig_a <- a
  orig_b <- b
  
  # Initialize coefficients
  old_x <- 1; x <- 0
  old_y <- 0; y <- 1
  
  while (b != 0) {
    quotient <- a %/% b
    
    # Update a and b
    temp <- b
    b <- a %% b
    a <- temp
    
    # Update x coefficients
    temp <- x
    x <- old_x - quotient * x
    old_x <- temp
    
    # Update y coefficients
    temp <- y
    y <- old_y - quotient * y
    old_y <- temp
  }
  
  # Adjust signs based on original inputs
  if (orig_a < 0) {
    a <- -a
    old_x <- -old_x
  }
  if (orig_b < 0) {
    old_y <- -old_y
  }
  
  return(list(
    gcd = abs(a),
    x = old_x,
    y = old_y
  ))
}

# Function to find modular multiplicative inverse
modular_inverse <- function(a, m) {
  #' Find modular multiplicative inverse of a modulo m
  #' Returns x such that (a * x) ≡ 1 (mod m)
  #' Only exists if gcd(a, m) = 1
  
  result <- extended_gcd_iterative(a, m)
  
  if (result$gcd != 1) {
    return(NULL)  # Inverse doesn't exist
  }
  
  # Make sure the result is positive
  inverse <- result$x %% m
  if (inverse < 0) {
    inverse <- inverse + m
  }
  
  return(inverse)
}

# Function to solve linear Diophantine equation ax + by = c
solve_diophantine <- function(a, b, c) {
  #' Solve the linear Diophantine equation ax + by = c
  #' Returns NULL if no integer solutions exist
  #' Returns one particular solution and the general solution pattern
  
  result <- extended_gcd_iterative(a, b)
  gcd_ab <- result$gcd
  
  # Check if solution exists
  if (c %% gcd_ab != 0) {
    return(NULL)  # No integer solutions exist
  }
  
  # Scale the coefficients
  scale <- c / gcd_ab
  x0 <- result$x * scale
  y0 <- result$y * scale
  
  return(list(
    particular_solution = list(x = x0, y = y0),
    general_solution = list(
      x_formula = paste0(x0, " + ", b/gcd_ab, "*t"),
      y_formula = paste0(y0, " - ", a/gcd_ab, "*t"),
      description = "where t is any integer"
    ),
    verification = a * x0 + b * y0 == c
  ))
}

# Function to find all solutions in a given range
find_diophantine_solutions_in_range <- function(a, b, c, x_min, x_max, y_min, y_max) {
  #' Find all integer solutions to ax + by = c in the given ranges
  
  dioph_result <- solve_diophantine(a, b, c)
  if (is.null(dioph_result)) {
    return(NULL)
  }
  
  x0 <- dioph_result$particular_solution$x
  y0 <- dioph_result$particular_solution$y
  gcd_ab <- extended_gcd_iterative(a, b)$gcd
  
  b_coeff <- b / gcd_ab
  a_coeff <- a / gcd_ab
  
  # Find range of t values
  t_min_x <- ceiling((x_min - x0) / b_coeff)
  t_max_x <- floor((x_max - x0) / b_coeff)
  t_min_y <- ceiling((y0 - y_max) / a_coeff)
  t_max_y <- floor((y0 - y_min) / a_coeff)
  
  t_min <- max(t_min_x, t_min_y)
  t_max <- min(t_max_x, t_max_y)
  
  if (t_min > t_max) {
    return(data.frame(x = integer(0), y = integer(0), t = integer(0)))
  }
  
  # Generate solutions
  t_values <- t_min:t_max
  x_values <- x0 + b_coeff * t_values
  y_values <- y0 - a_coeff * t_values
  
  return(data.frame(x = x_values, y = y_values, t = t_values))
}

# Example usage and testing
cat("=== Extended Euclidean Algorithm ===\n")

# Test basic extended GCD
cat("Testing Extended GCD with a=240, b=46:\n")
result1 <- extended_gcd_iterative(240, 46)
cat("GCD:", result1$gcd, "\n")
cat("Coefficients: x =", result1$x, ", y =", result1$y, "\n")
cat("Verification:", 240 * result1$x + 46 * result1$y, "= GCD:", result1$gcd, "\n")
cat("Check:", 240 * result1$x + 46 * result1$y == result1$gcd, "\n\n")

# Compare recursive and iterative methods
cat("Comparing recursive vs iterative methods:\n")
result_rec <- extended_gcd_recursive(240, 46)
result_iter <- extended_gcd_iterative(240, 46)
cat("Recursive - GCD:", result_rec$gcd, ", x:", result_rec$x, ", y:", result_rec$y, "\n")
cat("Iterative - GCD:", result_iter$gcd, ", x:", result_iter$x, ", y:", result_iter$y, "\n")
cat("Results match:", 
    result_rec$gcd == result_iter$gcd && 
    result_rec$x == result_iter$x && 
    result_rec$y == result_iter$y, "\n\n")

# Test modular multiplicative inverse
cat("=== Modular Multiplicative Inverse ===\n")
cat("Finding inverse of 7 modulo 26:\n")
inv <- modular_inverse(7, 26)
if (!is.null(inv)) {
  cat("7^(-1) ≡", inv, "(mod 26)\n")
  cat("Verification: 7 *", inv, "≡", (7 * inv) %% 26, "(mod 26)\n")
} else {
  cat("Inverse does not exist\n")
}

# Test case where inverse doesn't exist
cat("\nTesting case where inverse doesn't exist (a=6, m=9):\n")
inv2 <- modular_inverse(6, 9)
if (is.null(inv2)) {
  cat("Inverse does not exist (as expected, since gcd(6,9) = 3 ≠ 1)\n")
}

# Test solving Diophantine equations
cat("\n=== Linear Diophantine Equations ===\n")
cat("Solving 25x + 9y = 7:\n")
dioph1 <- solve_diophantine(25, 9, 7)
if (!is.null(dioph1)) {
  cat("Particular solution: x =", dioph1$particular_solution$x, 
      ", y =", dioph1$particular_solution$y, "\n")
  cat("General solution:\n")
  cat("  x =", dioph1$general_solution$x_formula, "\n")
  cat("  y =", dioph1$general_solution$y_formula, "\n")
  cat("  ", dioph1$general_solution$description, "\n")
  cat("Verification:", dioph1$verification, "\n")
} else {
  cat("No integer solutions exist\n")
}

# Test equation with no solutions
cat("\nSolving 6x + 9y = 10 (should have no integer solutions):\n")
dioph2 <- solve_diophantine(6, 9, 10)
if (is.null(dioph2)) {
  cat("No integer solutions exist (as expected, since gcd(6,9)=3 does not divide 10)\n")
}

# Find solutions in a specific range
cat("\n=== Solutions in Range ===\n")
cat("Finding solutions to 25x + 9y = 7 where -10 ≤ x ≤ 10 and -10 ≤ y ≤ 10:\n")
range_solutions <- find_diophantine_solutions_in_range(25, 9, 7, -10, 10, -10, 10)
if (!is.null(range_solutions) && nrow(range_solutions) > 0) {
  print(range_solutions)
  
  # Verify solutions
  cat("Verification of solutions:\n")
  for (i in 1:nrow(range_solutions)) {
    x <- range_solutions$x[i]
    y <- range_solutions$y[i]
    result_check <- 25 * x + 9 * y
    cat("25 *", x, "+ 9 *", y, "=", result_check, 
        "(", if(result_check == 7) "✓" else "✗", ")\n")
  }
} else {
  cat("No solutions found in the specified range\n")
}

# Applications example
cat("\n=== Practical Applications ===\n")
cat("Example: Making change with 3-cent and 5-cent coins\n")
cat("Problem: Can we make exactly 14 cents? Find all ways.\n")
change_problem <- solve_diophantine(3, 5, 14)
if (!is.null(change_problem)) {
  cat("Yes! Particular solution:", 
      change_problem$particular_solution$x, "three-cent coins and",
      change_problem$particular_solution$y, "five-cent coins\n")
  
  # Find all non-negative solutions
  solutions_range <- find_diophantine_solutions_in_range(3, 5, 14, 0, 10, 0, 10)
  if (nrow(solutions_range) > 0) {
    cat("All valid ways to make 14 cents:\n")
    for (i in 1:nrow(solutions_range)) {
      x <- solutions_range$x[i]
      y <- solutions_range$y[i]
      if (x >= 0 && y >= 0) {
        cat(x, "× 3¢ +", y, "× 5¢ = 14¢\n")
      }
    }
  }
}