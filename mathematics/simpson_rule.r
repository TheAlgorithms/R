# Simpson's Rule for Numerical Integration
#
# Simpson's Rule is a method for numerical integration that approximates 
# the definite integral of a function using quadratic polynomials.
# It provides better accuracy than the trapezoidal rule by using parabolic 
# arcs instead of straight line segments.
#
# Time Complexity: O(n) where n is the number of subintervals
# Space Complexity: O(n) for storing the x values
#
# Applications:
# - Numerical integration in scientific computing
# - Physics and engineering calculations
# - Signal processing and data analysis
# - Probability and statistics (computing areas under curves)
# - Computer graphics and visualization
#
# Formula: ∫[a,b] f(x)dx ≈ (h/3)[f(x₀) + 4f(x₁) + 2f(x₂) + 4f(x₃) + ... + f(xₙ)]
# where h = (b-a)/n and n must be even

simpson_rule <- function(f, a, b, n) {
  #' Approximate the definite integral of f(x) from a to b using Simpson's Rule
  #' @param f: Function to integrate
  #' @param a: Lower limit of integration (numeric)
  #' @param b: Upper limit of integration (numeric)
  #' @param n: Number of subintervals (must be even)
  #' @return: Approximation of the integral (numeric)
  #' @details Simpson's Rule uses parabolic approximations to estimate the 
  #'          area under a curve. The interval [a,b] is divided into n 
  #'          subintervals, and quadratic interpolation is applied.
  #' @references https://en.wikipedia.org/wiki/Simpson%27s_rule
  
  # Validate that n is even
  if (n %% 2 != 0) {
    stop("Error: Number of subintervals n must be even for Simpson's Rule.")
  }
  
  # Validate inputs
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(n)) {
    stop("Error: a, b, and n must be numeric values.")
  }
  
  if (n <= 0) {
    stop("Error: Number of subintervals n must be positive.")
  }
  
  if (a >= b) {
    stop("Error: Lower limit a must be less than upper limit b.")
  }
  
  # Calculate step size
  h <- (b - a) / n
  
  # Generate x values
  x <- seq(a, b, by = h)
  
  # Initialize result with endpoints
  result <- f(x[1]) + f(x[n + 1])
  
  # Apply Simpson's coefficients
  # Odd indices (i = 3, 5, 7, ...) get coefficient 4
  # Even indices (i = 2, 4, 6, ...) get coefficient 2
  for (i in 2:n) {
    if (i %% 2 != 0) {
      # Odd index: multiply by 4
      result <- result + 4 * f(x[i])
    } else {
      # Even index: multiply by 2
      result <- result + 2 * f(x[i])
    }
  }
  
  # Multiply by h/3
  result <- result * h / 3
  
  return(result)
}

# Vectorized version for better performance
simpson_rule_vectorized <- function(f, a, b, n) {
  #' Vectorized implementation of Simpson's Rule
  #' @param f: Function to integrate (must support vectorized input)
  #' @param a: Lower limit of integration
  #' @param b: Upper limit of integration
  #' @param n: Number of subintervals (must be even)
  #' @return: Approximation of the integral
  
  if (n %% 2 != 0) {
    stop("Error: Number of subintervals n must be even.")
  }
  
  h <- (b - a) / n
  x <- seq(a, b, by = h)
  y <- f(x)
  
  # Create coefficient vector: [1, 4, 2, 4, 2, ..., 2, 4, 1]
  coefficients <- rep(2, n + 1)
  coefficients[1] <- 1
  coefficients[n + 1] <- 1
  coefficients[seq(2, n, by = 2)] <- 4
  
  result <- sum(coefficients * y) * h / 3
  return(result)
}

# Helper function to print integration results
print_integration_result <- function(f, a, b, n, result, exact = NULL) {
  #' Print formatted integration results
  #' @param f: Function that was integrated
  #' @param a: Lower limit
  #' @param b: Upper limit
  #' @param n: Number of subintervals
  #' @param result: Computed integral value
  #' @param exact: Exact value if known (optional)
  
  cat("Simpson's Rule Integration:\n")
  cat(sprintf("  Interval: [%.4f, %.4f]\n", a, b))
  cat(sprintf("  Subintervals: %d\n", n))
  cat(sprintf("  Approximate integral: %.10f\n", result))
  
  if (!is.null(exact)) {
    error <- abs(result - exact)
    rel_error <- error / abs(exact) * 100
    cat(sprintf("  Exact value: %.10f\n", exact))
    cat(sprintf("  Absolute error: %.2e\n", error))
    cat(sprintf("  Relative error: %.4f%%\n", rel_error))
  }
  cat("\n")
}

# ========== Example Usage ==========

cat("========== Example 1: Integral of sin(x) from 0 to π ==========\n\n")

# Define the function to integrate
func1 <- function(x) sin(x)

# Integration limits
a1 <- 0
b1 <- pi

# Number of subintervals (must be even)
n1 <- 10

# Calculate integral
integral1 <- simpson_rule(func1, a1, b1, n1)
exact1 <- 2.0  # Exact value of ∫sin(x)dx from 0 to π is 2

print_integration_result(func1, a1, b1, n1, integral1, exact1)

# ========== Example 2: Integral of x² from 0 to 1 ==========

cat("========== Example 2: Integral of x² from 0 to 1 ==========\n\n")

func2 <- function(x) x^2
a2 <- 0
b2 <- 1
n2 <- 20

integral2 <- simpson_rule(func2, a2, b2, n2)
exact2 <- 1/3  # Exact value is 1/3

print_integration_result(func2, a2, b2, n2, integral2, exact2)

# ========== Example 3: Integral of e^x from 0 to 1 ==========

cat("========== Example 3: Integral of e^x from 0 to 1 ==========\n\n")

func3 <- function(x) exp(x)
a3 <- 0
b3 <- 1
n3 <- 10

integral3 <- simpson_rule(func3, a3, b3, n3)
exact3 <- exp(1) - 1  # Exact value is e - 1

print_integration_result(func3, a3, b3, n3, integral3, exact3)

# ========== Example 4: Comparison with different n values ==========

cat("========== Example 4: Convergence study for sin(x) ==========\n\n")

n_values <- c(10, 20, 50, 100, 200)
cat("Convergence of Simpson's Rule with increasing subintervals:\n")
cat(sprintf("%-15s %-20s %-15s\n", "Subintervals", "Approximate", "Error"))
cat(strrep("-", 50), "\n")

for (n in n_values) {
  result <- simpson_rule(sin, 0, pi, n)
  error <- abs(result - 2.0)
  cat(sprintf("%-15d %-20.12f %.4e\n", n, result, error))
}
cat("\n")

# ========== Example 5: Using vectorized version ==========

cat("========== Example 5: Vectorized implementation ==========\n\n")

func5 <- function(x) 1 / (1 + x^2)  # Arctangent derivative
a5 <- 0
b5 <- 1
n5 <- 100

integral5 <- simpson_rule_vectorized(func5, a5, b5, n5)
exact5 <- pi / 4  # ∫[0,1] 1/(1+x²)dx = arctan(1) - arctan(0) = π/4

print_integration_result(func5, a5, b5, n5, integral5, exact5)

# ========== Example 6: Gaussian function ==========

cat("========== Example 6: Gaussian (Normal) distribution PDF ==========\n\n")

# Standard normal distribution PDF from -3 to 3
gaussian <- function(x) (1 / sqrt(2 * pi)) * exp(-x^2 / 2)
a6 <- -3
b6 <- 3
n6 <- 100

integral6 <- simpson_rule(gaussian, a6, b6, n6)
# Exact value is approximately 0.9973 (99.73% within 3 standard deviations)

cat(sprintf("Integral of standard normal PDF from %.1f to %.1f: %.6f\n", a6, b6, integral6))
cat(sprintf("This represents %.2f%% of the total area under the curve.\n\n", integral6 * 100))

# ========== Notes ==========
cat("========== Important Notes ==========\n")
cat("1. Simpson's Rule requires an EVEN number of subintervals.\n")
cat("2. It provides O(h⁴) accuracy (fourth-order accurate).\n")
cat("3. More accurate than Trapezoidal Rule for smooth functions.\n")
cat("4. Works best for continuous, smooth functions.\n")
cat("5. For higher accuracy, increase the number of subintervals.\n")
