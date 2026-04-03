# Gaussian Elimination Algorithm
#
# This algorithm is used to solve a system of linear equations of the form Ax = b. It works by
# converting the matrix into row echelon form using Gaussian elimination (with partial pivoting), 
# and then applying back substitution to compute the solution vector x.
#
# Inputs:
#
# A: An n × n matrix containing the coefficients of the equations
# b: A vector of length n representing the right-hand side values
#
# Output:
#
# x: A vector of length n that contains the solution to the system
#
# Dependencies: None (uses base R functions)
#
# Note: This implementation assumes the matrix A is invertible. For real world scenarios,
# consider adding checks for singularity and numerical stability.

gaussian_elimination <- function(A, b) {

  # To check if A is a square matrix
  if (!is.matrix(A) || nrow(A) != ncol(A)) {
    stop("A must be a square matrix")
  }

  n <- nrow(A)

  # To check if b is a vector of correct length
  if (!is.vector(b) || length(b) != n) {
    stop("b must be a vector of length equal to the number of rows in A")
  }

  # To create augmented matrix [A|b]
  Ab <- cbind(A, b)

  # Forward elimination with partial pivoting
  for (i in 1:(n-1)) {
    # Find pivot row
    pivot_row <- i
    for (j in (i+1):n) {
      if (abs(Ab[j, i]) > abs(Ab[pivot_row, i])) {
        pivot_row <- j
      }
    }

    # Swap rows if needed
    if (pivot_row != i) {
      temp <- Ab[i, ]
      Ab[i, ] <- Ab[pivot_row, ]
      Ab[pivot_row, ] <- temp
    }

    # looking for zero pivot
    if (abs(Ab[i, i]) < .Machine$double.eps) {
      stop("Matrix is singular or nearly singular")
    }

    # Eliminate below pivot
    for (j in (i+1):n) {
      factor <- Ab[j, i] / Ab[i, i]
      Ab[j, ] <- Ab[j, ] - factor * Ab[i, ]
    }
  }

  #  To check last pivot
  if (abs(Ab[n, n]) < .Machine$double.eps) {
    stop("Matrix is singular or nearly singular")
  }

  # Substitution Method
  x <- numeric(n)
  for (i in n:1) {
    if (i < n) {
      x[i] <- (Ab[i, n+1] - sum(Ab[i, (i+1):n] * x[(i+1):n])) / Ab[i, i]
    } else {
      x[i] <- Ab[i, n+1] / Ab[i, i]
    }
  }

  return(x)
}

# Example
A <- matrix(c(2, 3, -1,
              4, 4, -3,
              2, -3, 1), nrow = 3, byrow = TRUE)

b <- c(5, 3, -1)

solution <- gaussian_elimination(A, b)

cat("Solution:\n")
cat(sprintf("x = %.6f\n", solution[1]))
cat(sprintf("y = %.6f\n", solution[2]))
cat(sprintf("z = %.6f\n", solution[3]))

verification <- A %*% solution
cat("\nVerification (Ax should equal b):\n")
cat(sprintf("Ax = [%.6f, %.6f, %.6f]\n", verification[1], verification[2], verification[3]))
cat(sprintf("b  = [%.6f, %.6f, %.6f]\n", b[1], b[2], b[3]))