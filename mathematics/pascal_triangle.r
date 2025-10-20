# pascal_triangle.r
# Pascal's Triangle Generator
# Each number is the sum of two numbers directly above it
# Formula: C(n, k) = C(n-1, k-1) + C(n-1, k)

#' Generate Pascal's Triangle up to n rows
#' @param n Number of rows
#' @return List of rows, each containing the triangle values
pascal_triangle <- function(n) {
  if (n <= 0) {
    stop("Number of rows must be positive")
  }
  
  triangle <- list()
  
  for (i in 1:n) {
    row <- numeric(i)
    row[1] <- 1
    row[i] <- 1
    
    if (i > 2) {
      for (j in 2:(i-1)) {
        row[j] <- triangle[[i-1]][j-1] + triangle[[i-1]][j]
      }
    }
    
    triangle[[i]] <- row
  }
  
  return(triangle)
}

#' Generate single row of Pascal's Triangle
#' @param n Row number (0-indexed)
#' @return Vector containing the nth row
pascal_row <- function(n) {
  if (n < 0) {
    stop("Row number must be non-negative")
  }
  
  row <- numeric(n + 1)
  row[1] <- 1
  
  for (i in 1:n) {
    row[i + 1] <- row[i] * (n - i + 1) / i
  }
  
  return(row)
}

#' Get specific element from Pascal's Triangle
#' @param n Row number (0-indexed)
#' @param k Column number (0-indexed)
#' @return Value at position (n, k) - binomial coefficient C(n, k)
pascal_element <- function(n, k) {
  if (n < 0 || k < 0 || k > n) {
    return(0)
  }
  
  # Use symmetry: C(n, k) = C(n, n-k)
  if (k > n - k) {
    k <- n - k
  }
  
  result <- 1
  for (i in 1:k) {
    result <- result * (n - i + 1) / i
  }
  
  return(result)
}

#' Print Pascal's Triangle in formatted style
#' @param n Number of rows
#' @param centered Whether to center the triangle (default: TRUE)
print_pascal_triangle <- function(n, centered = TRUE) {
  triangle <- pascal_triangle(n)
  
  if (centered) {
    # Calculate maximum width for centering
    max_row <- triangle[[n]]
    max_width <- sum(nchar(as.character(max_row))) + length(max_row) - 1
    
    for (i in 1:n) {
      row <- triangle[[i]]
      row_str <- paste(row, collapse = " ")
      row_width <- nchar(row_str)
      padding <- (max_width - row_width) / 2
      
      cat(rep(" ", floor(padding)), row_str, "\n", sep = "")
    }
  } else {
    for (i in 1:n) {
      cat(paste(triangle[[i]], collapse = " "), "\n")
    }
  }
}

#' Get diagonal sums from Pascal's Triangle
#' @param n Number of rows
#' @return Vector of diagonal sums (Fibonacci sequence)
pascal_diagonal_sums <- function(n) {
  triangle <- pascal_triangle(n)
  sums <- numeric(n)
  
  for (i in 1:n) {
    diagonal_sum <- 0
    row <- i
    col <- 1
    
    while (row >= 1 && col <= length(triangle[[row]])) {
      diagonal_sum <- diagonal_sum + triangle[[row]][col]
      row <- row - 1
      col <- col + 1
    }
    
    sums[i] <- diagonal_sum
  }
  
  return(sums)
}

#' Get row sum of Pascal's Triangle
#' @param n Row number (0-indexed)
#' @return Sum of row n (equals 2^n)
pascal_row_sum <- function(n) {
  return(2^n)
}

#' Get all row sums up to n rows
#' @param n Number of rows
#' @return Vector of row sums
pascal_all_row_sums <- function(n) {
  if (n <= 0) {
    return(numeric(0))
  }
  return(2^(0:(n-1)))
}

#' Generate Pascal's Triangle as matrix (padded with zeros)
#' @param n Number of rows
#' @return Matrix representation of Pascal's Triangle
pascal_triangle_matrix <- function(n) {
  triangle <- pascal_triangle(n)
  mat <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in 1:length(triangle[[i]])) {
      mat[i, j] <- triangle[[i]][j]
    }
  }
  
  return(mat)
}

#' Get odd numbers pattern in Pascal's Triangle (Sierpinski pattern)
#' @param n Number of rows
#' @return Matrix with 1 for odd, 0 for even
pascal_odd_pattern <- function(n) {
  triangle <- pascal_triangle(n)
  mat <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in 1:length(triangle[[i]])) {
      mat[i, j] <- triangle[[i]][j] %% 2
    }
  }
  
  return(mat)
}

# Examples
if (FALSE) {
  # Example 1: Generate and print Pascal's Triangle
  cat("Example 1: Pascal's Triangle (10 rows)\n")
  cat("======================================\n\n")
  print_pascal_triangle(10)
  
  # Example 2: Generate specific row
  cat("\nExample 2: Row 7 of Pascal's Triangle\n")
  cat("======================================\n")
  row7 <- pascal_row(7)
  cat(paste(row7, collapse = " "), "\n")
  cat("These are binomial coefficients: C(7,0), C(7,1), ..., C(7,7)\n")
  
  # Example 3: Get specific element
  cat("\nExample 3: Specific Elements\n")
  cat("============================\n")
  cat(sprintf("C(10, 3) = %d\n", pascal_element(10, 3)))
  cat(sprintf("C(8, 4) = %d\n", pascal_element(8, 4)))
  cat(sprintf("C(6, 2) = %d\n", pascal_element(6, 2)))
  
  # Example 4: Row sums (powers of 2)
  cat("\nExample 4: Row Sums\n")
  cat("===================\n")
  sums <- pascal_all_row_sums(8)
  for (i in 1:8) {
    cat(sprintf("Row %d sum: %d = 2^%d\n", i-1, sums[i], i-1))
  }
  
  # Example 5: Diagonal sums (Fibonacci sequence)
  cat("\nExample 5: Diagonal Sums (Fibonacci Sequence)\n")
  cat("=============================================\n")
  diag_sums <- pascal_diagonal_sums(12)
  cat("Diagonal sums:", paste(diag_sums, collapse = ", "), "\n")
  cat("This is the Fibonacci sequence!\n")
  
  # Example 6: Pascal's Triangle as matrix
  cat("\nExample 6: Pascal's Triangle as Matrix\n")
  cat("======================================\n")
  mat <- pascal_triangle_matrix(8)
  print(mat)
  
  # Example 7: Odd/Even pattern (Sierpinski Triangle)
  cat("\nExample 7: Odd Numbers Pattern (Sierpinski Triangle)\n")
  cat("===================================================\n")
  odd_pattern <- pascal_odd_pattern(16)
  
  for (i in 1:16) {
    # Center the pattern
    padding <- 16 - i
    cat(rep(" ", padding))
    for (j in 1:i) {
      if (odd_pattern[i, j] == 1) {
        cat("* ")
      } else {
        cat("  ")
      }
    }
    cat("\n")
  }
  
  # Example 8: Properties of Pascal's Triangle
  cat("\nExample 8: Properties of Pascal's Triangle\n")
  cat("==========================================\n")
  
  n <- 10
  triangle <- pascal_triangle(n)
  
  cat("\n1. Symmetry Property:\n")
  cat("Row 6:", paste(triangle[[7]], collapse = " "), "\n")
  cat("(Each row is symmetric)\n")
  
  cat("\n2. Sum of row n = 2^n:\n")
  for (i in 1:5) {
    row_sum <- sum(triangle[[i]])
    cat(sprintf("Row %d: sum = %d = 2^%d\n", i-1, row_sum, i-1))
  }
  
  cat("\n3. Binomial Theorem:\n")
  cat("(a + b)^4 = ")
  row4 <- triangle[[5]]
  terms <- character(5)
  for (i in 1:5) {
    power_a <- 4 - (i - 1)
    power_b <- i - 1
    coef <- row4[i]
    
    if (power_a == 0) {
      terms[i] <- sprintf("%db^%d", coef, power_b)
    } else if (power_b == 0) {
      terms[i] <- sprintf("%da^%d", coef, power_a)
    } else if (power_a == 1 && power_b == 1) {
      terms[i] <- sprintf("%dab", coef)
    } else if (power_a == 1) {
      terms[i] <- sprintf("%dab^%d", coef, power_b)
    } else if (power_b == 1) {
      terms[i] <- sprintf("%da^%db", coef, power_a)
    } else {
      terms[i] <- sprintf("%da^%db^%d", coef, power_a, power_b)
    }
  }
  cat(paste(terms, collapse = " + "), "\n")
  
  cat("\n4. Hockey Stick Pattern:\n")
  cat("Sum of diagonal: 1 + 4 + 10 + 20 = 35 = C(7,3)\n")
  cat("Formula: C(n,r) + C(n+1,r) + ... + C(n+k,r) = C(n+k+1,r+1)\n")
  
  cat("\n5. Catalan Numbers (central elements):\n")
  cat("Row 2: C(2,1) = 2\n")
  cat("Row 4: C(4,2) = 6\n")
  cat("Row 6: C(6,3) = 20\n")
  cat("Row 8: C(8,4) = 70\n")
  
  # Example 9: Application - Probability
  cat("\nExample 9: Application - Coin Flip Probability\n")
  cat("===============================================\n")
  cat("Flipping 5 coins, probability of exactly k heads:\n\n")
  row5 <- pascal_row(5)
  total <- sum(row5)
  
  for (k in 0:5) {
    prob <- row5[k + 1] / total
    cat(sprintf("%d heads: %d/%d = %.4f (%.2f%%)\n", 
                k, row5[k + 1], total, prob, prob * 100))
  }
  
  # Example 10: Large row computation
  cat("\nExample 10: Large Row Computation\n")
  cat("==================================\n")
  cat("Row 50 (too large to display fully):\n")
  row50 <- pascal_row(50)
  cat("Number of elements:", length(row50), "\n")
  cat("First 5 elements:", paste(row50[1:5], collapse = ", "), "\n")
  cat("Middle element C(50,25):", format(row50[26], scientific = FALSE), "\n")
  cat("Last 5 elements:", paste(row50[46:51], collapse = ", "), "\n")
  
  # Example 11: Verification with binomial coefficient
  cat("\nExample 11: Verification\n")
  cat("========================\n")
  n <- 10
  k <- 4
  element <- pascal_element(n, k)
  factorial_calc <- factorial(n) / (factorial(k) * factorial(n - k))
  cat(sprintf("C(%d,%d) using Pascal's Triangle: %d\n", n, k, element))
  cat(sprintf("C(%d,%d) using factorials: %d\n", n, k, factorial_calc))
  cat(sprintf("Match: %s\n", ifelse(element == factorial_calc, "✓", "✗")))
}
