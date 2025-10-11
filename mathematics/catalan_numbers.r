# Catalan Numbers Implementation in R
#
# Catalan numbers form a sequence of natural numbers that occur in various counting problems.
# The nth Catalan number is given by the formula: C(n) = (2n)! / ((n+1)! * n!)
# They appear in problems like counting binary trees, valid parentheses combinations,
# paths in a grid, and polygon triangulations.
#
# Time Complexity: O(n) for iterative approach, O(n^2) for recursive with memoization
# Space Complexity: O(n) for memoization table

# Function to calculate nth Catalan number using dynamic programming
# @param n: Non-negative integer for which to calculate Catalan number
# @return: The nth Catalan number
catalan_dp <- function(n) {
    # Base cases
    if (n <= 1) {
        return(1)
    }
    
    # Initialize dp table
    catalan <- numeric(n + 1)
    catalan[1] <- 1  # C(0) = 1
    catalan[2] <- 1  # C(1) = 1
    
    # Fill the table using the recurrence relation:
    # C(n) = sum(C(i) * C(n-1-i)) for i from 0 to n-1
    for (i in 2:n) {
        catalan[i + 1] <- 0
        for (j in 0:(i - 1)) {
            catalan[i + 1] <- catalan[i + 1] + catalan[j + 1] * catalan[i - j]
        }
    }
    
    return(catalan[n + 1])
}

# Function to calculate nth Catalan number using direct formula
# @param n: Non-negative integer for which to calculate Catalan number
# @return: The nth Catalan number
catalan_formula <- function(n) {
    if (n <= 1) {
        return(1)
    }
    
    # Use the formula: C(n) = (2n)! / ((n+1)! * n!)
    # Simplified to: C(n) = (2n choose n) / (n+1)
    result <- 1
    
    # Calculate using the iterative formula to avoid large factorials
    for (i in 0:(n - 1)) {
        result <- result * (n + i + 1) / (i + 1)
    }
    
    return(result / (n + 1))
}

# Function to generate first n Catalan numbers
# @param n: Number of Catalan numbers to generate
# @return: Vector containing first n Catalan numbers
first_n_catalan <- function(n) {
    if (n <= 0) {
        return(numeric(0))
    }
    
    result <- numeric(n)
    
    for (i in 1:n) {
        result[i] <- catalan_dp(i - 1)  # Generate C(0) to C(n-1)
    }
    
    return(result)
}

# Function to find applications of Catalan numbers
# @param n: The index for which to show applications
# @return: List of interpretations of the nth Catalan number
catalan_applications <- function(n) {
    cat_n <- catalan_dp(n)
    
    applications <- list(
        value = cat_n,
        interpretations = c(
            paste("Number of ways to arrange", n, "pairs of parentheses"),
            paste("Number of full binary trees with", n + 1, "leaves"),
            paste("Number of ways to triangulate a convex polygon with", n + 2, "vertices"),
            paste("Number of monotonic lattice paths from (0,0) to (n,n) not crossing y=x"),
            paste("Number of ways to arrange", n, "non-attacking rooks on a triangular board")
        )
    )
    
    return(applications)
}

# Example usage:
# # Calculate specific Catalan numbers
# print(paste("5th Catalan number (DP):", catalan_dp(5)))
# print(paste("5th Catalan number (Formula):", catalan_formula(5)))
# 
# # Generate first 10 Catalan numbers
# first_10 <- first_n_catalan(10)
# print("First 10 Catalan numbers:")
# print(first_10)
# 
# # Show applications of 4th Catalan number
# apps <- catalan_applications(4)
# print(paste("C(4) =", apps$value))
# print("Applications:")
# for (app in apps$interpretations) {
#     print(app)
# }