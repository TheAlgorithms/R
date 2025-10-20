# Sieve of Eratosthenes Algorithm
#
# The Sieve of Eratosthenes is an ancient algorithm for finding all prime numbers
# up to a given limit. It works by iteratively marking the multiples of each prime
# starting from 2, and the unmarked numbers that remain are primes.
#
# Time Complexity: O(n log log n)
# Space Complexity: O(n)
#
# Input: A positive integer n (the upper limit)
# Output: A vector of all prime numbers from 2 to n

sieve_of_eratosthenes <- function(n) {
  # Handle edge cases
  if (n < 2) {
    return(integer(0))  # No primes less than 2
  }
  
  # Create a boolean array "prime[0..n]" and initialize all entries as TRUE
  prime <- rep(TRUE, n + 1)
  prime[1] <- FALSE  # 1 is not a prime number
  
  p <- 2
  while (p * p <= n) {
    # If prime[p] is not changed, then it is a prime
    if (prime[p]) {
      # Update all multiples of p starting from p^2
      for (i in seq(p * p, n, by = p)) {
        prime[i] <- FALSE
      }
    }
    p <- p + 1
  }
  
  # Collect all prime numbers
  primes <- which(prime)[-1]  # Remove index 1 (since 1 is not prime)
  return(primes)
}

# Optimized version that only checks odd numbers after 2
sieve_of_eratosthenes_optimized <- function(n) {
  # Handle edge cases
  if (n < 2) {
    return(integer(0))
  }
  if (n == 2) {
    return(2)
  }
  
  # Start with 2 (the only even prime)
  primes <- c(2)
  
  # Create boolean array for odd numbers only (3, 5, 7, ...)
  # Index i represents number (2*i + 3)
  size <- (n - 1) %/% 2
  is_prime <- rep(TRUE, size)
  
  # Sieve process for odd numbers
  for (i in 1:size) {
    if (is_prime[i]) {
      num <- 2 * i + 1  # Convert index to actual odd number
      
      # Mark multiples of num starting from num^2
      if (num * num <= n) {
        start_idx <- (num * num - 1) %/% 2  # Convert num^2 to index
        for (j in seq(start_idx, size, by = num)) {
          if (j <= size) {
            is_prime[j] <- FALSE
          }
        }
      }
    }
  }
  
  # Collect odd primes
  odd_primes <- 2 * which(is_prime) + 1
  primes <- c(primes, odd_primes)
  
  return(primes)
}

# Function to count primes up to n (useful for large n)
count_primes_sieve <- function(n) {
  if (n < 2) {
    return(0)
  }
  
  prime <- rep(TRUE, n + 1)
  prime[1] <- FALSE
  
  p <- 2
  while (p * p <= n) {
    if (prime[p]) {
      for (i in seq(p * p, n, by = p)) {
        prime[i] <- FALSE
      }
    }
    p <- p + 1
  }
  
  return(sum(prime))
}

# Function to check if a number is prime using trial division (for comparison)
is_prime_trial_division <- function(n) {
  if (n <= 1) return(FALSE)
  if (n <= 3) return(TRUE)
  if (n %% 2 == 0 || n %% 3 == 0) return(FALSE)
  
  i <- 5
  while (i * i <= n) {
    if (n %% i == 0 || n %% (i + 2) == 0) {
      return(FALSE)
    }
    i <- i + 6
  }
  return(TRUE)
}

# Segmented sieve for finding primes in a range [low, high]
segmented_sieve <- function(low, high) {
  # First, find all primes up to sqrt(high)
  limit <- floor(sqrt(high))
  primes <- sieve_of_eratosthenes(limit)
  
  # Create a boolean array for range [low, high]
  size <- high - low + 1
  is_prime <- rep(TRUE, size)
  
  # Mark multiples of each prime in the range
  for (prime in primes) {
    # Find the minimum number in [low, high] that is a multiple of prime
    start <- max(prime * prime, low + (prime - low %% prime) %% prime)
    
    # Mark multiples of prime in the range
    for (j in seq(start, high, by = prime)) {
      is_prime[j - low + 1] <- FALSE
    }
  }
  
  # Handle the case where low = 1 (1 is not prime)
  if (low == 1) {
    is_prime[1] <- FALSE
  }
  
  # Collect primes in the range
  range_primes <- (low:high)[is_prime]
  return(range_primes)
}

# Example usage and testing
cat("=== Sieve of Eratosthenes Algorithm ===\n")

# Test with small number
cat("Primes up to 30:\n")
primes_30 <- sieve_of_eratosthenes(30)
cat(paste(primes_30, collapse = ", "), "\n")
cat("Count:", length(primes_30), "\n\n")

# Test optimized version
cat("Optimized sieve - Primes up to 30:\n")
primes_30_opt <- sieve_of_eratosthenes_optimized(30)
cat(paste(primes_30_opt, collapse = ", "), "\n")
cat("Count:", length(primes_30_opt), "\n\n")

# Test with larger number
cat("Primes up to 100:\n")
primes_100 <- sieve_of_eratosthenes(100)
cat("Count:", length(primes_100), "\n")
cat("First 10 primes:", paste(primes_100[1:10], collapse = ", "), "\n")
cat("Last 10 primes:", paste(tail(primes_100, 10), collapse = ", "), "\n\n")

# Performance comparison for counting primes
cat("=== Performance Comparison ===\n")
n <- 1000

# Count using sieve
start_time <- Sys.time()
sieve_count <- count_primes_sieve(n)
sieve_time <- as.numeric(Sys.time() - start_time, units = "secs")

# Count using trial division
start_time <- Sys.time()
trial_count <- sum(sapply(2:n, is_prime_trial_division))
trial_time <- as.numeric(Sys.time() - start_time, units = "secs")

cat("Primes up to", n, ":\n")
cat("Sieve method:", sieve_count, "primes (", sprintf("%.4f", sieve_time), "seconds )\n")
cat("Trial division:", trial_count, "primes (", sprintf("%.4f", trial_time), "seconds )\n")
cat("Speedup:", sprintf("%.2f", trial_time / sieve_time), "x\n\n")

# Test segmented sieve
cat("=== Segmented Sieve Example ===\n")
cat("Primes between 50 and 100:\n")
range_primes <- segmented_sieve(50, 100)
cat(paste(range_primes, collapse = ", "), "\n")
cat("Count:", length(range_primes), "\n\n")

# Edge cases
cat("=== Edge Cases ===\n")
cat("Primes up to 1:", paste(sieve_of_eratosthenes(1), collapse = ", "), "\n")
cat("Primes up to 2:", paste(sieve_of_eratosthenes(2), collapse = ", "), "\n")
cat("Primes up to 3:", paste(sieve_of_eratosthenes(3), collapse = ", "), "\n")

# Large example (uncomment for larger tests)
# cat("\n=== Large Scale Test ===\n")
# large_n <- 10000
# start_time <- Sys.time()
# large_primes <- sieve_of_eratosthenes(large_n)
# end_time <- Sys.time()
# cat("Found", length(large_primes), "primes up to", large_n, "\n")
# cat("Computation time:", as.numeric(end_time - start_time, units = "secs"), "seconds\n")