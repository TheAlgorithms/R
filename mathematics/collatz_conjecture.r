# Collatz Conjecture
#
# Start with any positive integer n and repeat:
# - if n is even, divide it by 2
# - if n is odd, replace it with 3 * n + 1
# The conjecture states that every positive integer eventually reaches 1.
# It is unproven, but it holds for every number tested so far.
#
# Time Complexity: O(s) where s = number of steps to reach 1
# Space Complexity: O(s) to store the sequence, O(1) to only count steps
#
# Applications:
# - Classic example of an unsolved problem in number theory
# - Practice for loops and sequence generation
# - Testing integer arithmetic and overflow handling
#
# Example usage:
# collatz_sequence(6)   # 6 3 10 5 16 8 4 2 1
# collatz_steps(27)     # 111

check_collatz_input <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || !is.finite(n) || n < 1 ||
      n != floor(n)) {
    stop("Error: n must be a positive integer")
  }
  if (n > 2^53) {
    stop("Error: n exceeds the exact-integer range")
  }
}

# Apply one Collatz step. Doubles hold integers exactly only up to 2^53, so
# stop before 3 * n + 1 would pass that limit instead of returning a rounded
# (wrong) value.
collatz_next <- function(n) {
  if (n %% 2 == 0) {
    return(n / 2)
  }
  if (n > (2^53 - 1) / 3) {
    stop("Error: Collatz transition exceeds the exact-integer range")
  }
  3 * n + 1
}

#' Generate the Collatz sequence starting at n
#' @param n Positive integer
#' @return Numeric vector from n down to 1
collatz_sequence <- function(n) {
  check_collatz_input(n)
  # Grow the buffer geometrically and trim once at the end: amortized O(s)
  sequence <- numeric(16)
  sequence[1] <- n
  sequence_length <- 1
  while (n != 1) {
    n <- collatz_next(n)
    sequence_length <- sequence_length + 1
    if (sequence_length > length(sequence)) {
      length(sequence) <- 2 * length(sequence)
    }
    sequence[sequence_length] <- n
  }
  sequence[seq_len(sequence_length)]
}

#' Count the steps needed to reach 1
#' @param n Positive integer
#' @return Number of steps (0 for n = 1)
collatz_steps <- function(n) {
  check_collatz_input(n)
  steps <- 0
  while (n != 1) {
    n <- collatz_next(n)
    steps <- steps + 1
  }
  steps
}

# ===========================
# Example Usage & Testing
# ===========================
stopifnot(identical(collatz_sequence(1), 1))
stopifnot(identical(collatz_sequence(6), c(6, 3, 10, 5, 16, 8, 4, 2, 1)))
stopifnot(collatz_steps(1) == 0)
stopifnot(collatz_steps(6) == 8)
stopifnot(collatz_steps(27) == 111)
stopifnot(length(collatz_sequence(27)) == collatz_steps(27) + 1)

# Invalid input raises an error
stopifnot(inherits(try(collatz_steps(0), silent = TRUE), "try-error"))
stopifnot(inherits(try(collatz_steps(2.5), silent = TRUE), "try-error"))

cat("Collatz sequence of 6:", paste(collatz_sequence(6), collapse = " "), "\n")
cat("Steps for 27:", collatz_steps(27), "\n")
cat("All Collatz tests passed\n")
