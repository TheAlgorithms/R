# Decode Ways (Dynamic Programming)
#
# Given a string s containing only digits, return the number of ways to decode it where:
# '1' -> 'A', '2' -> 'B', ... '26' -> 'Z'.
# '0' cannot stand alone; valid pairs include 10, 20. Ranges 11..19 and 21..26 are valid two-digit decodings.
#
# Approaches included:
# 1) Tabulation DP: O(n) time, O(n) space
# 2) Forward O(1) space: track previous two states (like Fibonacci)
# 3) Backward O(1) space: iterate from end to start
#
# Complexity
# - Time: O(n)
# - Space: O(1) for forward/backward; O(n) for tabulation

# Helper: check if a two-character substring forms a valid 10..26 code
is_valid_pair <- function(a, b) {
  if (a == '1') return(TRUE)
  if (a == '2' && b <= '6') return(TRUE)
  FALSE
}

# 1) Tabulation DP
num_decodings_tabulation <- function(s) {
  if (length(s) == 0) return(0L)
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(chars)
  if (n == 0) return(0L)
  if (chars[1] == '0') return(0L)

  dp <- integer(n + 1L)
  dp[1L] <- 1L  # empty prefix
  dp[2L] <- 1L  # first char is non-zero by check above

  if (n == 1L) return(dp[2L])

  for (i in 2:n) {
    cur <- 0L
    # single char decode if not '0'
    if (chars[i] != '0') cur <- (cur + dp[i])
    # two-char decode if valid pair
    if (is_valid_pair(chars[i - 1L], chars[i])) cur <- (cur + dp[i - 1L])
    dp[i + 1L] <- cur
  }

  dp[n + 1L]
}

# 2) Forward O(1) space
num_decodings_forward <- function(s) {
  if (length(s) == 0) return(0L)
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(chars)
  if (n == 0) return(0L)
  if (chars[1] == '0') return(0L)

  prev <- 1L  # ways up to i-1
  curr <- 1L  # ways up to i

  for (i in 2:n) {
    next_val <- 0L
    if (chars[i] != '0') next_val <- (next_val + curr)
    if (is_valid_pair(chars[i - 1L], chars[i])) next_val <- (next_val + prev)
    prev <- curr
    curr <- next_val
  }

  curr
}

# 3) Backward O(1) space
num_decodings_backward <- function(s) {
  if (length(s) == 0) return(0L)
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(chars)
  if (n == 0) return(0L)

  p <- 1L  # ways for suffix starting at i+1
  pp <- 0L # ways for suffix starting at i+2
  res <- 0L

  for (i in n:1) {
    cur <- if (chars[i] == '0') 0L else p
    if (i < n && is_valid_pair(chars[i], chars[i + 1L])) cur <- cur + pp
    pp <- p
    p <- cur
    res <- cur
  }

  if (n == 0) 0L else res
}

# ------------------------------
# Examples / Demonstration
# ------------------------------
cat("=== Decode Ways (DP) ===\n\n")

examples <- c("12", "226", "06", "11106", "0", "2101", "10", "27")
for (ex in examples) {
  cat(sprintf("s = %-5s -> tab: %-4d  fwd: %-4d  bwd: %-4d\n",
              ex,
              num_decodings_tabulation(ex),
              num_decodings_forward(ex),
              num_decodings_backward(ex)))
}
cat("\n")
