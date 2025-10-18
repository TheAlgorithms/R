# Traveling Salesman Problem (TSP) using Dynamic Programming + Bitmasking
# ------------------------------------------------------------
# Given a cost matrix, find the minimum cost to visit all cities exactly once
# and return to the starting city.

INF <- 1e9  # a large number representing infinity

# TSP function
tsp <- function(mask, pos, cost, dp, n) {
  # Base case: all cities visited, return to starting city
  if (mask == (1 << n) - 1) {
    return(cost[pos + 1, 1])  # return to city 0
  }
  
  # Return already computed result
  if (dp[mask + 1, pos + 1] != -1) {
    return(dp[mask + 1, pos + 1])
  }
  
  ans <- INF
  
  # Try visiting all unvisited cities
  for (city in 0:(n - 1)) {
    if ((mask & (1 << city)) == 0) {
      newAns <- cost[pos + 1, city + 1] + tsp(mask | (1 << city), city, cost, dp, n)
      ans <- min(ans, newAns)
    }
  }
  
  dp[mask + 1, pos + 1] <<- ans
  return(ans)
}

# Example usage
main <- function() {
  # Cost matrix: symmetric distances between cities
  cost <- matrix(
    c(0, 20, 42, 35,
      20, 0, 30, 34,
      42, 30, 0, 12,
      35, 34, 12, 0),
    nrow = 4, byrow = TRUE
  )
  
  n <- nrow(cost)
  dp <- matrix(-1, nrow = (1 << n), ncol = n)  # DP table
  
  # Start from city 0, with only city 0 visited (mask = 1)
  result <- tsp(1, 0, cost, dp, n)
  cat("Minimum Traveling Cost:", result, "\n")
}

# Run example
main()
