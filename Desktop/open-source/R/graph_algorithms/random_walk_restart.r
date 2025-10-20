# Random Walk with Restart (RWR) Algorithm
#
# Computes node relevance scores using a random walk on a graph with a probability of restarting
# at the source node in each step. Commonly used for link prediction, recommendation, and ranking.

random_walk_restart <- function(adj_matrix, restart_prob = 0.3, max_iter = 100, tol = 1e-6, source) {
  n <- nrow(adj_matrix)
  # Normalize adjacency matrix to column stochastic matrix
  col_sums <- colSums(adj_matrix)
  P <- adj_matrix
  for (j in 1:n) {
    if (col_sums[j] != 0) P[, j] <- P[, j] / col_sums[j]
  }
  
  # Initialize probability vector
  r <- rep(0, n)
  r[source] <- 1
  r_old <- r
  
  for (i in 1:max_iter) {
    r_new <- (1 - restart_prob) * P %*% r_old + restart_prob * r
    if (sum(abs(r_new - r_old)) < tol) break
    r_old <- r_new
  }
  
  return(as.numeric(r_new))
}

# Example usage
cat("=== Random Walk with Restart (RWR) Algorithm ===\n")

# Example adjacency matrix (directed or undirected graph)
adj_matrix <- matrix(c(
  0, 1, 1, 0,
  1, 0, 1, 1,
  1, 1, 0, 1,
  0, 1, 1, 0
), nrow = 4, byrow = TRUE)

cat("Adjacency matrix:\n")
print(adj_matrix)

cat("\nRandom Walk with Restart from node 1:\n")
scores <- random_walk_restart(adj_matrix, restart_prob = 0.3, source = 1)
for (i in 1:length(scores)) {
  cat("Node", i, ": score =", round(scores[i], 4), "\n")
}
