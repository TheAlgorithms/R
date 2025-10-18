# ==============================================
# Stoer-Wagner Algorithm
# ==============================================
# Algorithm: Global minimum cut in undirected weighted graphs.
# Framework: R (base R)
#
# Purpose:
# - Find the minimum cut in an undirected weighted graph.
# - Identifies the smallest set of edges whose removal disconnects the graph.
#
# Core Idea:
# - Repeatedly merge vertices while keeping track of the minimum cut value.
# - Uses a greedy approach to find minimum cuts iteratively.
#
# Complexity:
# - Time:  O(V^3) for dense graphs (V = number of vertices)
# - Space: O(V^2) for adjacency/weight matrix
#
# Edge Cases / Notes:
# - Works for connected undirected weighted graphs.
# - Graph must be undirected; weights must be non-negative.
# - Returns the minimum cut weight and optionally the vertex partition.
#
# Typical Applications:
# - Network reliability analysis
# - Graph clustering and partitioning
# - Identifying critical edges in networks
#
# Reference:
# Stoer, M., & Wagner, F. (1997). A simple min-cut algorithm. Journal of the ACM.
# ==============================================

# Example Graph: Adjacency Matrix (Weighted Undirected Graph)
graph <- matrix(c(
  0, 3, 1, 3,
  3, 0, 2, 2,
  1, 2, 0, 4,
  3, 2, 4, 0
), nrow = 4, byrow = TRUE)

# Stoer-Wagner Minimum Cut Function
stoer_wagner <- function(graph) {
  n <- nrow(graph)
  vertices <- 1:n
  min_cut <- Inf
  
  while (length(vertices) > 1) {
    used <- rep(FALSE, n)
    weights <- rep(0, n)
    prev <- NULL
    
    for (i in 1:length(vertices)) {
      sel <- which.max(weights * (!used))
      used[sel] <- TRUE
      if (i == length(vertices)) {
        cut_weight <- weights[sel]
        if (cut_weight < min_cut) {
          min_cut <- cut_weight
        }
        # Merge last two vertices
        if (!is.null(prev)) {
          graph[prev, ] <- graph[prev, ] + graph[sel, ]
          graph[, prev] <- graph[prev, ]
          vertices <- vertices[vertices != sel]
        }
      }
      prev <- sel
      # Update weights
      weights <- weights + graph[sel, ]
    }
  }
  return(min_cut)
}

# Compute Minimum Cut
min_cut_value <- stoer_wagner(graph)
cat("Minimum cut weight of the graph:", min_cut_value, "\n")

# ==============================================
# Note:
# - This script defines the Stoer-Wagner minimum cut algorithm in R.
# - Works with small to medium weighted undirected graphs.
# - Can be extended to return the vertex partition corresponding to the cut.
# ==============================================
