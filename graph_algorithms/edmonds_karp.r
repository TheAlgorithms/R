# Edmonds-Karp Maximum Flow (Ford-Fulkerson with BFS)
#
# This R implementation follows the repository style: clear header, example
# usage, and 1-based vertex indexing. The algorithm computes maximum flow
# in a directed graph given by a capacity matrix. It returns the max flow and
# the residual capacity matrix.
#
# Time Complexity: O(V * E^2) in the naive implementation; Edmonds-Karp is O(V * E^2)
# Space Complexity: O(V^2) for the capacity/residual matrix

# BFS to find an augmenting path. Returns list(found, parent)
bfs_ek <- function(capacity, source, sink) {
  n <- nrow(capacity)
  visited <- rep(FALSE, n)
  parent <- rep(-1L, n)

  # Simple queue implementation using preallocated vector
  queue <- integer(n)
  front <- 1L
  rear <- 1L
  queue[rear] <- source
  visited[source] <- TRUE

  while (front <= rear) {
    u <- queue[front]
    front <- front + 1L
    for (v in seq_len(n)) {
      if (!visited[v] && capacity[u, v] > 0) {
        rear <- rear + 1L
        queue[rear] <- v
        visited[v] <- TRUE
        parent[v] <- u
        if (v == sink) {
          return(list(found = TRUE, parent = parent))
        }
      }
    }
  }
  list(found = FALSE, parent = parent)
}

# Edmonds-Karp main function
edmonds_karp <- function(capacity, source, sink) {
  # Ensure capacity is a numeric matrix and uses 1-based indexing for vertices
  cap <- as.matrix(capacity)
  n <- nrow(cap)
  if (source < 1 || source > n || sink < 1 || sink > n) stop("source/sink out of range")

  max_flow <- 0

  repeat {
    res <- bfs_ek(cap, source, sink)
    if (!res$found) break
    parent <- res$parent

    # Find minimum residual capacity along the path
    path_flow <- Inf
    s <- sink
    while (s != source) {
      u <- parent[s]
      path_flow <- min(path_flow, cap[u, s])
      s <- u
    }

    # Update residual capacities along the path
    v <- sink
    while (v != source) {
      u <- parent[v]
      cap[u, v] <- cap[u, v] - path_flow
      cap[v, u] <- cap[v, u] + path_flow
      v <- parent[v]
    }

    max_flow <- max_flow + path_flow
  }

  invisible(list(max_flow = max_flow, residual = cap))
}

# Example usage
cat("=== Edmonds-Karp Maximum Flow (R) ===\n")
# Graph represented as capacity matrix (6x6)
graph <- matrix(c(
  0, 16, 13, 0, 0, 0,
  0, 0, 10, 12, 0, 0,
  0, 4, 0, 0, 14, 0,
  0, 0, 9, 0, 0, 20,
  0, 0, 0, 7, 0, 4,
  0, 0, 0, 0, 0, 0
), nrow = 6, byrow = TRUE)

# Note: Python example used 0-based indices; this R example uses 1-based indices
source <- 1L
sink <- 6L
res <- edmonds_karp(graph, source, sink)
cat("Maximum Flow:", res$max_flow, "\n")
