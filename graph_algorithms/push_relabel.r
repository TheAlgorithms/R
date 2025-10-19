# Push-Relabel (Preflow-Push) Maximum Flow Algorithm
#
# This implementation follows the push-relabel method for computing maximum
# flow in a directed graph represented by a capacity matrix. It is translated
# from a Python class-based implementation into an R functional style while
# keeping the repository's conventions (clear header, example usage, 1-based indexing).
#
# Time Complexity: O(V^3) in the generic implementation; practical performance
# depends on heuristics (gap relabeling, global relabeling) which are not included here.
# Space Complexity: O(V^2) for flow and capacity matrices

push_relabel <- function(capacity, source, sink) {
  cap <- as.matrix(capacity)
  n <- nrow(cap)
  if (ncol(cap) != n) stop("capacity must be a square matrix")
  if (source < 1 || source > n || sink < 1 || sink > n) stop("source/sink out of range")

  flow <- matrix(0, n, n)
  excess <- rep(0, n)
  height <- rep(0L, n)

  # Push operation
  push <- function(u, v) {
    send <- min(excess[u], cap[u, v] - flow[u, v])
    if (send <= 0) return(FALSE)
    flow[u, v] <<- flow[u, v] + send
    flow[v, u] <<- flow[v, u] - send
    excess[u] <<- excess[u] - send
    excess[v] <<- excess[v] + send
    TRUE
  }

  # Relabel operation
  relabel <- function(u) {
    min_height <- Inf
    for (v in seq_len(n)) {
      if (cap[u, v] - flow[u, v] > 0) {
        min_height <- min(min_height, height[v])
      }
    }
    if (is.finite(min_height)) {
      height[u] <<- as.integer(min_height + 1)
    }
  }

  # Discharge operation
  discharge <- function(u) {
    while (excess[u] > 0) {
      pushed <- FALSE
      for (v in seq_len(n)) {
        if (cap[u, v] - flow[u, v] > 0 && height[u] == height[v] + 1) {
          push(u, v)
          pushed <- TRUE
          if (excess[u] <= 0) break
        }
      }
      if (!pushed) relabel(u)
    }
  }

  # Initialization: preflow from source
  height[source] <- as.integer(n)
  for (v in seq_len(n)) {
    if (cap[source, v] > 0) {
      flow[source, v] <- cap[source, v]
      flow[v, source] <- -cap[source, v]
      excess[v] <- cap[source, v]
      excess[source] <- excess[source] - cap[source, v]
    }
  }

  # List of active vertices (exclude source and sink)
  vertices <- setdiff(seq_len(n), c(source, sink))
  p <- 1L
  while (p <= length(vertices)) {
    u <- vertices[p]
    old_height <- height[u]
    discharge(u)
    if (height[u] > old_height) {
      # move to front
      vertices <- c(u, vertices[-p])
      p <- 1L
    } else {
      p <- p + 1L
    }
  }

  maxflow <- sum(flow[source, ])
  invisible(list(max_flow = maxflow, flow = flow, excess = excess, height = height))
}

# Example usage
cat("=== Push-Relabel Maximum Flow (R) ===\n")
graph <- matrix(c(
  0, 16, 13, 0, 0, 0,
  0, 0, 10, 12, 0, 0,
  0, 4, 0, 0, 14, 0,
  0, 0, 9, 0, 0, 20,
  0, 0, 0, 7, 0, 4,
  0, 0, 0, 0, 0, 0
), nrow = 6, byrow = TRUE)

source <- 1L
sink <- 6L
res <- push_relabel(graph, source, sink)
cat("Maximum Flow:", res$max_flow, "\n")
