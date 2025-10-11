```r
# Bellman-Ford Shortest Path Algorithm
#
# The Bellman-Ford algorithm computes shortest paths from a single source vertex to
# all other vertices in a weighted graph. Unlike Dijkstra's algorithm, Bellman-Ford
# supports graphs with negative edge weights and can detect negative-weight cycles.
#
# Time Complexity: O(V * E)
# Space Complexity: O(V)
#
# Input: graph as an adjacency list where each entry is a list of edges with fields
#        `vertex` and `weight`, and `source` vertex index (integer)
# Output: A list containing distances, predecessors, and a flag indicating whether
#         a negative cycle was detected

bellman_ford_shortest_path <- function(graph, source) {
  # Collect all vertices (numeric indices expected)
  all_vertices <- unique(c(names(graph), unlist(lapply(graph, function(x) sapply(x, function(e) e$vertex)))))
  # Convert to numeric vector
  all_vertices <- as.numeric(all_vertices)
  num_vertices <- max(all_vertices)

  # Initialize distances and predecessors
  distances <- rep(Inf, num_vertices)
  predecessor <- rep(-1, num_vertices)

  distances[source] <- 0

  # Relax edges repeatedly (V-1 times)
  for (i in 1:(num_vertices - 1)) {
    updated <- FALSE
    # Iterate all edges
    for (u_char in names(graph)) {
      u <- as.numeric(u_char)
      for (edge in graph[[u_char]]) {
        v <- edge$vertex
        w <- edge$weight
        if (distances[u] != Inf && distances[u] + w < distances[v]) {
          distances[v] <- distances[u] + w
          predecessor[v] <- u
          updated <- TRUE
        }
      }
    }
    # If no update in this pass, we can stop early
    if (!updated) break
  }

  # Check for negative-weight cycles: if we can still relax, there is a negative cycle
  negative_cycle <- FALSE
  for (u_char in names(graph)) {
    u <- as.numeric(u_char)
    for (edge in graph[[u_char]]) {
      v <- edge$vertex
      w <- edge$weight
      if (distances[u] != Inf && distances[u] + w < distances[v]) {
        negative_cycle <- TRUE
        break
      }
    }
    if (negative_cycle) break
  }

  return(list(
    distances = distances,
    predecessor = predecessor,
    negative_cycle = negative_cycle
  ))
}

# Helper to reconstruct the shortest path from source to target
get_bellman_ford_path <- function(result, source, target) {
  if (result$negative_cycle) {
    return(list(path = NULL, distance = NA, message = "Negative-weight cycle detected; shortest path undefined"))
  }

  distances <- result$distances
  predecessor <- result$predecessor

  if (is.infinite(distances[target])) {
    return(list(path = NULL, distance = Inf, message = "Target not reachable from source"))
  }

  path <- c()
  current <- target
  while (current != -1 && current != 0) {
    path <- c(current, path)
    current <- predecessor[current]
  }

  return(list(path = path, distance = distances[target]))
}

# Example usage and tests
cat("=== Bellman-Ford Shortest Path Algorithm ===\n")

# Example graph with negative edges but no negative cycle
# Graph structure:
# 1 -> 2 (6), 1 -> 3 (5), 1 -> 4 (5)
# 2 -> 5 (-1)
# 3 -> 2 (-2), 3 -> 5 (1)
# 4 -> 3 (-2), 4 -> 6 (-1)
# 5 -> 6 (3)
# 6 -> (none)
bf_graph <- list(
  "1" = list(list(vertex = 2, weight = 6), list(vertex = 3, weight = 5), list(vertex = 4, weight = 5)),
  "2" = list(list(vertex = 5, weight = -1)),
  "3" = list(list(vertex = 2, weight = -2), list(vertex = 5, weight = 1)),
  "4" = list(list(vertex = 3, weight = -2), list(vertex = 6, weight = -1)),
  "5" = list(list(vertex = 6, weight = 3)),
  "6" = list()
)

cat("Graph (adjacency list):\n")
for (v in names(bf_graph)) {
  edges <- bf_graph[[v]]
  if (length(edges) > 0) {
    edge_strs <- sapply(edges, function(e) paste0(e$vertex, "(", e$weight, ")"))
    cat("Vertex", v, "-> [", paste(edge_strs, collapse = ", "), "]\n")
  } else {
    cat("Vertex", v, "-> []\n")
  }
}

cat("\nRunning Bellman-Ford from vertex 1:\n")
bf_result <- bellman_ford_shortest_path(bf_graph, 1)
cat("Negative cycle detected:", bf_result$negative_cycle, "\n")

cat("Distances from vertex 1:\n")
for (i in 1:length(bf_result$distances)) {
  d <- bf_result$distances[i]
  if (is.infinite(d)) {
    cat("To vertex", i, ": unreachable\n")
  } else {
    cat("To vertex", i, ": distance =", d, "\n")
  }
}

cat("\nShortest path from 1 to 6:\n")
path_info <- get_bellman_ford_path(bf_result, 1, 6)
if (!is.null(path_info$path)) {
  cat("Path:", paste(path_info$path, collapse = " -> "), "\n")
  cat("Distance:", path_info$distance, "\n")
} else {
  cat(path_info$message, "\n")
}

``` 
