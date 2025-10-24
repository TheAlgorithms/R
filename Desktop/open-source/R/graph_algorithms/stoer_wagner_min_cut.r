# Stoer-Wagner Minimum Cut Algorithm
#
# Computes the global minimum cut of an undirected weighted graph.
# The minimum cut is a partition of vertices into two sets minimizing
# the sum of weights of edges crossing the partition.

stoer_wagner_min_cut <- function(graph) {
  num_vertices <- max(as.numeric(names(graph)))
  vertices <- 1:num_vertices
  min_cut_value <- Inf
  best_cut <- NULL
  
  g <- graph  # copy of graph to modify
  
  while(length(vertices) > 1) {
    used <- rep(FALSE, num_vertices)
    weights <- rep(0, num_vertices)
    prev <- NULL
    
    for (i in 1:length(vertices)) {
      sel <- which.max(weights * (!used))
      used[sel] <- TRUE
      if (i == length(vertices)) {
        # last vertex added
        if (weights[sel] < min_cut_value) {
          min_cut_value <- weights[sel]
          best_cut <- c(sel)
        }
        # merge sel and prev
        if (!is.null(prev)) {
          for (v in 1:num_vertices) {
            g[[prev]][v] <- g[[prev]][v] + g[[sel]][v]
            g[[v]][prev] <- g[[prev]][v]
          }
          vertices <- vertices[vertices != sel]
        }
      } else {
        # update weights
        for (v in vertices) {
          if (!used[v]) weights[v] <- weights[v] + g[[sel]][v]
        }
        prev <- sel
      }
    }
  }
  
  return(list(min_cut_value = min_cut_value, cut_vertex = best_cut))
}

# Example usage
cat("=== Stoer-Wagner Minimum Cut Algorithm ===\n")

# Example undirected weighted graph as adjacency matrix
# 1-2(3), 1-3(1), 2-3(3), 2-4(4), 3-4(5)
graph_matrix <- list(
  "1" = c(0,3,1,0),
  "2" = c(3,0,3,4),
  "3" = c(1,3,0,5),
  "4" = c(0,4,5,0)
)

cat("Graph (adjacency matrix):\n")
for (i in names(graph_matrix)) {
  cat("Vertex", i, "-> [", paste(graph_matrix[[i]], collapse = ", "), "]\n")
}

cat("\nComputing global minimum cut:\n")
min_cut_result <- stoer_wagner_min_cut(graph_matrix)
cat("Minimum cut value:", min_cut_result$min_cut_value, "\n")
cat("Cut vertex (representative):", min_cut_result$cut_vertex, "\n")
