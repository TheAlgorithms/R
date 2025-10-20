# Topological Sort Algorithm (Kahn's Algorithm)
#
# Topological sorting of a Directed Acyclic Graph (DAG) is a linear ordering 
# of vertices such that for every directed edge u -> v, vertex u comes before v.
# It is not possible if the graph has cycles.
#
# Time Complexity: O(V + E)
# Space Complexity: O(V + E)
#
# Input: graph as an adjacency list where each entry is a list of vertices it points to
# Output: A list containing the topological order and a flag indicating if a cycle exists

topological_sort <- function(graph) {
  all_vertices <- unique(c(names(graph), unlist(graph)))
  all_vertices <- as.numeric(all_vertices)
  
  num_vertices <- max(all_vertices)
  
  # Compute in-degree of each vertex
  in_degree <- rep(0, num_vertices)
  for (u_char in names(graph)) {
    u <- as.numeric(u_char)
    for (v in graph[[u_char]]) {
      in_degree[v] <- in_degree[v] + 1
    }
  }
  
  # Initialize queue with vertices having in-degree 0
  queue <- which(in_degree == 0)
  topo_order <- c()
  
  while (length(queue) > 0) {
    u <- queue[1]
    queue <- queue[-1]
    topo_order <- c(topo_order, u)
    
    if (!is.null(graph[[as.character(u)]])) {
      for (v in graph[[as.character(u)]]) {
        in_degree[v] <- in_degree[v] - 1
        if (in_degree[v] == 0) {
          queue <- c(queue, v)
        }
      }
    }
  }
  
  # Check for cycles
  has_cycle <- length(topo_order) != num_vertices
  
  return(list(
    topo_order = if (!has_cycle) topo_order else NULL,
    has_cycle = has_cycle
  ))
}

# Example usage
cat("=== Topological Sort (Kahn's Algorithm) ===\n")

# Example DAG
# Graph structure:
# 1 -> 2, 1 -> 3
# 2 -> 4
# 3 -> 4
# 4 -> 5
topo_graph <- list(
  "1" = c(2, 3),
  "2" = c(4),
  "3" = c(4),
  "4" = c(5),
  "5" = c()
)

cat("Graph (adjacency list):\n")
for (v in names(topo_graph)) {
  edges <- topo_graph[[v]]
  if (length(edges) > 0) {
    cat("Vertex", v, "-> [", paste(edges, collapse = ", "), "]\n")
  } else {
    cat("Vertex", v, "-> []\n")
  }
}

cat("\nRunning Topological Sort:\n")
ts_result <- topological_sort(topo_graph)

if (!ts_result$has_cycle) {
  cat("Topological Order:", paste(ts_result$topo_order, collapse = " -> "), "\n")
} else {
  cat("Cycle detected! Topological sort not possible.\n")
}
