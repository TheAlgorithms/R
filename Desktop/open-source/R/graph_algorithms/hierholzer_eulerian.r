# Hierholzer's Algorithm for Eulerian Circuit
#
# Hierholzer's algorithm finds an Eulerian circuit in a graph, i.e., a closed path
# that visits every edge exactly once. The graph must be connected, and all vertices
# must have even degree (for undirected graphs) or in-degree = out-degree (for directed graphs).
#
# Time Complexity: O(E)
# Space Complexity: O(V + E)
#
# Input: graph as an adjacency list (each entry is a vector of neighbors)
# Output: Eulerian circuit as a vector of vertices in order

hierholzer_eulerian <- function(graph) {
  # Copy of the graph to modify during traversal
  temp_graph <- lapply(graph, function(x) x)
  
  circuit <- c()
  stack <- c(1)  # Start from vertex 1 (can choose any vertex)
  
  while (length(stack) > 0) {
    u <- tail(stack, 1)
    if (length(temp_graph[[as.character(u)]]) > 0) {
      v <- temp_graph[[as.character(u)]][1]
      temp_graph[[as.character(u)]] <- temp_graph[[as.character(u)]][-1]
      stack <- c(stack, v)
    } else {
      circuit <- c(stack[length(stack)], circuit)
      stack <- stack[-length(stack)]
    }
  }
  
  return(circuit)
}

# Example usage
cat("=== Hierholzer's Algorithm for Eulerian Circuit ===\n")

# Example undirected graph with Eulerian circuit:
# 1-2, 1-3, 2-3, 2-4, 3-4
hierholzer_graph <- list(
  "1" = c(2,3),
  "2" = c(1,3,4),
  "3" = c(1,2,4),
  "4" = c(2,3)
)

cat("Graph (adjacency list):\n")
for (v in names(hierholzer_graph)) {
  edges <- hierholzer_graph[[v]]
  cat("Vertex", v, "-> [", paste(edges, collapse = ", "), "]\n")
}

cat("\nFinding Eulerian circuit:\n")
euler_circuit <- hierholzer_eulerian(hierholzer_graph)
cat("Eulerian circuit:", paste(euler_circuit, collapse = " -> "), "\n")

