# Topological Sort (Kahn's Algorithm)
#
# The topological sort of a directed acyclic graph (DAG) is a linear ordering 
# of vertices such that for every directed edge u -> v, vertex u comes before v.
#
# Time Complexity: O(V + E)
# Space Complexity: O(V)
#
# Applications:
# - Task scheduling and dependency resolution
# - Build systems (compilation order)
# - Job scheduling in distributed systems
# - Resolving package dependencies

topological_sort <- function(V, edges) {
  #' Perform topological sorting on a DAG
  #' @param V: Number of vertices (0 to V-1)
  #' @param edges: List of edge pairs (u, v)
  #' @return: Topologically sorted order or NULL if cycle exists
  
  adj <- vector("list", V)
  indegree <- rep(0, V)
  
  # Build adjacency list and calculate in-degrees
  for (e in edges) {
    u <- e[1]
    v <- e[2]
    adj[[u]] <- c(adj[[u]], v)
    indegree[v] <- indegree[v] + 1
  }
  
  # Initialize queue with vertices having no incoming edges
  q <- which(indegree == 0)
  topo <- c()
  
  # Process vertices in topological order
  while (length(q) > 0) {
    node <- q[1]
    q <- q[-1]
    topo <- c(topo, node)
    
    # Update in-degrees of neighbors
    for (nbr in adj[[node]]) {
      indegree[nbr] <- indegree[nbr] - 1
      if (indegree[nbr] == 0) {
        q <- c(q, nbr)  # Add to queue when in-degree becomes 0
      }
    }
  }
  
  # Check if valid topological sort exists
  if (length(topo) == V) {
    return(topo)
  } else {
    return(NULL)  # Cycle detected
  }
}

# Example Usage
# Test case 1: Simple DAG
cat("=== Test Case 1: Simple DAG ===\n")
V1 <- 4
edges1 <- list(c(0,1), c(0,2), c(2,3), c(1,3))
result1 <- topological_sort(V1, edges1)
if (!is.null(result1)) {
  cat("Topological Order:", paste(result1, collapse = " -> "), "\n")
} else {
  cat("Graph contains a cycle!\n")
}

# Test case 2: Larger DAG
cat("\n=== Test Case 2: Larger DAG ===\n")
V2 <- 6
edges2 <- list(c(5,2), c(5,0), c(4,0), c(4,1), c(2,3), c(3,1))
result2 <- topological_sort(V2, edges2)
if (!is.null(result2)) {
  cat("Topological Order:", paste(result2, collapse = " -> "), "\n")
} else {
  cat("Graph contains a cycle!\n")
}

# Test case 3: Graph with cycle
cat("\n=== Test Case 3: Graph with Cycle ===\n")
V3 <- 3
edges3 <- list(c(0,1), c(1,2), c(2,0))  # Cycle: 0->1->2->0
result3 <- topological_sort(V3, edges3)
if (!is.null(result3)) {
  cat("Topological Order:", paste(result3, collapse = " -> "), "\n")
} else {
  cat("Graph contains a cycle!\n")
}