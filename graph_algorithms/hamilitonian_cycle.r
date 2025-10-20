# Hamiltonian Cycle Detection (Backtracking)
#
# The Hamiltonian Cycle problem determines whether there exists a cycle
# in a graph that visits each vertex exactly once and returns to the start.
# It is an NP-complete problem.
#
# This implementation uses backtracking to explore possible paths.
#
# Time Complexity: O(N!)
# Space Complexity: O(N)
#
# Input: adjacency matrix (square matrix where adj[i][j] = 1 if edge exists)
# Output: list containing:
#   - has_cycle: TRUE/FALSE
#   - cycle: list of vertices forming the Hamiltonian cycle (if found)
#
# Example usage at the end of this file.

hamiltonian_cycle <- function(graph) {
  num_vertices <- nrow(graph)
  path <- rep(-1, num_vertices)
  
  # Start at vertex 1 (can be any vertex)
  path[1] <- 1

  # Helper function to check if vertex v can be added at position pos
  is_safe <- function(v, graph, path, pos) {
    # Check if current vertex is adjacent to previous vertex
    if (graph[path[pos - 1], v] == 0) return(FALSE)
    
    # Check if vertex is already in the path
    if (v %in% path[1:(pos - 1)]) return(FALSE)
    
    return(TRUE)
  }

  # Recursive utility to build Hamiltonian cycle
  ham_cycle_util <- function(graph, path, pos) {
    if (pos == num_vertices + 1) {
      # If last vertex connects to the first → cycle found
      if (graph[path[num_vertices], path[1]] == 1) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }

    # Try adding each vertex as next candidate
    for (v in 2:num_vertices) {
      if (is_safe(v, graph, path, pos)) {
        path[pos] <- v
        if (ham_cycle_util(graph, path, pos + 1)) return(TRUE)
        # Backtrack
        path[pos] <- -1
      }
    }
    return(FALSE)
  }

  # Start backtracking from vertex 1
  if (ham_cycle_util(graph, path, 2)) {
    path <- c(path, path[1])  # complete the cycle
    return(list(has_cycle = TRUE, cycle = path))
  } else {
    return(list(has_cycle = FALSE, cycle = NULL))
  }
}

# ============================
# Example Usage and Test
# ============================

cat("=== Hamiltonian Cycle Detection ===\n")

# Example graph (Adjacency Matrix)
# Graph:
# 1 - 2 - 3
# |   |   |
# 4 - 5 - 6
# This graph contains a Hamiltonian cycle
ham_graph <- matrix(
  c(0,1,0,1,0,0,
    1,0,1,1,1,0,
    0,1,0,0,1,1,
    1,1,0,0,1,0,
    0,1,1,1,0,1,
    0,0,1,0,1,0),
  nrow = 6, byrow = TRUE
)

cat("Adjacency Matrix:\n")
print(ham_graph)

cat("\nRunning Hamiltonian Cycle detection...\n")
result <- hamiltonian_cycle(ham_graph)

if (result$has_cycle) {
  cat("Hamiltonian Cycle found:\n")
  cat("Cycle:", paste(result$cycle, collapse = " -> "), "\n")
} else {
  cat("No Hamiltonian Cycle exists in this graph.\n")
}
