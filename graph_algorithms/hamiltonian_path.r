# Hamiltonian Path (Backtracking)
#
# This implementation searches for a Hamiltonian Path in an undirected graph
# represented by an adjacency matrix. It uses backtracking to try all possible
# vertex sequences. The implementation follows the style used in other
# algorithms in the `R/graph_algorithms` folder.
#
# Time Complexity: O(n!) in the worst case (backtracking over permutations)
# Space Complexity: O(n) for the path and recursion stack
#
# Input: adjacency matrix `graph` (n x n)
# Output: prints a Hamiltonian path if found and returns TRUE, otherwise prints
#         a message and returns FALSE

# Function to check if vertex v can be added to path at position pos
isSafe <- function(v, graph, path, pos) {
  # Check adjacency between current vertex and previous vertex
  if (graph[path[pos - 1], v] == 0)
    return(FALSE)

  # Check if vertex is already in path
  if (v %in% path)
    return(FALSE)

  return(TRUE)
}

# Recursive function to find Hamiltonian path
hamiltonianUtil <- function(graph, path, pos) {
  n <- nrow(graph)

  # Base case: if all vertices are included in the path
  if (pos > n)
    return(TRUE)

  for (v in 1:n) {
    if (isSafe(v, graph, path, pos)) {
      path[pos] <- v

      if (hamiltonianUtil(graph, path, pos + 1))
        return(TRUE)

      # Backtrack
      path[pos] <- -1
    }
  }
  return(FALSE)
}

# Main function to find Hamiltonian path
hamiltonianPath <- function(graph) {
  n <- nrow(graph)

  for (start in 1:n) {
    path <- rep(-1, n)
    path[1] <- start

    if (hamiltonianUtil(graph, path, 2)) {
      cat("Hamiltonian Path found:\n")
      print(path)
      return(TRUE)
    }
  }

  cat("No Hamiltonian Path found.\n")
  return(FALSE)
}

# Example usage and test
cat("=== Hamiltonian Path (Backtracking) ===\n")

graph <- matrix(c(
  0, 1, 1, 0,
  1, 0, 1, 1,
  1, 1, 0, 1,
  0, 1, 1, 0
), nrow = 4, byrow = TRUE)

cat("Adjacency matrix:\n")
print(graph)

cat("\nSearching for Hamiltonian Path...\n")
hamiltonianPath(graph)
