# Function to perform topological sort
topological_sort <- function(graph) {
  # Number of vertices in the graph
  num_vertices <- length(graph)
  
  # Helper function to perform DFS
  dfs <- function(node, visited, stack) {
    visited[node] <- TRUE
    
    # Visit all adjacent vertices
    for (neighbor in graph[[node]]) {
      if (!visited[neighbor]) {
        dfs(neighbor, visited, stack)
      }
    }
    
    # Push the current node onto the stack
    stack <<- c(stack, node)
  }
  
  # Initialize data structures
  visited <- rep(FALSE, num_vertices)
  stack <- c()
  
  # Perform DFS for each unvisited vertex
  for (node in 1:num_vertices) {
    if (!visited[node]) {
      dfs(node, visited, stack)
    }
  }
  
  # Reverse the stack to get the topological order
  topological_order <- rev(stack)
  return(topological_order)
}

# Example usage
# Define a sample DAG as an adjacency list
# Here, we represent the graph as a list of vectors, where each vector contains the neighbors of the corresponding node.
graph <- list(
  c(2, 3),    # Node 1 points to nodes 2 and 3
  c(3, 4),    # Node 2 points to nodes 3 and 4
  c(5),       # Node 3 points to node 5
  c(5),       # Node 4 points to node 5
  numeric(0)  # Node 5 has no outgoing edges
)

topological_order <- topological_sort(graph)
cat("Topological Order:", topological_order, "\n")
