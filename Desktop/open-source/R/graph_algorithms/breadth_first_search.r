# Breadth-First Search (BFS) Algorithm
#
# BFS is a graph traversal algorithm that explores all vertices at the current depth
# before moving to vertices at the next depth level. It uses a queue data structure.
#
# Time Complexity: O(V + E) where V is vertices and E is edges
# Space Complexity: O(V) for the visited array and queue
#
# Input: An adjacency list representation of a graph and a starting vertex
# Output: The order in which vertices are visited during BFS traversal

# BFS function using queue (implemented with vector)
breadth_first_search <- function(graph, start_vertex) {
  # Get all vertices in the graph
  all_vertices <- unique(c(names(graph), unlist(graph)))
  
  # Initialize visited array and queue
  visited <- rep(FALSE, max(all_vertices))
  names(visited) <- 1:max(all_vertices)
  queue <- c(start_vertex)
  result <- c()
  
  # Mark starting vertex as visited
  visited[start_vertex] <- TRUE
  
  while (length(queue) > 0) {
    # Dequeue vertex from front of queue
    vertex <- queue[1]
    queue <- queue[-1]
    result <- c(result, vertex)
    
    # Add all unvisited neighbors to queue
    if (as.character(vertex) %in% names(graph)) {
      for (neighbor in graph[[as.character(vertex)]]) {
        if (!visited[neighbor]) {
          visited[neighbor] <- TRUE
          queue <- c(queue, neighbor)
        }
      }
    }
  }
  
  return(result)
}

# BFS to find shortest path between two vertices
bfs_shortest_path <- function(graph, start_vertex, end_vertex) {
  # Get all vertices in the graph
  all_vertices <- unique(c(names(graph), unlist(graph)))
  
  # Initialize visited array, queue, and parent tracking
  visited <- rep(FALSE, max(all_vertices))
  names(visited) <- 1:max(all_vertices)
  parent <- rep(-1, max(all_vertices))
  names(parent) <- 1:max(all_vertices)
  queue <- c(start_vertex)
  
  # Mark starting vertex as visited
  visited[start_vertex] <- TRUE
  
  while (length(queue) > 0) {
    # Dequeue vertex from front of queue
    vertex <- queue[1]
    queue <- queue[-1]
    
    # If we reached the target vertex, reconstruct path
    if (vertex == end_vertex) {
      path <- c()
      current <- end_vertex
      
      # Backtrack using parent array
      while (current != -1) {
        path <- c(current, path)
        current <- parent[current]
      }
      
      return(list(
        path = path,
        distance = length(path) - 1
      ))
    }
    
    # Add all unvisited neighbors to queue
    if (as.character(vertex) %in% names(graph)) {
      for (neighbor in graph[[as.character(vertex)]]) {
        if (!visited[neighbor]) {
          visited[neighbor] <- TRUE
          parent[neighbor] <- vertex
          queue <- c(queue, neighbor)
        }
      }
    }
  }
  
  # No path found
  return(list(
    path = NULL,
    distance = -1
  ))
}

# BFS to find all vertices at a specific distance
bfs_vertices_at_distance <- function(graph, start_vertex, target_distance) {
  # Get all vertices in the graph
  all_vertices <- unique(c(names(graph), unlist(graph)))
  
  # Initialize visited array, queue with distances
  visited <- rep(FALSE, max(all_vertices))
  names(visited) <- 1:max(all_vertices)
  queue <- list(list(vertex = start_vertex, distance = 0))
  vertices_at_distance <- c()
  
  # Mark starting vertex as visited
  visited[start_vertex] <- TRUE
  
  while (length(queue) > 0) {
    # Dequeue vertex with distance from front of queue
    current <- queue[[1]]
    queue <- queue[-1]
    vertex <- current$vertex
    distance <- current$distance
    
    # If current distance matches target, add to result
    if (distance == target_distance) {
      vertices_at_distance <- c(vertices_at_distance, vertex)
    }
    
    # Don't explore further if we've reached target distance
    if (distance < target_distance) {
      # Add all unvisited neighbors to queue
      if (as.character(vertex) %in% names(graph)) {
        for (neighbor in graph[[as.character(vertex)]]) {
          if (!visited[neighbor]) {
            visited[neighbor] <- TRUE
            queue <- c(queue, list(list(vertex = neighbor, distance = distance + 1)))
          }
        }
      }
    }
  }
  
  return(vertices_at_distance)
}

# Example usage and testing
cat("=== Breadth-First Search (BFS) Algorithm ===\n")

# Create a sample graph as adjacency list
# Graph structure:
#     1
#    / \
#   2   3
#  / \   \
# 4   5   6
graph <- list(
  "1" = c(2, 3),
  "2" = c(4, 5),
  "3" = c(6),
  "4" = c(),
  "5" = c(),
  "6" = c()
)

cat("Graph structure (adjacency list):\n")
for (vertex in names(graph)) {
  cat("Vertex", vertex, "-> [", paste(graph[[vertex]], collapse = ", "), "]\n")
}

# Test BFS traversal
cat("\nBFS starting from vertex 1:\n")
result <- breadth_first_search(graph, 1)
cat("Traversal order:", paste(result, collapse = " -> "), "\n")

# Test BFS from different starting vertex
cat("\nBFS starting from vertex 2:\n")
result_from_2 <- breadth_first_search(graph, 2)
cat("Traversal order:", paste(result_from_2, collapse = " -> "), "\n")

# Test shortest path finding
cat("\n=== Shortest Path Finding ===\n")
path_result <- bfs_shortest_path(graph, 1, 5)
if (!is.null(path_result$path)) {
  cat("Shortest path from 1 to 5:", paste(path_result$path, collapse = " -> "), "\n")
  cat("Distance:", path_result$distance, "\n")
} else {
  cat("No path found from 1 to 5\n")
}

# Test vertices at specific distance
cat("\n=== Vertices at Specific Distance ===\n")
vertices_dist_2 <- bfs_vertices_at_distance(graph, 1, 2)
cat("Vertices at distance 2 from vertex 1:", paste(vertices_dist_2, collapse = ", "), "\n")

# Example with a more complex connected graph
cat("\n=== Example with More Complex Graph ===\n")
complex_graph <- list(
  "1" = c(2, 3),
  "2" = c(1, 4, 5),
  "3" = c(1, 6),
  "4" = c(2, 7),
  "5" = c(2, 8),
  "6" = c(3, 9),
  "7" = c(4),
  "8" = c(5),
  "9" = c(6)
)

cat("Complex graph BFS starting from vertex 1:\n")
complex_result <- breadth_first_search(complex_graph, 1)
cat("Traversal order:", paste(complex_result, collapse = " -> "), "\n")

# Test shortest path in complex graph
path_complex <- bfs_shortest_path(complex_graph, 1, 9)
if (!is.null(path_complex$path)) {
  cat("Shortest path from 1 to 9:", paste(path_complex$path, collapse = " -> "), "\n")
  cat("Distance:", path_complex$distance, "\n")
}