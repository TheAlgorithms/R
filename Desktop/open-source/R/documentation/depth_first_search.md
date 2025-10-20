

``` r
# Depth-First Search (DFS) Algorithm
# 
# DFS is a graph traversal algorithm that explores as far as possible along each branch
# before backtracking. It uses a stack data structure (implemented via recursion here).
#
# Time Complexity: O(V + E) where V is vertices and E is edges
# Space Complexity: O(V) for the visited array and recursion stack
#
# Input: An adjacency list representation of a graph and a starting vertex
# Output: The order in which vertices are visited during DFS traversal

# Recursive DFS function
dfs_recursive <- function(graph, vertex, visited, result) {
  # Mark current vertex as visited
  visited[vertex] <- TRUE
  result <- c(result, vertex)
  
  # Visit all unvisited adjacent vertices
  if (vertex %in% names(graph)) {
    for (neighbor in graph[[as.character(vertex)]]) {
      if (!visited[neighbor]) {
        result <- dfs_recursive(graph, neighbor, visited, result)
      }
    }
  }
  
  return(result)
}

# Main DFS function
depth_first_search <- function(graph, start_vertex) {
  # Get all vertices in the graph
  all_vertices <- unique(c(names(graph), unlist(graph)))
  
  # Initialize visited array
  visited <- rep(FALSE, max(all_vertices))
  names(visited) <- 1:max(all_vertices)
  
  # Perform DFS starting from the given vertex
  result <- dfs_recursive(graph, start_vertex, visited, c())
  
  return(result)
}

# Iterative DFS function using explicit stack
dfs_iterative <- function(graph, start_vertex) {
  # Get all vertices in the graph
  all_vertices <- unique(c(names(graph), unlist(graph)))
  
  # Initialize visited array and stack
  visited <- rep(FALSE, max(all_vertices))
  names(visited) <- 1:max(all_vertices)
  stack <- c(start_vertex)
  result <- c()
  
  while (length(stack) > 0) {
    # Pop vertex from stack
    vertex <- stack[length(stack)]
    stack <- stack[-length(stack)]
    
    if (!visited[vertex]) {
      # Mark as visited and add to result
      visited[vertex] <- TRUE
      result <- c(result, vertex)
      
      # Add all unvisited neighbors to stack (in reverse order to maintain left-to-right traversal)
      if (as.character(vertex) %in% names(graph)) {
        neighbors <- graph[[as.character(vertex)]]
        for (neighbor in rev(neighbors)) {
          if (!visited[neighbor]) {
            stack <- c(stack, neighbor)
          }
        }
      }
    }
  }
  
  return(result)
}

# Example usage and testing
cat("=== Depth-First Search (DFS) Algorithm ===\n")
```

```
## === Depth-First Search (DFS) Algorithm ===
```

``` r
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
```

```
## Graph structure (adjacency list):
```

``` r
for (vertex in names(graph)) {
  cat("Vertex", vertex, "-> [", paste(graph[[vertex]], collapse = ", "), "]\n")
}
```

```
## Vertex 1 -> [ 2, 3 ]
## Vertex 2 -> [ 4, 5 ]
## Vertex 3 -> [ 6 ]
## Vertex 4 -> [  ]
## Vertex 5 -> [  ]
## Vertex 6 -> [  ]
```

``` r
# Test recursive DFS
cat("\nRecursive DFS starting from vertex 1:\n")
```

```
## 
## Recursive DFS starting from vertex 1:
```

``` r
result_recursive <- depth_first_search(graph, 1)
cat("Traversal order:", paste(result_recursive, collapse = " -> "), "\n")
```

```
## Traversal order: 1 -> 2 -> 4 -> 5 -> 3 -> 6
```

``` r
# Test iterative DFS
cat("\nIterative DFS starting from vertex 1:\n")
```

```
## 
## Iterative DFS starting from vertex 1:
```

``` r
result_iterative <- dfs_iterative(graph, 1)
cat("Traversal order:", paste(result_iterative, collapse = " -> "), "\n")
```

```
## Traversal order: 1 -> 2 -> 4 -> 5 -> 3 -> 6
```

``` r
# Test with different starting vertex
cat("\nRecursive DFS starting from vertex 2:\n")
```

```
## 
## Recursive DFS starting from vertex 2:
```

``` r
result_from_2 <- depth_first_search(graph, 2)
cat("Traversal order:", paste(result_from_2, collapse = " -> "), "\n")
```

```
## Traversal order: 2 -> 4 -> 5
```

``` r
# Example with a more complex graph (with cycles)
cat("\n=== Example with Cyclic Graph ===\n")
```

```
## 
## === Example with Cyclic Graph ===
```

``` r
cyclic_graph <- list(
  "1" = c(2, 3),
  "2" = c(1, 4),
  "3" = c(1, 5),
  "4" = c(2, 6),
  "5" = c(3, 6),
  "6" = c(4, 5)
)

cat("Cyclic graph structure:\n")
```

```
## Cyclic graph structure:
```

``` r
for (vertex in names(cyclic_graph)) {
  cat("Vertex", vertex, "-> [", paste(cyclic_graph[[vertex]], collapse = ", "), "]\n")
}
```

```
## Vertex 1 -> [ 2, 3 ]
## Vertex 2 -> [ 1, 4 ]
## Vertex 3 -> [ 1, 5 ]
## Vertex 4 -> [ 2, 6 ]
## Vertex 5 -> [ 3, 6 ]
## Vertex 6 -> [ 4, 5 ]
```

``` r
cat("\nDFS on cyclic graph starting from vertex 1:\n")
```

```
## 
## DFS on cyclic graph starting from vertex 1:
```

``` r
cyclic_result <- depth_first_search(cyclic_graph, 1)
cat("Traversal order:", paste(cyclic_result, collapse = " -> "), "\n")
```

```
## Traversal order: 1 -> 2 -> 4 -> 6 -> 5 -> 3 -> 3 -> 5 -> 6 -> 4 -> 2
```

