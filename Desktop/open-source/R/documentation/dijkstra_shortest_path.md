

``` r
# Dijkstra's Shortest Path Algorithm
#
# Dijkstra's algorithm finds the shortest path between a source vertex and all other vertices
# in a weighted graph with non-negative edge weights. It uses a greedy approach with a priority queue.
#
# Time Complexity: O((V + E) log V) with binary heap, O(V^2) with simple array
# Space Complexity: O(V) for distance and visited arrays
#
# Input: A weighted graph represented as adjacency list with weights, and a source vertex
# Output: Shortest distances from source to all vertices and the paths

# Priority queue implementation using simple vector (for educational purposes)
# In production, use more efficient data structures
create_priority_queue <- function() {
  list(
    elements = data.frame(vertex = integer(0), distance = numeric(0)),
    size = 0
  )
}

# Insert element into priority queue
pq_insert <- function(pq, vertex, distance) {
  pq$elements <- rbind(pq$elements, data.frame(vertex = vertex, distance = distance))
  pq$size <- pq$size + 1
  return(pq)
}

# Extract minimum element from priority queue
pq_extract_min <- function(pq) {
  if (pq$size == 0) {
    return(list(pq = pq, min_element = NULL))
  }
  
  min_idx <- which.min(pq$elements$distance)
  min_element <- pq$elements[min_idx, ]
  pq$elements <- pq$elements[-min_idx, ]
  pq$size <- pq$size - 1
  
  return(list(pq = pq, min_element = min_element))
}

# Check if priority queue is empty
pq_is_empty <- function(pq) {
  return(pq$size == 0)
}

# Main Dijkstra's algorithm implementation
dijkstra_shortest_path <- function(graph, source) {
  # Get all vertices in the graph
  all_vertices <- unique(c(names(graph), unlist(lapply(graph, function(x) x$vertex))))
  num_vertices <- max(all_vertices)
  
  # Initialize distances and previous vertices
  distances <- rep(Inf, num_vertices)
  previous <- rep(-1, num_vertices)
  visited <- rep(FALSE, num_vertices)
  
  # Set source distance to 0
  distances[source] <- 0
  
  # Create priority queue and add source
  pq <- create_priority_queue()
  pq <- pq_insert(pq, source, 0)
  
  while (!pq_is_empty(pq)) {
    # Extract vertex with minimum distance
    result <- pq_extract_min(pq)
    pq <- result$pq
    current <- result$min_element
    
    if (is.null(current)) break
    
    u <- current$vertex
    
    # Skip if already visited
    if (visited[u]) next
    
    # Mark as visited
    visited[u] <- TRUE
    
    # Update distances to neighbors
    if (as.character(u) %in% names(graph)) {
      for (edge in graph[[as.character(u)]]) {
        v <- edge$vertex
        weight <- edge$weight
        
        # Relaxation step
        if (!visited[v] && distances[u] + weight < distances[v]) {
          distances[v] <- distances[u] + weight
          previous[v] <- u
          pq <- pq_insert(pq, v, distances[v])
        }
      }
    }
  }
  
  return(list(
    distances = distances,
    previous = previous
  ))
}

# Reconstruct shortest path from source to target
get_shortest_path <- function(dijkstra_result, source, target) {
  previous <- dijkstra_result$previous
  distances <- dijkstra_result$distances
  
  # Check if target is reachable
  if (distances[target] == Inf) {
    return(list(
      path = NULL,
      distance = Inf
    ))
  }
  
  # Reconstruct path by backtracking
  path <- c()
  current <- target
  
  while (current != -1) {
    path <- c(current, path)
    current <- previous[current]
  }
  
  return(list(
    path = path,
    distance = distances[target]
  ))
}

# Find shortest paths to all vertices
get_all_shortest_paths <- function(dijkstra_result, source) {
  distances <- dijkstra_result$distances
  previous <- dijkstra_result$previous
  paths <- list()
  
  for (target in 1:length(distances)) {
    if (distances[target] != Inf) {
      path_result <- get_shortest_path(dijkstra_result, source, target)
      paths[[as.character(target)]] <- path_result
    }
  }
  
  return(paths)
}

# Example usage and testing
cat("=== Dijkstra's Shortest Path Algorithm ===\n")
```

```
## === Dijkstra's Shortest Path Algorithm ===
```

``` r
# Create a weighted graph as adjacency list
# Graph structure with weights:
#       1
#     /   \
#   4/     \2
#   /       \
#  2         3
#  |3      /1
#  |      /
#  4-----5
#     2
weighted_graph <- list(
  "1" = list(
    list(vertex = 2, weight = 4),
    list(vertex = 3, weight = 2)
  ),
  "2" = list(
    list(vertex = 4, weight = 3)
  ),
  "3" = list(
    list(vertex = 5, weight = 1)
  ),
  "4" = list(
    list(vertex = 5, weight = 2)
  ),
  "5" = list()
)

cat("Weighted graph structure:\n")
```

```
## Weighted graph structure:
```

``` r
for (vertex in names(weighted_graph)) {
  edges <- weighted_graph[[vertex]]
  if (length(edges) > 0) {
    edge_strs <- sapply(edges, function(e) paste0(e$vertex, "(", e$weight, ")"))
    cat("Vertex", vertex, "-> [", paste(edge_strs, collapse = ", "), "]\n")
  } else {
    cat("Vertex", vertex, "-> []\n")
  }
}
```

```
## Vertex 1 -> [ 2(4), 3(2) ]
## Vertex 2 -> [ 4(3) ]
## Vertex 3 -> [ 5(1) ]
## Vertex 4 -> [ 5(2) ]
## Vertex 5 -> []
```

``` r
# Run Dijkstra's algorithm from vertex 1
cat("\nRunning Dijkstra's algorithm from vertex 1:\n")
```

```
## 
## Running Dijkstra's algorithm from vertex 1:
```

``` r
result <- dijkstra_shortest_path(weighted_graph, 1)

# Display shortest distances
cat("Shortest distances from vertex 1:\n")
```

```
## Shortest distances from vertex 1:
```

``` r
for (i in 1:length(result$distances)) {
  if (result$distances[i] != Inf) {
    cat("To vertex", i, ": distance =", result$distances[i], "\n")
  }
}
```

```
## To vertex 1 : distance = 0 
## To vertex 2 : distance = 4 
## To vertex 3 : distance = 2 
## To vertex 4 : distance = 7 
## To vertex 5 : distance = 3
```

``` r
# Get shortest path to specific vertex
cat("\nShortest path from 1 to 5:\n")
```

```
## 
## Shortest path from 1 to 5:
```

``` r
path_to_5 <- get_shortest_path(result, 1, 5)
if (!is.null(path_to_5$path)) {
  cat("Path:", paste(path_to_5$path, collapse = " -> "), "\n")
  cat("Distance:", path_to_5$distance, "\n")
}
```

```
## Path: 1 -> 3 -> 5 
## Distance: 3
```

``` r
# Get all shortest paths
cat("\nAll shortest paths from vertex 1:\n")
```

```
## 
## All shortest paths from vertex 1:
```

``` r
all_paths <- get_all_shortest_paths(result, 1)
for (target in names(all_paths)) {
  path_info <- all_paths[[target]]
  cat("To vertex", target, ": ", paste(path_info$path, collapse = " -> "), 
      " (distance:", path_info$distance, ")\n")
}
```

```
## To vertex 1 :  1  (distance: 0 )
## To vertex 2 :  1 -> 2  (distance: 4 )
## To vertex 3 :  1 -> 3  (distance: 2 )
## To vertex 4 :  1 -> 2 -> 4  (distance: 7 )
## To vertex 5 :  1 -> 3 -> 5  (distance: 3 )
```

``` r
# Example with a more complex graph
cat("\n=== More Complex Weighted Graph Example ===\n")
```

```
## 
## === More Complex Weighted Graph Example ===
```

``` r
complex_weighted_graph <- list(
  "1" = list(
    list(vertex = 2, weight = 7),
    list(vertex = 3, weight = 9),
    list(vertex = 6, weight = 14)
  ),
  "2" = list(
    list(vertex = 3, weight = 10),
    list(vertex = 4, weight = 15)
  ),
  "3" = list(
    list(vertex = 4, weight = 11),
    list(vertex = 6, weight = 2)
  ),
  "4" = list(
    list(vertex = 5, weight = 6)
  ),
  "5" = list(),
  "6" = list(
    list(vertex = 5, weight = 9)
  )
)

cat("Complex weighted graph from vertex 1:\n")
```

```
## Complex weighted graph from vertex 1:
```

``` r
complex_result <- dijkstra_shortest_path(complex_weighted_graph, 1)

cat("Shortest distances:\n")
```

```
## Shortest distances:
```

``` r
for (i in 1:length(complex_result$distances)) {
  if (complex_result$distances[i] != Inf) {
    cat("To vertex", i, ": distance =", complex_result$distances[i], "\n")
  }
}
```

```
## To vertex 1 : distance = 0 
## To vertex 2 : distance = 7 
## To vertex 3 : distance = 9 
## To vertex 4 : distance = 20 
## To vertex 5 : distance = 20 
## To vertex 6 : distance = 11
```

``` r
# Shortest path to vertex 5
path_to_5_complex <- get_shortest_path(complex_result, 1, 5)
if (!is.null(path_to_5_complex$path)) {
  cat("Shortest path from 1 to 5:", paste(path_to_5_complex$path, collapse = " -> "), "\n")
  cat("Distance:", path_to_5_complex$distance, "\n")
}
```

```
## Shortest path from 1 to 5: 1 -> 3 -> 6 -> 5 
## Distance: 20
```

