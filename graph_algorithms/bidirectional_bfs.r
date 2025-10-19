# ==============================================================
# Bidirectional Breadth-First Search (BFS) Shortest Path Algorithm
# ==============================================================
#
# Description:
#   Finds the shortest path between a source and target in an
#   unweighted graph using Bidirectional BFS.
#
# Time Complexity: O(b^(d/2)) — much faster than normal BFS O(b^d)
# Space Complexity: O(V)
#
# Input:
#   graph  - adjacency list (list of integer vectors)
#   source - integer (starting vertex)
#   target - integer (destination vertex)
#
# Output:
#   A list containing:
#       path       - vector of vertices representing the path
#       distance   - number of edges in the shortest path
#       found      - logical flag (TRUE if path found, else FALSE)
#
# Example usage at bottom of file.
# ==============================================================

bidirectional_bfs <- function(graph, source, target) {
  if (source == target) {
    return(list(path = c(source), distance = 0, found = TRUE))
  }

  # Initialize BFS from both ends
  visited_from_source <- setNames(rep(FALSE, length(graph)), names(graph))
  visited_from_target <- setNames(rep(FALSE, length(graph)), names(graph))
  
  parent_from_source <- rep(NA, length(graph))
  parent_from_target <- rep(NA, length(graph))
  
  queue_source <- c(source)
  queue_target <- c(target)
  
  visited_from_source[source] <- TRUE
  visited_from_target[target] <- TRUE
  
  meeting_node <- NA
  
  # Function to check intersection
  get_intersection <- function() {
    common <- which(visited_from_source & visited_from_target)
    if (length(common) > 0) return(common[1])
    return(NA)
  }
  
  # Main loop
  while (length(queue_source) > 0 && length(queue_target) > 0) {
    # Expand one level from source side
    next_queue_list <- list()
    idx <- 1
    for (u in queue_source) {
      for (v in graph[[as.character(u)]]) {
        if (!visited_from_source[v]) {
          visited_from_source[v] <- TRUE
          parent_from_source[v] <- u
          next_queue_list[[idx]] <- v
          idx <- idx + 1
        }
      }
    }
    queue_source <- unlist(next_queue_list, use.names = FALSE)
    
    # Check intersection
    meeting_node <- get_intersection()
    if (!is.na(meeting_node)) break
    
    # Expand one level from target side
    next_queue_list <- list()
    idx <- 1
    for (u in queue_target) {
      for (v in graph[[as.character(u)]]) {
        if (!visited_from_target[v]) {
          visited_from_target[v] <- TRUE
          parent_from_target[v] <- u
          next_queue_list[[idx]] <- v
          idx <- idx + 1
        }
      }
    }
    queue_target <- unlist(next_queue_list, use.names = FALSE)
    
    # Check intersection again
    meeting_node <- get_intersection()
    if (!is.na(meeting_node)) break
  }
  
  if (is.na(meeting_node)) {
    return(list(path = NULL, distance = Inf, found = FALSE))
  }
  
  # Reconstruct path from source → meeting_node
  path1 <- c()
  node <- meeting_node
  while (!is.na(node)) {
    path1 <- c(node, path1)
    node <- parent_from_source[node]
  }
  
  # Reconstruct path from meeting_node → target
  path2 <- c()
  node <- parent_from_target[meeting_node]
  while (!is.na(node)) {
    path2 <- c(path2, node)
    node <- parent_from_target[node]
  }
  
  full_path <- c(path1, path2)
  return(list(path = full_path, distance = length(full_path) - 1, found = TRUE))
}

# ==============================================================
# Example Usage and Test
# ==============================================================

cat("=== Bidirectional BFS Shortest Path ===\n")

# Example Graph (Unweighted)
# 1 -- 2 -- 3
#  |    |
#  4 -- 5 -- 6

graph <- list(
  "1" = c(2, 4),
  "2" = c(1, 3, 5),
  "3" = c(2, 6),
  "4" = c(1, 5),
  "5" = c(2, 4, 6),
  "6" = c(3, 5)
)

cat("Graph adjacency list:\n")
for (v in names(graph)) {
  cat("Vertex", v, "-> [", paste(graph[[v]], collapse = ", "), "]\n")
}

cat("\nRunning Bidirectional BFS from 1 to 6...\n")
result <- bidirectional_bfs(graph, 1, 6)

if (result$found) {
  cat("Shortest Path Found!\n")
  cat("Path:", paste(result$path, collapse = " -> "), "\n")
  cat("Distance:", result$distance, "\n")
} else {
  cat("No path found between source and target.\n")
}