# bridge_detector.r
# Bridge Detection Algorithm in R using Tarjan's Algorithm
# Finds all critical edges (bridges) in an undirected graph.
# A bridge is an edge whose removal increases the number of connected components.
#
# Algorithm details:
# - Uses DFS traversal with discovery time and low-link values
# - disc[v]: Discovery time of vertex v in DFS
# - low[v]: Earliest discovered vertex reachable from v's subtree
# - Bridge condition: For edge (u,v), if low[v] > disc[u], then (u,v) is a bridge
# - Time complexity: O(V + E) where V = vertices, E = edges
# - Space complexity: O(V) for visited, discovery, and low-link arrays

library(R6)

BridgeDetector <- R6Class(
  "BridgeDetector",
  
  public = list(
    vertices = NULL,
    graph = NULL,
    time_counter = NULL,
    
    initialize = function(n_vertices) {
      "Initialize the bridge detector with specified number of vertices"
      if (!is.numeric(n_vertices) || n_vertices < 0 || n_vertices != round(n_vertices)) {
        stop("Number of vertices must be a non-negative integer")
      }
      self$vertices <- n_vertices
      self$graph <- vector("list", n_vertices)
      self$time_counter <- 0
      
      for (i in seq_len(n_vertices)) {
        self$graph[[i]] <- integer(0)
      }
    },
    
    add_edge = function(u, v) {
      "Add an undirected edge between vertices u and v (0-indexed)"
      if (!is.numeric(u) || !is.numeric(v) || 
          u < 0 || v < 0 || 
          u >= self$vertices || v >= self$vertices ||
          u != round(u) || v != round(v)) {
        stop("Vertex indices must be integers in range [0, n_vertices-1]")
      }
      u_idx <- u + 1
      v_idx <- v + 1
      self$graph[[u_idx]] <- c(self$graph[[u_idx]], v_idx)
      self$graph[[v_idx]] <- c(self$graph[[v_idx]], u_idx)
    },
    
    find_bridges = function() {
      "Find all bridges in the graph using Tarjan's algorithm"
      visited <- rep(FALSE, self$vertices)
      disc <- rep(Inf, self$vertices)
      low <- rep(Inf, self$vertices)
      parent <- rep(-1, self$vertices)
      bridges <- list()
      
      self$time_counter <- 0
      
      for (v in seq_len(self$vertices)) {
        if (!visited[v]) {
          res <- private$dfs_bridge(v, visited, disc, low, parent, bridges)
          visited <- res$visited
          disc <- res$disc
          low <- res$low
          parent <- res$parent
          bridges <- res$bridges
        }
      }
      return(bridges)
    },
    
    print_graph = function() {
      "Print adjacency list of the graph (0-indexed)"
      cat("Graph Adjacency List:\n")
      for (i in seq_len(self$vertices)) {
        neighbors <- if (length(self$graph[[i]]) > 0) {
          paste(self$graph[[i]] - 1, collapse = ", ")
        } else {
          "none"
        }
        cat(sprintf("Vertex %d: %s\n", i - 1, neighbors))
      }
    }
  ),
  
  private = list(
    dfs_bridge = function(u, visited, disc, low, parent, bridges) {
      visited[u] <- TRUE
      disc[u] <- self$time_counter
      low[u] <- self$time_counter
      self$time_counter <- self$time_counter + 1
      
      for (v_idx in self$graph[[u]]) {
        if (!visited[v_idx]) {
          parent[v_idx] <- u
          res <- private$dfs_bridge(v_idx, visited, disc, low, parent, bridges)
          visited <- res$visited
          disc <- res$disc
          low <- res$low
          parent <- res$parent
          bridges <- res$bridges
          
          low[u] <- min(low[u], low[v_idx])
          
          if (low[v_idx] > disc[u]) {
            bridges[[length(bridges) + 1]] <- c(u - 1, v_idx - 1)
          }
        } else if (v_idx != parent[u]) {
          low[u] <- min(low[u], disc[v_idx])
        }
      }
      
      return(list(
        visited = visited, 
        disc = disc, 
        low = low, 
        parent = parent, 
        bridges = bridges
      ))
    }
  )
)

# Demonstration
demonstrate_bridge_detection <- function() {
  cat("=== Bridge Detection Algorithm Demo ===\n\n")
  
  # Example 1
  cat("Example 1: Simple network with bridges\n")
  cat("Graph: 0-1-2-3\n")
  cat("       |   |\n")
  cat("       4   5\n\n")
  
  detector1 <- BridgeDetector$new(6)
  detector1$add_edge(0, 1)
  detector1$add_edge(1, 2)
  detector1$add_edge(2, 3)
  detector1$add_edge(0, 4)
  detector1$add_edge(2, 5)
  
  bridges1 <- detector1$find_bridges()
  cat("Bridges found:\n")
  for (b in bridges1) {
    cat(sprintf("  (%d, %d)\n", b[1], b[2]))
  }
  cat("All edges are critical - removing any disconnects the network.\n\n")
  
  # Example 2
  cat("Example 2: Network with cycle (no bridges)\n")
  cat("Graph: 0-1-2\n")
  cat("       |   |\n")
  cat("       +---+\n\n")
  
  detector2 <- BridgeDetector$new(3)
  detector2$add_edge(0, 1)
  detector2$add_edge(1, 2)
  detector2$add_edge(2, 0)
  
  bridges2 <- detector2$find_bridges()
  if (length(bridges2) == 0) {
    cat("Bridges found: None\n")
    cat("The cycle provides redundancy - no single edge is critical.\n\n")
  }
  
  # Example 3
  cat("Example 3: Complex network topology\n")
  detector3 <- BridgeDetector$new(7)
  detector3$add_edge(0, 1)
  detector3$add_edge(1, 2)
  detector3$add_edge(2, 0)
  detector3$add_edge(1, 3)
  detector3$add_edge(3, 4)
  detector3$add_edge(4, 5)
  detector3$add_edge(5, 6)
  detector3$add_edge(6, 4)
  
  bridges3 <- detector3$find_bridges()
  cat("Bridges found:\n")
  for (b in bridges3) {
    cat(sprintf("  (%d, %d)\n", b[1], b[2]))
  }
  cat("Edge (1,3) connects two robust sub-networks.\n\n")
  
  # Example 4: Testing print_graph
  cat("Example 4: Viewing graph structure\n")
  detector4 <- BridgeDetector$new(4)
  detector4$add_edge(0, 1)
  detector4$add_edge(1, 2)
  detector4$add_edge(2, 3)
  detector4$print_graph()
  cat("\n")
  
  # Example 5: Edge cases
  cat("Example 5: Edge cases\n")
  
  # Empty graph
  detector5 <- BridgeDetector$new(0)
  bridges5 <- detector5$find_bridges()
  cat("Empty graph bridges: ", length(bridges5), "\n")
  
  # Single edge
  detector6 <- BridgeDetector$new(2)
  detector6$add_edge(0, 1)
  bridges6 <- detector6$find_bridges()
  cat("Single edge graph bridges:\n")
  for (b in bridges6) {
    cat(sprintf("  (%d, %d)\n", b[1], b[2]))
  }
  
  # Disconnected components
  detector7 <- BridgeDetector$new(4)
  detector7$add_edge(0, 1)
  detector7$add_edge(2, 3)
  bridges7 <- detector7$find_bridges()
  cat("Disconnected components bridges:\n")
  for (b in bridges7) {
    cat(sprintf("  (%d, %d)\n", b[1], b[2]))
  }
  cat("\n")
}

# Run demo
demonstrate_bridge_detection()