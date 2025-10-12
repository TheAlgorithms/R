# Floyd-Warshall Algorithm Implementation in R
# Finds shortest paths between all pairs of vertices in a weighted graph
# Can handle negative edge weights, but not negative cycles
# Time complexity: O(V^3) where V is number of vertices
# Space complexity: O(V^2) for distance and predecessor matrices

library(R6)

#' FloydWarshall Class
#' @description R6 class implementing the Floyd-Warshall algorithm
#' @details Finds shortest paths between all pairs of vertices in a weighted directed graph.
#' Can handle:
#' - Positive and negative edge weights
#' - Direct path reconstruction
#' - Cycle detection
#' - Disconnected components (represented by Inf)
FloydWarshall <- R6Class(
  "FloydWarshall",
  
  public = list(
    #' @description Initialize the algorithm with graph size
    #' @param n_vertices Number of vertices in the graph
    initialize = function(n_vertices) {
      if (!is.numeric(n_vertices) || n_vertices < 0 || n_vertices != round(n_vertices)) {
        stop("Number of vertices must be a non-negative integer")
      }
      
      self$n_vertices <- n_vertices
      private$initialize_matrices()
      invisible(self)
    },
    
    #' @description Add a weighted edge to the graph
    #' @param from Source vertex (1-based indexing)
    #' @param to Target vertex (1-based indexing)
    #' @param weight Edge weight (can be negative)
    add_edge = function(from, to, weight) {
      private$validate_vertices(from, to)
      if (!is.numeric(weight)) {
        stop("Edge weight must be numeric")
      }
      
      private$dist_matrix[from, to] <- weight
      private$pred_matrix[from, to] <- from
      invisible(self)
    },
    
    #' @description Run the Floyd-Warshall algorithm
    #' @return List containing distance matrix and presence of negative cycles
    run = function() {
      # Floyd-Warshall main loop
      for (k in 1:self$n_vertices) {
        for (i in 1:self$n_vertices) {
          for (j in 1:self$n_vertices) {
            if (!is.infinite(private$dist_matrix[i, k]) && 
                !is.infinite(private$dist_matrix[k, j])) {
              new_dist <- private$dist_matrix[i, k] + private$dist_matrix[k, j]
              if (new_dist < private$dist_matrix[i, j]) {
                private$dist_matrix[i, j] <- new_dist
                private$pred_matrix[i, j] <- private$pred_matrix[k, j]
              }
            }
          }
        }
      }
      
      # Check for negative cycles
      has_negative_cycle <- FALSE
      for (i in 1:self$n_vertices) {
        if (private$dist_matrix[i, i] < 0) {
          has_negative_cycle <- TRUE
          break
        }
      }
      
      private$algorithm_run <- TRUE
      
      return(list(
        distances = private$dist_matrix,
        has_negative_cycle = has_negative_cycle
      ))
    },
    
    #' @description Get the shortest path between two vertices
    #' @param from Source vertex
    #' @param to Target vertex
    #' @return List containing path and total distance
    get_path = function(from, to) {
      if (!private$algorithm_run) {
        stop("Run the algorithm first using run()")
      }
      
      private$validate_vertices(from, to)
      
      if (is.infinite(private$dist_matrix[from, to])) {
        return(list(
          path = numeric(0),
          distance = Inf,
          exists = FALSE
        ))
      }
      
      # Reconstruct path
      path <- c(from)
      current <- from
      
      while (current != to) {
        current <- private$pred_matrix[current, to]
        path <- c(path, current)
        
        # Check for cycles
        if (length(path) > self$n_vertices) {
          stop("Negative cycle detected in path reconstruction")
        }
      }
      
      return(list(
        path = path,
        distance = private$dist_matrix[from, to],
        exists = TRUE
      ))
    },
    
    #' @description Get minimum distances from a source vertex to all others
    #' @param from Source vertex
    #' @return Named vector of distances
    get_distances_from = function(from) {
      if (!private$algorithm_run) {
        stop("Run the algorithm first using run()")
      }
      
      private$validate_vertices(from)
      return(private$dist_matrix[from, ])
    },
    
    #' @description Check if the graph has a negative cycle
    #' @return TRUE if negative cycle exists, FALSE otherwise
    has_negative_cycle = function() {
      if (!private$algorithm_run) {
        stop("Run the algorithm first using run()")
      }
      
      for (i in 1:self$n_vertices) {
        if (private$dist_matrix[i, i] < 0) {
          return(TRUE)
        }
      }
      return(FALSE)
    },
    
    #' @description Print the distance matrix
    print_distances = function() {
      if (!private$algorithm_run) {
        stop("Run the algorithm first using run()")
      }
      
      cat("Distance Matrix:\n")
      print(private$dist_matrix)
      invisible(self)
    },
    
    # Public fields
    n_vertices = NULL
  ),
  
  private = list(
    dist_matrix = NULL,
    pred_matrix = NULL,
    algorithm_run = FALSE,
    
    initialize_matrices = function() {
      # Initialize distance matrix with Inf for non-adjacent vertices
      private$dist_matrix <- matrix(Inf, nrow = self$n_vertices, ncol = self$n_vertices)
      diag(private$dist_matrix) <- 0
      
      # Initialize predecessor matrix
      private$pred_matrix <- matrix(NA, nrow = self$n_vertices, ncol = self$n_vertices)
      for (i in 1:self$n_vertices) {
        private$pred_matrix[i, i] <- i
      }
    },
    
    validate_vertices = function(from, to = NULL) {
      vertices <- if (is.null(to)) from else c(from, to)
      
      if (!all(is.numeric(vertices)) || 
          !all(vertices == round(vertices)) ||
          !all(vertices >= 1) ||
          !all(vertices <= self$n_vertices)) {
        stop("Vertex indices must be integers between 1 and ", self$n_vertices)
      }
    }
  )
)

# Demonstration
demonstrate_floyd_warshall <- function() {
  cat("=== Floyd-Warshall Algorithm Demo ===\n\n")
  
  # Example 1: Simple weighted graph
  cat("Example 1: Simple weighted graph\n")
  cat("Graph: 4 vertices with various weighted edges\n\n")
  
  fw <- FloydWarshall$new(4)
  
  # Add edges (with weights)
  fw$add_edge(1, 2, 5)
  fw$add_edge(2, 3, 3)
  fw$add_edge(3, 4, 1)
  fw$add_edge(1, 3, 10)
  fw$add_edge(2, 4, 6)
  
  # Run algorithm
  result <- fw$run()
  
  cat("All-pairs shortest distances:\n")
  fw$print_distances()
  
  # Get specific path
  path_result <- fw$get_path(1, 4)
  cat("\nShortest path from 1 to 4:\n")
  cat(sprintf("Path: %s\n", paste(path_result$path, collapse = " → ")))
  cat(sprintf("Distance: %d\n\n", path_result$distance))
  
  # Example 2: Graph with negative weights
  cat("Example 2: Graph with negative weights\n")
  cat("Graph: 3 vertices with some negative edges\n\n")
  
  fw2 <- FloydWarshall$new(3)
  fw2$add_edge(1, 2, 4)
  fw2$add_edge(2, 3, -2)
  fw2$add_edge(1, 3, 5)
  
  result2 <- fw2$run()
  
  cat("All-pairs shortest distances:\n")
  fw2$print_distances()
  
  # Example 3: Negative cycle detection
  cat("\nExample 3: Negative cycle detection\n")
  cat("Graph: 3 vertices with a negative cycle\n\n")
  
  fw3 <- FloydWarshall$new(3)
  fw3$add_edge(1, 2, 1)
  fw3$add_edge(2, 3, -5)
  fw3$add_edge(3, 1, 2)
  
  result3 <- fw3$run()
  
  cat(sprintf("Contains negative cycle: %s\n\n",
              ifelse(result3$has_negative_cycle, "Yes", "No")))
  
  # Example 4: Disconnected components
  cat("Example 4: Disconnected components\n")
  cat("Graph: 4 vertices with two components\n\n")
  
  fw4 <- FloydWarshall$new(4)
  fw4$add_edge(1, 2, 3)
  fw4$add_edge(3, 4, 2)
  
  result4 <- fw4$run()
  
  cat("All-pairs shortest distances:\n")
  fw4$print_distances()
  
  cat("\n=== Demo Complete ===\n")
}

# Run demonstration if not in interactive mode
if (!interactive()) {
  demonstrate_floyd_warshall()
}