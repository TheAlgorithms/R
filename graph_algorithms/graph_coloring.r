# Graph Coloring Algorithm using Backtracking
#
# The graph coloring problem involves assigning colors to vertices of a graph such that
# no two adjacent vertices share the same color. This implementation uses backtracking
# to find a valid coloring with a specified number of colors. (The chromatic number is computed by find_chromatic_number().)
#
# Time Complexity: O(m^V) where m is number of colors and V is number of vertices
# Space Complexity: O(V) for recursion stack and color assignment array
#
# Input: graph as adjacency matrix (n x n), number of colors
# Output: A list with fields:
#   - success: TRUE if a valid coloring was found, FALSE otherwise
#   - colors: integer vector of color assignments for each vertex (or NULL if not successful)
#   - num_colors_used: number of colors used (or NULL if not successful)

graph_coloring <- function(graph, num_colors) {
  n <- nrow(graph)
  colors <- rep(0, n)
  
  # Check if color assignment is safe for vertex v
  is_safe <- function(v, c) {
    for (i in 1:n) {
      if (graph[v, i] == 1 && colors[i] == c) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
  
  # Backtracking function to color vertices
  color_vertex <- function(v) {
    if (v > n) {
      return(TRUE)
    }
    
    for (c in 1:num_colors) {
      if (is_safe(v, c)) {
        colors[v] <<- c
        
        if (color_vertex(v + 1)) {
          return(TRUE)
        }
        
        colors[v] <<- 0
      }
    }
    
    return(FALSE)
  }
  
  if (color_vertex(1)) {
    return(list(
      success = TRUE,
      colors = colors,
      num_colors_used = max(colors)
    ))
  } else {
    return(list(
      success = FALSE,
      colors = NULL,
      num_colors_used = NULL
    ))
  }
}

# Find chromatic number (minimum colors needed)
find_chromatic_number <- function(graph) {
  n <- nrow(graph)
  
  for (num_colors in 1:n) {
    result <- graph_coloring(graph, num_colors)
    if (result$success) {
      return(num_colors)
    }
  }
  
  return(n)
}

# Greedy graph coloring (faster but not always optimal)
greedy_coloring <- function(graph) {
  n <- nrow(graph)
  colors <- rep(0, n)
  
  for (v in 1:n) {
    available <- rep(TRUE, n)
    
    for (i in 1:n) {
      if (graph[v, i] == 1 && colors[i] != 0) {
        available[colors[i]] <- FALSE
      }
    }
    
    for (c in 1:n) {
      if (available[c]) {
        colors[v] <- c
        break
      }
    }
  }
  
  num_colors_used <- max(colors)
  
  return(list(
    colors = colors,
    num_colors_used = num_colors_used
  ))
}

# Welsh-Powell algorithm (colors vertices in descending degree order)
welsh_powell_coloring <- function(graph) {
  n <- nrow(graph)
  degrees <- rowSums(graph)
  vertex_order <- order(degrees, decreasing = TRUE)
  
  colors <- rep(0, n)
  
  for (v in vertex_order) {
    available <- rep(TRUE, n + 1)
    
    for (i in 1:n) {
      if (graph[v, i] == 1 && colors[i] != 0) {
        available[colors[i]] <- FALSE
      }
    }
    
    for (c in 1:(n + 1)) {
      if (available[c]) {
        colors[v] <- c
        break
      }
    }
  }
  
  num_colors_used <- max(colors)
  
  return(list(
    colors = colors,
    num_colors_used = num_colors_used
  ))
}

# Validate coloring solution
validate_coloring <- function(graph, colors) {
  n <- nrow(graph)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (graph[i, j] == 1 && colors[i] == colors[j]) {
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
}

# Example usage and tests
cat("=== Graph Coloring Algorithm ===\n\n")

# Example 1: Simple triangle graph (needs 3 colors)
cat("Example 1: Triangle Graph\n")
triangle <- matrix(c(
  0, 1, 1,
  1, 0, 1,
  1, 1, 0
), nrow = 3, byrow = TRUE)

cat("Adjacency Matrix:\n")
print(triangle)

result1 <- graph_coloring(triangle, 3)
cat("\nBacktracking with 3 colors:\n")
cat("Success:", result1$success, "\n")
cat("Color assignment:", result1$colors, "\n")
cat("Valid:", validate_coloring(triangle, result1$colors), "\n")

result1_fail <- graph_coloring(triangle, 2)
cat("\nBacktracking with 2 colors:\n")
cat("Success:", result1_fail$success, "\n")

# Example 2: Petersen graph (chromatic number = 3)
cat("\n\nExample 2: Petersen Graph (10 vertices)\n")
petersen <- matrix(0, nrow = 10, ncol = 10)
edges <- list(
  c(1, 2), c(2, 3), c(3, 4), c(4, 5), c(5, 1),
  c(1, 6), c(2, 7), c(3, 8), c(4, 9), c(5, 10),
  c(6, 8), c(8, 10), c(10, 7), c(7, 9), c(9, 6)
)
for (edge in edges) {
  petersen[edge[1], edge[2]] <- 1
  petersen[edge[2], edge[1]] <- 1
}

cat("Finding chromatic number...\n")
chromatic_num <- find_chromatic_number(petersen)
cat("Chromatic number:", chromatic_num, "\n")

result2 <- graph_coloring(petersen, chromatic_num)
cat("Color assignment:", result2$colors, "\n")
cat("Valid:", validate_coloring(petersen, result2$colors), "\n")

# Example 3: Bipartite graph (needs 2 colors)
cat("\n\nExample 3: Bipartite Graph K(3,3)\n")
bipartite <- matrix(c(
  0, 0, 0, 1, 1, 1,
  0, 0, 0, 1, 1, 1,
  0, 0, 0, 1, 1, 1,
  1, 1, 1, 0, 0, 0,
  1, 1, 1, 0, 0, 0,
  1, 1, 1, 0, 0, 0
), nrow = 6, byrow = TRUE)

result3 <- graph_coloring(bipartite, 2)
cat("Backtracking with 2 colors:\n")
cat("Success:", result3$success, "\n")
cat("Color assignment:", result3$colors, "\n")
cat("Valid:", validate_coloring(bipartite, result3$colors), "\n")

# Example 4: Compare algorithms
cat("\n\nExample 4: Algorithm Comparison on Random Graph\n")
set.seed(42)
n <- 8
random_graph <- matrix(0, nrow = n, ncol = n)
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    if (runif(1) < 0.3) {
      random_graph[i, j] <- 1
      random_graph[j, i] <- 1
    }
  }
}

cat("Graph size:", n, "vertices\n")
cat("Number of edges:", sum(random_graph) / 2, "\n\n")

greedy_result <- greedy_coloring(random_graph)
cat("Greedy Coloring:\n")
cat("Colors used:", greedy_result$num_colors_used, "\n")
cat("Color assignment:", greedy_result$colors, "\n")
cat("Valid:", validate_coloring(random_graph, greedy_result$colors), "\n\n")

wp_result <- welsh_powell_coloring(random_graph)
cat("Welsh-Powell Coloring:\n")
cat("Colors used:", wp_result$num_colors_used, "\n")
cat("Color assignment:", wp_result$colors, "\n")
cat("Valid:", validate_coloring(random_graph, wp_result$colors), "\n\n")

chromatic <- find_chromatic_number(random_graph)
cat("Optimal (Backtracking):\n")
cat("Chromatic number:", chromatic, "\n")

optimal_result <- graph_coloring(random_graph, chromatic)
cat("Color assignment:", optimal_result$colors, "\n")
cat("Valid:", validate_coloring(random_graph, optimal_result$colors), "\n")
