# Rotten Oranges Problem - BFS Implementation using Queue
#
# Problem Statement:
# You are given an m x n grid where each cell can have one of three values:
# - 0 representing an empty cell
# - 1 representing a fresh orange
# - 2 representing a rotten orange
#
# Every minute, any fresh orange that is 4-directionally adjacent to a rotten orange becomes rotten.
# Return the minimum number of minutes that must elapse until no cell has a fresh orange.
# If this is impossible, return -1.
#
# Input: grid = [[2,1,1],[1,1,0],[0,1,1]]
# Output: 4
# Explanation:
# Minute 0: [[2,1,1],[1,1,0],[0,1,1]]
# Minute 1: [[2,2,1],[2,1,0],[0,1,1]]
# Minute 2: [[2,2,2],[2,2,0],[0,1,1]]
# Minute 3: [[2,2,2],[2,2,0],[0,2,1]]
# Minute 4: [[2,2,2],[2,2,0],[0,2,2]]
#
# Input: grid = [[2,1,1],[0,1,1],[1,0,1]]
# Output: -1
# Explanation: The orange in the bottom left corner (row 2, column 0) is never rotten,
# because rotting only happens 4-directionally.
#
# Algorithm: Multi-source BFS
# Time Complexity: O(m * n) where m and n are grid dimensions
# Space Complexity: O(m * n) for the queue in worst case

# Simple Queue implementation for BFS
BFSQueue <- setRefClass("BFSQueue",
  fields = list(
    items = "list",
    front = "numeric",
    rear = "numeric"
  ),
  methods = list(
    initialize = function() {
      .self$items <- list()
      .self$front <- 1
      .self$rear <- 0
    },
    
    enqueue = function(item) {
      .self$rear <- .self$rear + 1
      .self$items[[.self$rear]] <- item
    },
    
    dequeue = function() {
      if (.self$is_empty()) return(NULL)
      item <- .self$items[[.self$front]]
      .self$front <- .self$front + 1
      return(item)
    },
    
    is_empty = function() {
      return(.self$front > .self$rear)
    },
    
    size = function() {
      if (.self$is_empty()) return(0)
      return(.self$rear - .self$front + 1)
    }
  )
)

# Main function to solve Rotten Oranges problem
rotten_oranges <- function(grid) {
  if (is.null(grid) || length(grid) == 0 || length(grid[[1]]) == 0) {
    return(0)
  }
  
  rows <- length(grid)
  cols <- length(grid[[1]])
  queue <- BFSQueue$new()
  fresh_count <- 0
  
  # Find all initially rotten oranges and count fresh ones
  for (i in 1:rows) {
    for (j in 1:cols) {
      if (grid[[i]][[j]] == 2) {
        # Add rotten orange position to queue with time 0
        queue$enqueue(list(row = i, col = j, time = 0))
      } else if (grid[[i]][[j]] == 1) {
        fresh_count <- fresh_count + 1
      }
    }
  }
  
  # If no fresh oranges, return 0
  if (fresh_count == 0) return(0)
  
  # Directions for 4-directional movement (up, down, left, right)
  directions <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  max_time <- 0
  
  # BFS to spread the rot
  while (!queue$is_empty()) {
    current <- queue$dequeue()
    row <- current$row
    col <- current$col
    time <- current$time
    
    max_time <- max(max_time, time)
    
    # Check all 4 directions
    for (dir in directions) {
      new_row <- row + dir[1]
      new_col <- col + dir[2]
      
      # Check bounds and if it's a fresh orange
      if (new_row >= 1 && new_row <= rows && 
          new_col >= 1 && new_col <= cols && 
          grid[[new_row]][[new_col]] == 1) {
        
        # Make it rotten
        grid[[new_row]][[new_col]] <- 2
        fresh_count <- fresh_count - 1
        
        # Add to queue with incremented time
        queue$enqueue(list(row = new_row, col = new_col, time = time + 1))
      }
    }
  }
  
  # If there are still fresh oranges, return -1
  if (fresh_count > 0) return(-1)
  
  return(max_time)
}

# Helper function to print grid nicely
print_grid <- function(grid, title = "Grid") {
  cat("\n", title, ":\n")
  for (i in seq_along(grid)) {
    row_str <- paste(grid[[i]], collapse = " ")
    cat("[", row_str, "]\n")
  }
}

# Helper function to create grid from matrix representation
create_grid <- function(matrix_data) {
  rows <- nrow(matrix_data)
  cols <- ncol(matrix_data)
  grid <- list()
  
  for (i in 1:rows) {
    grid[[i]] <- list()
    for (j in 1:cols) {
      grid[[i]][[j]] <- matrix_data[i, j]
    }
  }
  return(grid)
}

# Function to demonstrate the algorithm step by step
demonstrate_rotten_oranges <- function() {
  cat("=== Rotten Oranges Problem - BFS Solution ===\n")
  cat("\nProblem: Given a grid with fresh oranges (1), rotten oranges (2), and empty cells (0),")
  cat("\nfind minimum time for all oranges to rot. Rot spreads 4-directionally each minute.\n")
  
  # Test Case 1: Normal case
  cat("\n--- Test Case 1 ---")
  grid1_matrix <- matrix(c(2,1,1,1,1,0,0,1,1), nrow = 3, byrow = TRUE)
  grid1 <- create_grid(grid1_matrix)
  print_grid(grid1, "Input Grid 1")
  
  result1 <- rotten_oranges(grid1)
  cat("Result: ", result1, " minutes\n")
  cat("Explanation: All oranges can rot in", result1, "minutes\n")
  
  # Test Case 2: Impossible case
  cat("\n--- Test Case 2 ---")
  grid2_matrix <- matrix(c(2,1,1,0,1,1,1,0,1), nrow = 3, byrow = TRUE)
  grid2 <- create_grid(grid2_matrix)
  print_grid(grid2, "Input Grid 2")
  
  result2 <- rotten_oranges(grid2)
  cat("Result: ", result2, "\n")
  cat("Explanation: Some oranges cannot be reached, so return -1\n")
  
  # Test Case 3: All already rotten
  cat("\n--- Test Case 3 ---")
  grid3_matrix <- matrix(c(2,2,2,2,2,2), nrow = 2, byrow = TRUE)
  grid3 <- create_grid(grid3_matrix)
  print_grid(grid3, "Input Grid 3")
  
  result3 <- rotten_oranges(grid3)
  cat("Result: ", result3, " minutes\n")
  cat("Explanation: All oranges already rotten\n")
  
  # Test Case 4: No oranges
  cat("\n--- Test Case 4 ---")
  grid4_matrix <- matrix(c(0,0,0,0,0,0), nrow = 2, byrow = TRUE)
  grid4 <- create_grid(grid4_matrix)
  print_grid(grid4, "Input Grid 4")
  
  result4 <- rotten_oranges(grid4)
  cat("Result: ", result4, " minutes\n")
  cat("Explanation: No oranges to rot\n")
  
  # Test Case 5: Single fresh orange with no rotten ones
  cat("\n--- Test Case 5 ---")
  grid5_matrix <- matrix(c(1), nrow = 1, byrow = TRUE)
  grid5 <- create_grid(grid5_matrix)
  print_grid(grid5, "Input Grid 5")
  
  result5 <- rotten_oranges(grid5)
  cat("Result: ", result5, "\n")
  cat("Explanation: No rotten oranges to start the process\n")
}

# Advanced version with step-by-step visualization
rotten_oranges_with_steps <- function(grid) {
  if (is.null(grid) || length(grid) == 0 || length(grid[[1]]) == 0) {
    return(list(result = 0, steps = list()))
  }
  
  rows <- length(grid)
  cols <- length(grid[[1]])
  queue <- BFSQueue$new()
  fresh_count <- 0
  steps <- list()
  
  # Create a copy for visualization
  working_grid <- grid
  steps[[1]] <- list(time = 0, grid = working_grid, description = "Initial state")
  
  # Find all initially rotten oranges and count fresh ones
  for (i in 1:rows) {
    for (j in 1:cols) {
      if (working_grid[[i]][[j]] == 2) {
        queue$enqueue(list(row = i, col = j, time = 0))
      } else if (working_grid[[i]][[j]] == 1) {
        fresh_count <- fresh_count + 1
      }
    }
  }
  
  if (fresh_count == 0) {
    return(list(result = 0, steps = steps))
  }
  
  directions <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  max_time <- 0
  current_time <- -1
  
  while (!queue$is_empty()) {
    current <- queue$dequeue()
    row <- current$row
    col <- current$col
    time <- current$time
    
    # If we've moved to a new time step, save the grid state
    if (time > current_time) {
      current_time <- time
      if (time > 0) {
        # Deep copy the grid
        step_grid <- list()
        for (i in 1:rows) {
          step_grid[[i]] <- list()
          for (j in 1:cols) {
            step_grid[[i]][[j]] <- working_grid[[i]][[j]]
          }
        }
        steps <- append(steps, list(list(
          time = time, 
          grid = step_grid, 
          description = paste("After", time, "minute(s)")
        )))
      }
    }
    
    max_time <- max(max_time, time)
    
    for (dir in directions) {
      new_row <- row + dir[1]
      new_col <- col + dir[2]
      
      if (new_row >= 1 && new_row <= rows && 
          new_col >= 1 && new_col <= cols && 
          working_grid[[new_row]][[new_col]] == 1) {
        
        working_grid[[new_row]][[new_col]] <- 2
        fresh_count <- fresh_count - 1
        queue$enqueue(list(row = new_row, col = new_col, time = time + 1))
      }
    }
  }
  
  result <- if (fresh_count > 0) -1 else max_time
  return(list(result = result, steps = steps))
}

# Run demonstrations if script is executed directly
if (sys.nframe() == 0) {
  demonstrate_rotten_oranges()
  
  cat("\n\n=== Step-by-Step Visualization ===\n")
  grid_matrix <- matrix(c(2,1,1,1,1,0,0,1,1), nrow = 3, byrow = TRUE)
  grid <- create_grid(grid_matrix)
  
  solution <- rotten_oranges_with_steps(grid)
  
  for (step in solution$steps) {
    cat("\n", step$description, ":\n")
    print_grid(step$grid, "")
  }
  
  cat("\nFinal result:", solution$result, "minutes\n")
}