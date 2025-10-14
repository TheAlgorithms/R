# Jump Search Algorithm Implementation in R
# An efficient search algorithm for sorted arrays
# Works by jumping ahead by fixed steps then performing linear search
# Time complexity: O(√n) where n is the array length
# Space complexity: O(1)

library(R6)

#' JumpSearch Class
#' @description R6 class implementing the Jump Search algorithm
#' @details Jump Search is a searching algorithm for sorted arrays that works by:
#' 1. Dividing the array into blocks of size √n
#' 2. Jumping ahead by √n steps until finding a block where target might be
#' 3. Performing linear search within that block
#' Advantages:
#' - Better than linear search: O(√n) vs O(n)
#' - Better for systems with slow backward iteration compared to binary search
#' - Simple implementation
#' Limitations:
#' - Requires sorted array
#' - Slower than binary search: O(√n) vs O(log n)
JumpSearch <- R6Class(
  "JumpSearch",
  
  public = list(
    #' @description Initialize Jump Search
    #' @param data Sorted array to search in
    #' @param validate_sorted Whether to validate if array is sorted
    initialize = function(data = NULL, validate_sorted = TRUE) {
      if (!is.null(data)) {
        private$validate_input(data, validate_sorted)
        self$data <- data
        private$n <- length(data)
        private$optimal_jump <- floor(sqrt(private$n))
      }
      invisible(self)
    },
    
    #' @description Search for a target value in the array
    #' @param target Value to search for
    #' @param jump_size Optional custom jump size (default: √n)
    #' @return List containing index, number of comparisons, and success status
    search = function(target, jump_size = NULL) {
      if (is.null(self$data)) {
        stop("No data available. Please initialize with data first.")
      }
      
      # Use custom jump size or optimal jump size
      step <- if (is.null(jump_size)) private$optimal_jump else jump_size
      
      if (step <= 0 || step != round(step)) {
        stop("Jump size must be a positive integer")
      }
      
      prev <- 0
      comparisons <- 0
      
      # Jump through array to find the block where target might be
      while (step <= private$n && self$data[step] < target) {
        comparisons <- comparisons + 1
        prev <- step
        step <- step + private$optimal_jump
      }
      
      # If we've jumped past the end, adjust step to not exceed n
      if (prev >= private$n) {
        return(list(
          index = -1,
          comparisons = comparisons,
          found = FALSE
        ))
      }
      
      # Linear search in the identified block
      while ((prev + 1) <= private$n && self$data[prev + 1] <= target) {
        comparisons <- comparisons + 1
        prev <- prev + 1
        
        if (self$data[prev] == target) {
          return(list(
            index = prev,
            comparisons = comparisons,
            found = TRUE
          ))
        }
      }
      
      # Target not found
      return(list(
        index = -1,
        comparisons = comparisons,
        found = FALSE
      ))
    },
    
    #' @description Search for multiple targets
    #' @param targets Vector of values to search for
    #' @return List of search results for each target
    search_multiple = function(targets) {
      if (!is.numeric(targets)) {
        stop("Targets must be numeric")
      }
      
      results <- list()
      for (i in seq_along(targets)) {
        results[[i]] <- self$search(targets[i])
        results[[i]]$target <- targets[i]
      }
      return(results)
    },
    
    #' @description Find the optimal jump size for the current data
    #' @return Optimal jump size (√n)
    get_optimal_jump_size = function() {
      if (is.null(self$data)) {
        stop("No data available. Please initialize with data first.")
      }
      return(private$optimal_jump)
    },
    
    #' @description Compare performance with different jump sizes
    #' @param target Value to search for
    #' @param jump_sizes Vector of jump sizes to test
    #' @return Data frame with performance comparison
    compare_jump_sizes = function(target, jump_sizes = NULL) {
      if (is.null(self$data)) {
        stop("No data available. Please initialize with data first.")
      }
      
      if (is.null(jump_sizes)) {
        jump_sizes <- c(
          floor(sqrt(private$n) / 2),
          private$optimal_jump,
          floor(sqrt(private$n) * 2)
        )
      }
      
      results <- data.frame(
        jump_size = numeric(),
        comparisons = numeric(),
        found = logical(),
        stringsAsFactors = FALSE
      )
      
      for (size in jump_sizes) {
        if (size > 0 && size <= private$n) {
          result <- self$search(target, jump_size = size)
          results <- rbind(results, data.frame(
            jump_size = size,
            comparisons = result$comparisons,
            found = result$found
          ))
        }
      }
      
      return(results)
    },
    
    #' @description Update the data array
    #' @param new_data New sorted array
    #' @param validate_sorted Whether to validate if array is sorted
    update_data = function(new_data, validate_sorted = TRUE) {
      private$validate_input(new_data, validate_sorted)
      self$data <- new_data
      private$n <- length(new_data)
      private$optimal_jump <- floor(sqrt(private$n))
      invisible(self)
    },
    
    # Public fields
    data = NULL
  ),
  
  private = list(
    n = NULL,
    optimal_jump = NULL,
    
    validate_input = function(data, check_sorted) {
      if (!is.numeric(data)) {
        stop("Input data must be numeric")
      }
      if (any(is.na(data))) {
        stop("Input data contains missing values")
      }
      if (length(data) == 0) {
        stop("Input data cannot be empty")
      }
      if (check_sorted && !private$is_sorted(data)) {
        stop("Input data must be sorted in ascending order")
      }
    },
    
    is_sorted = function(data) {
      all(diff(data) >= 0)
    }
  )
)

# Demonstration
demonstrate_jump_search <- function() {
  cat("=== Jump Search Algorithm Demo ===\n\n")
  
  # Example 1: Basic usage
  cat("Example 1: Basic jump search\n")
  data <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29)
  cat("Sorted array:", paste(data, collapse = ", "), "\n")
  
  js <- JumpSearch$new(data)
  cat(sprintf("Array size: %d, Optimal jump size: %d\n\n", 
              length(data), js$get_optimal_jump_size()))
  
  targets <- c(7, 19, 30)
  for (target in targets) {
    result <- js$search(target)
    if (result$found) {
      cat(sprintf("Target %d found at index %d (comparisons: %d)\n", 
                  target, result$index, result$comparisons))
    } else {
      cat(sprintf("Target %d not found (comparisons: %d)\n", 
                  target, result$comparisons))
    }
  }
  
  # Example 2: Larger dataset
  cat("\nExample 2: Larger dataset\n")
  set.seed(42)
  large_data <- sort(sample(1:1000, 100))
  js2 <- JumpSearch$new(large_data)
  
  cat(sprintf("Array size: %d, Optimal jump size: %d\n", 
              length(large_data), js2$get_optimal_jump_size()))
  
  search_targets <- c(large_data[25], large_data[75], 999)
  results <- js2$search_multiple(search_targets)
  
  cat("\nMultiple search results:\n")
  for (i in seq_along(results)) {
    res <- results[[i]]
    cat(sprintf("Target %d: %s (index: %d, comparisons: %d)\n",
                res$target,
                ifelse(res$found, "Found", "Not found"),
                res$index,
                res$comparisons))
  }
  
  # Example 3: Jump size comparison
  cat("\nExample 3: Comparing different jump sizes\n")
  test_data <- 1:100
  js3 <- JumpSearch$new(test_data)
  target <- 87
  
  comparison <- js3$compare_jump_sizes(target)
  cat(sprintf("Searching for %d in array of size %d:\n\n", target, length(test_data)))
  print(comparison)
  
  # Example 4: Performance analysis
  cat("\nExample 4: Performance analysis\n")
  sizes <- c(100, 1000, 10000)
  
  cat("Average comparisons for different array sizes:\n")
  for (n in sizes) {
    test_array <- 1:n
    js_test <- JumpSearch$new(test_array)
    
    # Test multiple searches
    num_tests <- 20
    total_comps <- 0
    for (i in 1:num_tests) {
      target <- sample(test_array, 1)
      result <- js_test$search(target)
      total_comps <- total_comps + result$comparisons
    }
    
    avg_comps <- total_comps / num_tests
    theoretical_bound <- sqrt(n)
    
    cat(sprintf("n = %5d: Avg comparisons = %.1f, Theoretical O(√n) = %.1f\n",
                n, avg_comps, theoretical_bound))
  }
  
  cat("\n=== Demo Complete ===\n")
}

# Run demonstration if not in interactive mode
if (!interactive()) {
  demonstrate_jump_search()
}