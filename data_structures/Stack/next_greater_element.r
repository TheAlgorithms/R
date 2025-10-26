# Next Greater Element Problem - Stack Implementation
#
# Problem Statement:
# Given an array of integers, for each element find the next greater element to its right.
# The next greater element for an element x is the first greater element on the right side of x.
# If no greater element exists, return -1 for that element.
#
# Examples:
# Input: [4, 5, 2, 25]
# Output: [5, 25, 25, -1]
# Explanation: 
# - For 4, next greater is 5
# - For 5, next greater is 25
# - For 2, next greater is 25  
# - For 25, no greater element exists
#
# Input: [13, 7, 6, 12]
# Output: [-1, 12, 12, -1]
#
# Input: [1, 3, 2, 4]
# Output: [3, 4, 4, -1]
#
# Algorithm: Stack-based O(n) solution
# Time Complexity: O(n) - each element is pushed and popped at most once
# Space Complexity: O(n) - for the stack in worst case (decreasing sequence)

# Simple Stack implementation for algorithm use
AlgorithmStack <- setRefClass("AlgorithmStack",
  fields = list(
    items = "list",
    top_idx = "numeric"
  ),
  methods = list(
    initialize = function() {
      .self$items <- list()
      .self$top_idx <- 0
    },
    
    push = function(item) {
      .self$top_idx <- .self$top_idx + 1
      .self$items[[.self$top_idx]] <- item
    },
    
    pop = function() {
      if (.self$top_idx == 0) return(NULL)
      item <- .self$items[[.self$top_idx]]
      .self$top_idx <- .self$top_idx - 1
      return(item)
    },
    
    peek = function() {
      if (.self$top_idx == 0) return(NULL)
      return(.self$items[[.self$top_idx]])
    },
    
    is_empty = function() {
      return(.self$top_idx == 0)
    },
    
    size = function() {
      return(.self$top_idx)
    }
  )
)

# Main function to find next greater elements
next_greater_element <- function(arr) {
  "Find next greater element for each element in array using stack"
  if (length(arr) == 0) return(c())
  
  n <- length(arr)
  result <- rep(-1, n)  # Initialize all with -1
  stack <- AlgorithmStack$new()
  
  # Process each element from left to right
  for (i in 1:n) {
    # While stack is not empty and current element is greater than
    # the element at index stored at top of stack
    while (!stack$is_empty() && arr[i] > arr[stack$peek()]) {
      index <- stack$pop()
      result[index] <- arr[i]
    }
    
    # Push current element's index to stack
    stack$push(i)
  }
  
  return(result)
}

# Enhanced version with step-by-step visualization
next_greater_element_with_steps <- function(arr) {
  "Find next greater elements with detailed step-by-step visualization"
  if (length(arr) == 0) return(list(result = c(), steps = list()))
  
  n <- length(arr)
  result <- rep(-1, n)
  stack <- AlgorithmStack$new()
  steps <- list()
  
  # Initial state
  steps[[1]] <- list(
    step = 0,
    current_element = "Start",
    array = arr,
    stack_contents = c(),
    result = result,
    description = "Initial state"
  )
  
  for (i in 1:n) {
    step_description <- paste("Processing element", arr[i], "at index", i)
    
    # Pop elements and update result
    popped_elements <- c()
    while (!stack$is_empty() && arr[i] > arr[stack$peek()]) {
      index <- stack$pop()
      result[index] <- arr[i]
      popped_elements <- c(popped_elements, index)
    }
    
    if (length(popped_elements) > 0) {
      step_description <- paste(step_description, "| Found NGE for indices:", paste(popped_elements, collapse = ", "))
    }
    
    # Push current index
    stack$push(i)
    
    # Get current stack contents for visualization
    stack_contents <- c()
    if (!stack$is_empty()) {
      for (j in 1:stack$size()) {
        if (j <= length(stack$items)) {
          stack_contents <- c(stack_contents, stack$items[[j]])
        }
      }
    }
    
    # Record step
    steps[[i + 1]] <- list(
      step = i,
      current_element = arr[i],
      array = arr,
      stack_contents = stack_contents,
      result = result,
      description = step_description
    )
  }
  
  return(list(result = result, steps = steps))
}

# Helper function to print array nicely
print_array <- function(arr, title = "Array") {
  cat(title, ": [", paste(arr, collapse = ", "), "]\n")
}

# Helper function to print step visualization
print_step <- function(step_info) {
  cat("\nStep", step_info$step, ":", step_info$description, "\n")
  cat("Current element:", step_info$current_element, "\n")
  print_array(step_info$array, "Input")
  
  if (length(step_info$stack_contents) > 0) {
    stack_values <- sapply(step_info$stack_contents, function(idx) paste0(step_info$array[idx], "(", idx, ")"))
    cat("Stack (indices): [", paste(stack_values, collapse = ", "), "]\n")
  } else {
    cat("Stack: [empty]\n")
  }
  
  print_array(step_info$result, "Result so far")
}

# Next Greater Element to the Right for Circular Array
next_greater_element_circular <- function(arr) {
  "Find next greater elements in circular array (wrapping around)"
  if (length(arr) == 0) return(c())
  
  n <- length(arr)
  result <- rep(-1, n)
  stack <- AlgorithmStack$new()
  
  # Process the array twice to handle circular nature
  for (i in 1:(2 * n)) {
    current_index <- ((i - 1) %% n) + 1  # Convert to 1-based circular index
    
    while (!stack$is_empty() && arr[current_index] > arr[stack$peek()]) {
      index <- stack$pop()
      if (result[index] == -1) {  # Only update if not already found
        result[index] <- arr[current_index]
      }
    }
    
    if (i <= n) {  # Only push indices in first pass
      stack$push(current_index)
    }
  }
  
  return(result)
}

# Previous Greater Element (using stack)
previous_greater_element <- function(arr) {
  "Find previous greater element for each element"
  if (length(arr) == 0) return(c())
  
  n <- length(arr)
  result <- rep(-1, n)
  stack <- AlgorithmStack$new()
  
  # Process from left to right
  for (i in 1:n) {
    # Remove smaller or equal elements
    while (!stack$is_empty() && arr[stack$peek()] <= arr[i]) {
      stack$pop()
    }
    
    # If stack is not empty, top element is previous greater
    if (!stack$is_empty()) {
      result[i] <- arr[stack$peek()]
    }
    
    stack$push(i)
  }
  
  return(result)
}

# Stock Span Problem using Stack
stock_span <- function(prices) {
  "Calculate stock span for each day (consecutive previous days with price <= current day)"
  if (length(prices) == 0) return(c())
  
  n <- length(prices)
  spans <- rep(1, n)  # Initialize all spans to 1
  stack <- AlgorithmStack$new()
  
  for (i in 1:n) {
    # Pop elements while stack is not empty and 
    # price at stack top is less than or equal to current price
    while (!stack$is_empty() && prices[stack$peek()] <= prices[i]) {
      stack$pop()
    }
    
    # If stack becomes empty, span is i (all previous days)
    # Otherwise, span is difference between current index and index at stack top
    spans[i] <- if (stack$is_empty()) i else (i - stack$peek())
    
    # Push current index to stack
    stack$push(i)
  }
  
  return(spans)
}

# Largest Rectangle in Histogram using Stack
largest_rectangle_histogram <- function(heights) {
  "Find the largest rectangle area in histogram using stack"
  if (length(heights) == 0) return(0)
  
  n <- length(heights)
  stack <- AlgorithmStack$new()
  max_area <- 0
  
  for (i in 1:n) {
    # While stack is not empty and current height is less than
    # height at stack top, calculate area with stack top as smallest bar
    while (!stack$is_empty() && heights[i] < heights[stack$peek()]) {
      height <- heights[stack$pop()]
      width <- if (stack$is_empty()) i - 1 else i - stack$peek() - 1
      area <- height * width
      max_area <- max(max_area, area)
    }
    stack$push(i)
  }
  
  # Process remaining bars in stack
  while (!stack$is_empty()) {
    height <- heights[stack$pop()]
    width <- if (stack$is_empty()) n else n - stack$peek()
    area <- height * width
    max_area <- max(max_area, area)
  }
  
  return(max_area)
}

# Function to demonstrate all stack applications
demonstrate_stack_applications <- function() {
  cat("=== Stack Applications - Problem Solving ===\n")
  
  # Test Case 1: Next Greater Element
  cat("\n--- Next Greater Element Problem ---\n")
  test_arrays <- list(
    c(4, 5, 2, 25),
    c(13, 7, 6, 12),
    c(1, 3, 2, 4),
    c(5, 4, 3, 2, 1),
    c(1, 2, 3, 4, 5)
  )
  
  for (i in seq_along(test_arrays)) {
    arr <- test_arrays[[i]]
    result <- next_greater_element(arr)
    cat("\nTest", i, ":\n")
    print_array(arr, "Input")
    print_array(result, "Next Greater")
  }
  
  # Test Case 2: Circular Array
  cat("\n--- Next Greater Element (Circular Array) ---\n")
  circular_test <- c(1, 2, 1)
  print_array(circular_test, "Input")
  circular_result <- next_greater_element_circular(circular_test)
  print_array(circular_result, "Next Greater (Circular)")
  
  # Test Case 3: Previous Greater Element
  cat("\n--- Previous Greater Element ---\n")
  prev_test <- c(4, 5, 2, 25, 7, 8)
  print_array(prev_test, "Input")
  prev_result <- previous_greater_element(prev_test)
  print_array(prev_result, "Previous Greater")
  
  # Test Case 4: Stock Span Problem
  cat("\n--- Stock Span Problem ---\n")
  stock_prices <- c(100, 80, 60, 70, 60, 75, 85)
  print_array(stock_prices, "Stock Prices")
  span_result <- stock_span(stock_prices)
  print_array(span_result, "Stock Spans")
  cat("Explanation: Span[i] = number of consecutive days (including current) with price <= price[i]\n")
  
  # Test Case 5: Largest Rectangle in Histogram
  cat("\n--- Largest Rectangle in Histogram ---\n")
  histogram_heights <- c(6, 2, 5, 4, 5, 1, 6)
  print_array(histogram_heights, "Histogram Heights")
  max_area <- largest_rectangle_histogram(histogram_heights)
  cat("Largest Rectangle Area:", max_area, "\n")
}

# Detailed step-by-step demonstration
demonstrate_detailed_steps <- function() {
  cat("\n\n=== Detailed Step-by-Step: Next Greater Element ===\n")
  
  test_array <- c(4, 5, 2, 25)
  cat("\nSolving for array:", paste(test_array, collapse = ", "), "\n")
  cat("Algorithm: Use stack to store indices of elements for which NGE is not found yet\n")
  
  solution <- next_greater_element_with_steps(test_array)
  
  for (step in solution$steps) {
    print_step(step)
  }
  
  cat("\nFinal Result:", paste(solution$result, collapse = ", "), "\n")
  cat("\nTime Complexity: O(n) - each element pushed and popped at most once\n")
  cat("Space Complexity: O(n) - for the stack in worst case\n")
}

# Run demonstrations if script is executed directly
if (sys.nframe() == 0) {
  demonstrate_stack_applications()
  demonstrate_detailed_steps()
}