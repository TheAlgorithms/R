# Queue Data Structure Implementation in R
#
# A queue is a linear data structure that follows the First In First Out (FIFO) principle.
# Elements are added at the rear (enqueue) and removed from the front (dequeue).
#
# Time Complexities:
# - Enqueue: O(1) - adding element to rear
# - Dequeue: O(1) - removing element from front
# - Peek/Front: O(1) - viewing front element
# - Size: O(1) - getting queue size
# - IsEmpty: O(1) - checking if queue is empty
#
# Space Complexity: O(n) where n is number of elements
#
# Applications:
# - CPU scheduling and process management
# - Breadth-First Search (BFS) in graphs
# - Handling requests in web servers
# - Print queue management
# - Level order traversal in trees
# - Buffer for data streams

# Define Queue class using Reference Classes
Queue <- setRefClass("Queue",
  fields = list(
    items = "list",
    front_index = "numeric",
    rear_index = "numeric",
    max_size = "numeric"
  ),
  methods = list(
    initialize = function(max_size = Inf) {
      "Initialize an empty queue with optional maximum size"
      .self$items <- list()
      .self$front_index <- 1
      .self$rear_index <- 0
      .self$max_size <- max_size
      cat("Queue initialized with max size:", ifelse(is.infinite(max_size), "unlimited", max_size), "\n")
    },
    
    enqueue = function(item) {
      "Add an element to the rear of the queue"
      if (.self$size() >= .self$max_size) {
        stop("Queue overflow: Cannot add more elements. Max size reached: ", .self$max_size)
      }
      
      .self$rear_index <- .self$rear_index + 1
      .self$items[[.self$rear_index]] <- item
      cat("Enqueued:", item, "| Size:", .self$size(), "\n")
    },
    
    dequeue = function() {
      "Remove and return the front element from the queue"
      if (.self$is_empty()) {
        stop("Queue underflow: Cannot dequeue from empty queue")
      }
      
      item <- .self$items[[.self$front_index]]
      .self$items[[.self$front_index]] <- NULL
      .self$front_index <- .self$front_index + 1
      
      # Reset indices when queue becomes empty to optimize memory
      if (.self$is_empty()) {
        .self$front_index <- 1
        .self$rear_index <- 0
        .self$items <- list()
      }
      
      cat("Dequeued:", item, "| Size:", .self$size(), "\n")
      return(item)
    },
    
    front = function() {
      "Return the front element without removing it"
      if (.self$is_empty()) {
        stop("Queue is empty: No front element")
      }
      return(.self$items[[.self$front_index]])
    },
    
    rear = function() {
      "Return the rear element without removing it"
      if (.self$is_empty()) {
        stop("Queue is empty: No rear element")
      }
      return(.self$items[[.self$rear_index]])
    },
    
    is_empty = function() {
      "Check if the queue is empty"
      return(.self$rear_index < .self$front_index)
    },
    
    is_full = function() {
      "Check if the queue is full (only applicable for bounded queues)"
      return(.self$size() >= .self$max_size)
    },
    
    size = function() {
      "Return the number of elements in the queue"
      if (.self$is_empty()) return(0)
      return(.self$rear_index - .self$front_index + 1)
    },
    
    clear = function() {
      "Remove all elements from the queue"
      .self$items <- list()
      .self$front_index <- 1
      .self$rear_index <- 0
      cat("Queue cleared\n")
    },
    
    display = function() {
      "Display all elements in the queue from front to rear"
      if (.self$is_empty()) {
        cat("Queue is empty: []\n")
        return()
      }
      
      elements <- character(0)
      for (i in .self$front_index:.self$rear_index) {
        if (i <= length(.self$items) && !is.null(.self$items[[i]])) {
          elements <- c(elements, as.character(.self$items[[i]]))
        }
      }
      cat("Queue: [", paste(elements, collapse = " <- "), "] (front <- rear)\n")
    },
    
    to_vector = function() {
      "Convert queue to vector (front to rear order)"
      if (.self$is_empty()) return(c())
      
      result <- c()
      for (i in .self$front_index:.self$rear_index) {
        result <- c(result, .self$items[[i]])
      }
      return(result)
    },
    
    search = function(item) {
      "Search for an item in the queue and return its position from front (1-indexed)"
      if (.self$is_empty()) return(-1)
      
      for (i in .self$front_index:.self$rear_index) {
        if (identical(.self$items[[i]], item)) {
          return(i - .self$front_index + 1)
        }
      }
      return(-1)  # Item not found
    }
  )
)

# Utility function to demonstrate queue operations
demonstrate_queue_operations <- function() {
  cat("\n=== Queue Data Structure Demonstration ===\n\n")
  
  # Create a queue with maximum size of 5
  q <- Queue$new(max_size = 5)
  
  cat("\n1. Basic Enqueue Operations:\n")
  q$enqueue("A")
  q$enqueue("B")
  q$enqueue("C")
  q$display()
  
  cat("\n2. Queue Status:\n")
  cat("Size:", q$size(), "\n")
  cat("Is Empty:", q$is_empty(), "\n")
  cat("Is Full:", q$is_full(), "\n")
  cat("Front element:", q$front(), "\n")
  cat("Rear element:", q$rear(), "\n")
  
  cat("\n3. Dequeue Operations:\n")
  q$dequeue()
  q$display()
  q$dequeue()
  q$display()
  
  cat("\n4. More Enqueue Operations:\n")
  q$enqueue("D")
  q$enqueue("E")
  q$enqueue("F")
  q$enqueue("G")
  q$display()
  
  cat("\n5. Search Operations:\n")
  cat("Position of 'E':", q$search("E"), "\n")
  cat("Position of 'Z':", q$search("Z"), "\n")
  
  cat("\n6. Queue to Vector:\n")
  vec <- q$to_vector()
  cat("As vector:", paste(vec, collapse = ", "), "\n")
  
  cat("\n7. Testing Queue Overflow:\n")
  tryCatch({
    q$enqueue("H")  # This should cause overflow
  }, error = function(e) {
    cat("Error caught:", e$message, "\n")
  })
  
  cat("\n8. Clear Queue:\n")
  q$clear()
  q$display()
  
  cat("\n9. Testing Queue Underflow:\n")
  tryCatch({
    q$dequeue()  # This should cause underflow
  }, error = function(e) {
    cat("Error caught:", e$message, "\n")
  })
}

# Priority Queue implementation (bonus)
PriorityQueue <- setRefClass("PriorityQueue",
  fields = list(
    items = "list",
    priorities = "numeric"
  ),
  methods = list(
    initialize = function() {
      "Initialize an empty priority queue"
      .self$items <- list()
      .self$priorities <- numeric(0)
      cat("Priority Queue initialized\n")
    },
    
    enqueue = function(item, priority = 0) {
      "Add an element with priority (higher number = higher priority)"
      .self$items <- append(.self$items, list(item))
      .self$priorities <- append(.self$priorities, priority)
      cat("Enqueued:", item, "with priority:", priority, "\n")
    },
    
    dequeue = function() {
      "Remove and return the highest priority element"
      if (length(.self$items) == 0) {
        stop("Priority queue is empty")
      }
      
      max_priority_index <- which.max(.self$priorities)
      item <- .self$items[[max_priority_index]]
      priority <- .self$priorities[max_priority_index]
      
      .self$items <- .self$items[-max_priority_index]
      .self$priorities <- .self$priorities[-max_priority_index]
      
      cat("Dequeued:", item, "with priority:", priority, "\n")
      return(list(item = item, priority = priority))
    },
    
    is_empty = function() {
      "Check if priority queue is empty"
      return(length(.self$items) == 0)
    },
    
    size = function() {
      "Return number of elements"
      return(length(.self$items))
    },
    
    display = function() {
      "Display all elements with their priorities"
      if (.self$is_empty()) {
        cat("Priority Queue is empty\n")
        return()
      }
      
      cat("Priority Queue:\n")
      for (i in seq_along(.self$items)) {
        cat("  ", .self$items[[i]], "(priority:", .self$priorities[i], ")\n")
      }
    }
  )
)

# Example usage and testing
if (sys.nframe() == 0) {
  # Demonstrate basic queue operations
  demonstrate_queue_operations()
  
  cat("\n\n=== Priority Queue Demonstration ===\n")
  pq <- PriorityQueue$new()
  pq$enqueue("Low priority task", 1)
  pq$enqueue("High priority task", 5)
  pq$enqueue("Medium priority task", 3)
  pq$display()
  
  cat("\nDequeuing in priority order:\n")
  while (!pq$is_empty()) {
    pq$dequeue()
  }
}