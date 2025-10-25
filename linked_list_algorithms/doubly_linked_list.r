# Doubly Linked List Implementation in R
#
# A doubly linked list is a linear data structure where each node contains data
# and two references: one to the next node and one to the previous node.
# This bidirectional linking allows traversal in both directions.
#
# Time Complexities:
# - Access: O(n) - must traverse from head or tail
# - Search: O(n) - linear search through nodes
# - Insertion: O(1) at head/tail, O(n) at arbitrary position
# - Deletion: O(1) at head/tail, O(n) at arbitrary position
#
# Space Complexity: O(n) where n is number of elements
#
# Advantages over Singly Linked List:
# - Bidirectional traversal (forward and backward)
# - Easier deletion (no need to track previous node)
# - Better for certain algorithms (e.g., LRU cache)
#
# Disadvantages:
# - Extra memory overhead for storing previous pointer
# - More complex implementation
# - Slightly slower insertion/deletion due to extra pointer updates
#
# Applications:
# - Browser history (back/forward navigation)
# - Undo/Redo operations
# - LRU (Least Recently Used) cache implementation
# - Music player (previous/next song)
# - Navigation in applications

# Define Node class for doubly linked list
DoublyNode <- setRefClass("DoublyNode",
  fields = list(
    data = "ANY",
    next_node = "ANY",
    prev_node = "ANY"
  ),
  methods = list(
    initialize = function(data = NULL, next_node = NULL, prev_node = NULL) {
      .self$data <- data
      .self$next_node <- next_node
      .self$prev_node <- prev_node
    },
    
    print = function() {
      prev_data <- if(is.null(.self$prev_node)) "NULL" else .self$prev_node$data
      next_data <- if(is.null(.self$next_node)) "NULL" else .self$next_node$data
      cat("DoublyNode(prev =", prev_data, ", data =", .self$data, ", next =", next_data, ")\n")
    }
  )
)

# Define Doubly Linked List class
DoublyLinkedList <- setRefClass("DoublyLinkedList",
  fields = list(
    head = "ANY",
    tail = "ANY",
    size = "numeric"
  ),
  methods = list(
    initialize = function() {
      .self$head <- NULL
      .self$tail <- NULL
      .self$size <- 0
    },
    
    # Insert element at the beginning of the list
    insert_at_head = function(data) {
      "Insert a new node at the beginning of the list"
      new_node <- DoublyNode$new(data = data, next_node = .self$head, prev_node = NULL)
      
      if (!is.null(.self$head)) {
        .self$head$prev_node <- new_node
      } else {
        # First node, also becomes tail
        .self$tail <- new_node
      }
      
      .self$head <- new_node
      .self$size <- .self$size + 1
      cat("Inserted", data, "at head. List size:", .self$size, "\n")
    },
    
    # Insert element at the end of the list
    insert_at_tail = function(data) {
      "Insert a new node at the end of the list"
      new_node <- DoublyNode$new(data = data, next_node = NULL, prev_node = .self$tail)
      
      if (!is.null(.self$tail)) {
        .self$tail$next_node <- new_node
      } else {
        # First node, also becomes head
        .self$head <- new_node
      }
      
      .self$tail <- new_node
      .self$size <- .self$size + 1
      cat("Inserted", data, "at tail. List size:", .self$size, "\n")
    },
    
    # Insert element at specified position
    insert_at_position = function(data, position) {
      "Insert a new node at the specified position (0-indexed)"
      if (position < 0 || position > .self$size) {
        stop("Position out of bounds. Valid range: 0 to ", .self$size)
      }
      
      if (position == 0) {
        .self$insert_at_head(data)
        return()
      }
      
      if (position == .self$size) {
        .self$insert_at_tail(data)
        return()
      }
      
      # Determine whether to traverse from head or tail for efficiency
      if (position <= .self$size / 2) {
        # Traverse from head
        current <- .self$head
        for (i in 1:position) {
          current <- current$next_node
        }
      } else {
        # Traverse from tail
        current <- .self$tail
        for (i in 1:(.self$size - position)) {
          current <- current$prev_node
        }
      }
      
      new_node <- DoublyNode$new(data = data, next_node = current, prev_node = current$prev_node)
      current$prev_node$next_node <- new_node
      current$prev_node <- new_node
      .self$size <- .self$size + 1
      cat("Inserted", data, "at position", position, ". List size:", .self$size, "\n")
    },
    
    # Delete element from the beginning
    delete_from_head = function() {
      "Remove and return the first element"
      if (is.null(.self$head)) {
        stop("Cannot delete from empty list")
      }
      
      deleted_data <- .self$head$data
      
      if (.self$size == 1) {
        .self$head <- NULL
        .self$tail <- NULL
      } else {
        .self$head <- .self$head$next_node
        .self$head$prev_node <- NULL
      }
      
      .self$size <- .self$size - 1
      cat("Deleted", deleted_data, "from head. List size:", .self$size, "\n")
      return(deleted_data)
    },
    
    # Delete element from the end
    delete_from_tail = function() {
      "Remove and return the last element"
      if (is.null(.self$tail)) {
        stop("Cannot delete from empty list")
      }
      
      deleted_data <- .self$tail$data
      
      if (.self$size == 1) {
        .self$head <- NULL
        .self$tail <- NULL
      } else {
        .self$tail <- .self$tail$prev_node
        .self$tail$next_node <- NULL
      }
      
      .self$size <- .self$size - 1
      cat("Deleted", deleted_data, "from tail. List size:", .self$size, "\n")
      return(deleted_data)
    },
    
    # Delete element at specified position
    delete_at_position = function(position) {
      "Remove and return element at specified position (0-indexed)"
      if (position < 0 || position >= .self$size) {
        stop("Position out of bounds. Valid range: 0 to ", .self$size - 1)
      }
      
      if (position == 0) {
        return(.self$delete_from_head())
      }
      
      if (position == .self$size - 1) {
        return(.self$delete_from_tail())
      }
      
      # Determine whether to traverse from head or tail for efficiency
      if (position <= .self$size / 2) {
        current <- .self$head
        for (i in 1:position) {
          current <- current$next_node
        }
      } else {
        current <- .self$tail
        for (i in 1:(.self$size - 1 - position)) {
          current <- current$prev_node
        }
      }
      
      deleted_data <- current$data
      current$prev_node$next_node <- current$next_node
      current$next_node$prev_node <- current$prev_node
      .self$size <- .self$size - 1
      cat("Deleted", deleted_data, "at position", position, ". List size:", .self$size, "\n")
      return(deleted_data)
    },
    
    # Delete by value (first occurrence)
    delete_by_value = function(value) {
      "Remove first occurrence of specified value"
      if (is.null(.self$head)) {
        cat("List is empty, nothing to delete\n")
        return(FALSE)
      }
      
      current <- .self$head
      while (!is.null(current)) {
        if (current$data == value) {
          if (identical(current, .self$head)) {
            .self$delete_from_head()
          } else if (identical(current, .self$tail)) {
            .self$delete_from_tail()
          } else {
            current$prev_node$next_node <- current$next_node
            current$next_node$prev_node <- current$prev_node
            .self$size <- .self$size - 1
            cat("Deleted first occurrence of", value, ". List size:", .self$size, "\n")
          }
          return(TRUE)
        }
        current <- current$next_node
      }
      
      cat("Value", value, "not found in list\n")
      return(FALSE)
    },
    
    # Search for an element
    search = function(value) {
      "Search for a value and return its position (0-indexed), -1 if not found"
      current <- .self$head
      position <- 0
      
      while (!is.null(current)) {
        if (current$data == value) {
          cat("Found", value, "at position", position, "\n")
          return(position)
        }
        current <- current$next_node
        position <- position + 1
      }
      
      cat("Value", value, "not found in list\n")
      return(-1)
    },
    
    # Get element at specified position
    get = function(position) {
      "Get element at specified position without removing it"
      if (position < 0 || position >= .self$size) {
        stop("Position out of bounds. Valid range: 0 to ", .self$size - 1)
      }
      
      # Optimize traversal by choosing direction
      if (position <= .self$size / 2) {
        current <- .self$head
        for (i in 1:position) {
          current <- current$next_node
        }
      } else {
        current <- .self$tail
        for (i in 1:(.self$size - 1 - position)) {
          current <- current$prev_node
        }
      }
      
      return(current$data)
    },
    
    # Get the size of the list
    get_size = function() {
      "Return the number of elements in the list"
      return(.self$size)
    },
    
    # Check if the list is empty
    is_empty = function() {
      "Check if the list is empty"
      return(.self$size == 0)
    },
    
    # Convert list to R vector (forward direction)
    to_vector = function() {
      "Convert linked list to R vector"
      if (.self$is_empty()) {
        return(c())
      }
      
      result <- c()
      current <- .self$head
      while (!is.null(current)) {
        result <- c(result, current$data)
        current <- current$next_node
      }
      return(result)
    },
    
    # Convert list to R vector (backward direction)
    to_vector_reverse = function() {
      "Convert linked list to R vector in reverse order"
      if (.self$is_empty()) {
        return(c())
      }
      
      result <- c()
      current <- .self$tail
      while (!is.null(current)) {
        result <- c(result, current$data)
        current <- current$prev_node
      }
      return(result)
    },
    
    # Reverse the linked list
    reverse = function() {
      "Reverse the doubly linked list in place"
      if (.self$is_empty() || .self$size == 1) {
        return()
      }
      
      current <- .self$head
      temp <- NULL
      
      # Swap next and prev for all nodes
      while (!is.null(current)) {
        temp <- current$prev_node
        current$prev_node <- current$next_node
        current$next_node <- temp
        current <- current$prev_node  # Move to next node (was prev due to swap)
      }
      
      # Swap head and tail
      temp <- .self$head
      .self$head <- .self$tail
      .self$tail <- temp
      
      cat("List reversed successfully\n")
    },
    
    # Print the entire list (forward)
    print_list = function() {
      "Print all elements in the list (forward direction)"
      if (.self$is_empty()) {
        cat("List is empty\n")
        return()
      }
      
      cat("Doubly Linked List (Forward): NULL <- ")
      current <- .self$head
      elements <- c()
      
      while (!is.null(current)) {
        elements <- c(elements, current$data)
        current <- current$next_node
      }
      
      cat(paste(elements, collapse = " <-> "), " -> NULL\n")
      cat("Size:", .self$size, "\n")
    },
    
    # Print the entire list (backward)
    print_list_reverse = function() {
      "Print all elements in the list (backward direction)"
      if (.self$is_empty()) {
        cat("List is empty\n")
        return()
      }
      
      cat("Doubly Linked List (Backward): NULL <- ")
      current <- .self$tail
      elements <- c()
      
      while (!is.null(current)) {
        elements <- c(elements, current$data)
        current <- current$prev_node
      }
      
      cat(paste(elements, collapse = " <-> "), " -> NULL\n")
      cat("Size:", .self$size, "\n")
    },
    
    # Clear the entire list
    clear = function() {
      "Remove all elements from the list"
      .self$head <- NULL
      .self$tail <- NULL
      .self$size <- 0
      cat("List cleared\n")
    }
  )
)

# ==============================================================================
# EXAMPLES AND DEMONSTRATIONS
# ==============================================================================

demonstrate_doubly_linked_list <- function() {
  cat("=================================================================\n")
  cat("DOUBLY LINKED LIST - COMPREHENSIVE DEMONSTRATION\n")
  cat("=================================================================\n\n")
  
  # Create a new doubly linked list
  cat("1. CREATING A NEW DOUBLY LINKED LIST\n")
  cat("-----------------------------------------------------------------\n")
  dll <- DoublyLinkedList$new()
  cat("Created empty doubly linked list\n")
  dll$print_list()
  
  cat("\n2. INSERTION OPERATIONS\n")
  cat("-----------------------------------------------------------------\n")
  
  # Insert at head
  cat("Inserting elements at head: 10, 20, 30\n")
  dll$insert_at_head(10)
  dll$insert_at_head(20)
  dll$insert_at_head(30)
  dll$print_list()
  
  # Insert at tail
  cat("\nInserting elements at tail: 5, 1\n")
  dll$insert_at_tail(5)
  dll$insert_at_tail(1)
  dll$print_list()
  
  # Insert at specific position
  cat("\nInserting 15 at position 2\n")
  dll$insert_at_position(15, 2)
  dll$print_list()
  
  cat("\n3. BIDIRECTIONAL TRAVERSAL\n")
  cat("-----------------------------------------------------------------\n")
  
  cat("Forward traversal:\n")
  dll$print_list()
  
  cat("\nBackward traversal:\n")
  dll$print_list_reverse()
  
  cat("\nForward vector:", dll$to_vector(), "\n")
  cat("Backward vector:", dll$to_vector_reverse(), "\n")
  
  cat("\n4. ACCESS AND SEARCH OPERATIONS\n")
  cat("-----------------------------------------------------------------\n")
  
  # Search for elements
  cat("Searching for elements:\n")
  dll$search(15)
  dll$search(99)
  
  # Get elements by position (optimized traversal)
  cat("\nAccessing elements by position (optimized traversal):\n")
  cat("Element at position 0:", dll$get(0), "\n")
  cat("Element at position", dll$get_size() - 1, ":", dll$get(dll$get_size() - 1), "\n")
  
  cat("\n5. DELETION OPERATIONS\n")
  cat("-----------------------------------------------------------------\n")
  
  # Delete from head
  cat("Deleting from head:\n")
  deleted <- dll$delete_from_head()
  cat("Deleted value:", deleted, "\n")
  dll$print_list()
  
  # Delete from tail
  cat("\nDeleting from tail:\n")
  deleted <- dll$delete_from_tail()
  cat("Deleted value:", deleted, "\n")
  dll$print_list()
  
  # Delete by value
  cat("\nDeleting by value (15):\n")
  dll$delete_by_value(15)
  dll$print_list()
  
  cat("\n6. ADVANCED OPERATIONS\n")
  cat("-----------------------------------------------------------------\n")
  
  # Add more elements for demonstration
  cat("Adding more elements for advanced operations:\n")
  dll$insert_at_tail(100)
  dll$insert_at_tail(200)
  dll$insert_at_tail(300)
  dll$print_list()
  
  # Reverse the list
  cat("\nReversing the list:\n")
  dll$reverse()
  dll$print_list()
  cat("After reverse - backward traversal:\n")
  dll$print_list_reverse()
  
  cat("\n7. PRACTICAL EXAMPLE: BROWSER HISTORY\n")
  cat("-----------------------------------------------------------------\n")
  
  # Simulate browser history using doubly linked list
  browser_history <- DoublyLinkedList$new()
  
  cat("Simulating browser history navigation:\n")
  websites <- c("google.com", "github.com", "stackoverflow.com", "reddit.com")
  
  cat("Visiting websites:\n")
  for (site in websites) {
    browser_history$insert_at_tail(site)
    cat("Visited:", site, "\n")
  }
  
  cat("\nCurrent history (chronological order):\n")
  browser_history$print_list()
  
  cat("\nCurrent history (reverse chronological order):\n")
  browser_history$print_list_reverse()
  
  # Simulate going back
  cat("\nGoing back (removing current page):\n")
  current_page <- browser_history$delete_from_tail()
  cat("Left page:", current_page, "\n")
  cat("Now at:", browser_history$get(browser_history$get_size() - 1), "\n")
  
  # Add new page (forward navigation)
  cat("\nNavigating to new page: wikipedia.org\n")
  browser_history$insert_at_tail("wikipedia.org")
  browser_history$print_list()
  
  cat("\n8. PERFORMANCE COMPARISON\n")
  cat("-----------------------------------------------------------------\n")
  
  cat("Doubly linked list advantages demonstrated:\n")
  cat("- Bidirectional traversal: O(1) access to both head and tail\n")
  cat("- Optimized position access: traverse from closer end\n")
  cat("- Easier deletion: no need to track previous node\n")
  
  # Demonstrate optimized access
  cat("\nOptimized access example (large list):\n")
  large_dll <- DoublyLinkedList$new()
  for (i in 1:10) {
    large_dll$insert_at_tail(i * 10)
  }
  
  cat("Accessing element near beginning (position 2):\n")
  cat("Element:", large_dll$get(2), "(traversed from head)\n")
  
  cat("Accessing element near end (position 8):\n")
  cat("Element:", large_dll$get(8), "(traversed from tail)\n")
  
  cat("\n=================================================================\n")
  cat("END OF DOUBLY LINKED LIST DEMONSTRATION\n")
  cat("=================================================================\n")
}

# Helper function to create a doubly linked list from vector
create_doubly_list_from_vector <- function(vector_data) {
  "Create a doubly linked list from an R vector"
  new_list <- DoublyLinkedList$new()
  for (element in vector_data) {
    new_list$insert_at_tail(element)
  }
  return(new_list)
}

# Examples are available but not run automatically to avoid side effects
# To run examples, execute: demonstrate_doubly_linked_list()
if (interactive()) {
  cat("Loading Doubly Linked List implementation...\n")
  cat("Run 'demonstrate_doubly_linked_list()' to see comprehensive examples.\n")
  cat("Use 'DoublyLinkedList$new()' to create a new doubly linked list.\n")
}

# Uncomment the following line to run examples automatically:
# demonstrate_doubly_linked_list()