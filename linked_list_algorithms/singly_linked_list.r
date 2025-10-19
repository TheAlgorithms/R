# Singly Linked List Implementation in R
#
# A singly linked list is a linear data structure where each element (node) contains
# data and a reference (pointer) to the next node in the sequence. Unlike arrays,
# linked lists don't store elements in contiguous memory locations.
#
# Time Complexities:
# - Access: O(n) - must traverse from head to reach element
# - Search: O(n) - linear search through nodes
# - Insertion: O(1) at head, O(n) at arbitrary position
# - Deletion: O(1) at head, O(n) at arbitrary position
#
# Space Complexity: O(n) where n is number of elements
#
# Advantages:
# - Dynamic size (can grow/shrink during runtime)
# - Efficient insertion/deletion at beginning
# - Memory efficient (allocates memory as needed)
#
# Disadvantages:
# - No random access (must traverse from head)
# - Extra memory overhead for storing pointers
# - Not cache-friendly due to non-contiguous memory
#
# Applications:
# - Implementation of stacks and queues
# - Undo functionality in applications
# - Music playlist (next song)
# - Browser history navigation

# Define Node class for singly linked list
SinglyNode <- setRefClass("SinglyNode",
  fields = list(
    data = "ANY",
    next_node = "ANY"
  ),
  methods = list(
    initialize = function(data = NULL, next_node = NULL) {
      .self$data <- data
      .self$next_node <- next_node
    },
    
    print = function() {
      cat("Node(data =", .self$data, ")\n")
    }
  )
)

# Define Singly Linked List class
SinglyLinkedList <- setRefClass("SinglyLinkedList",
  fields = list(
    head = "ANY",
    size = "numeric"
  ),
  methods = list(
    initialize = function() {
      .self$head <- NULL
      .self$size <- 0
    },
    
    # Insert element at the beginning of the list
    insert_at_head = function(data) {
      "Insert a new node at the beginning of the list"
      new_node <- SinglyNode$new(data = data, next_node = .self$head)
      .self$head <- new_node
      .self$size <- .self$size + 1
      cat("Inserted", data, "at head. List size:", .self$size, "\n")
    },
    
    # Insert element at the end of the list
    insert_at_tail = function(data) {
      "Insert a new node at the end of the list"
      new_node <- SinglyNode$new(data = data, next_node = NULL)
      
      if (is.null(.self$head)) {
        .self$head <- new_node
      } else {
        current <- .self$head
        while (!is.null(current$next_node)) {
          current <- current$next_node
        }
        current$next_node <- new_node
      }
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
      
      new_node <- SinglyNode$new(data = data)
      current <- .self$head
      
      # Traverse to position - 1
      for (i in 1:(position - 1)) {
        current <- current$next_node
      }
      
      new_node$next_node <- current$next_node
      current$next_node <- new_node
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
      .self$head <- .self$head$next_node
      .self$size <- .self$size - 1
      cat("Deleted", deleted_data, "from head. List size:", .self$size, "\n")
      return(deleted_data)
    },
    
    # Delete element from the end
    delete_from_tail = function() {
      "Remove and return the last element"
      if (is.null(.self$head)) {
        stop("Cannot delete from empty list")
      }
      
      if (is.null(.self$head$next_node)) {
        # Only one element
        deleted_data <- .self$head$data
        .self$head <- NULL
        .self$size <- .self$size - 1
        cat("Deleted", deleted_data, "from tail. List size:", .self$size, "\n")
        return(deleted_data)
      }
      
      # Find second-to-last node
      current <- .self$head
      while (!is.null(current$next_node$next_node)) {
        current <- current$next_node
      }
      
      deleted_data <- current$next_node$data
      current$next_node <- NULL
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
      
      current <- .self$head
      # Traverse to position - 1
      for (i in 1:(position - 1)) {
        current <- current$next_node
      }
      
      deleted_data <- current$next_node$data
      current$next_node <- current$next_node$next_node
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
      
      # If head contains the value
      if (.self$head$data == value) {
        .self$delete_from_head()
        return(TRUE)
      }
      
      current <- .self$head
      while (!is.null(current$next_node)) {
        if (current$next_node$data == value) {
          current$next_node <- current$next_node$next_node
          .self$size <- .self$size - 1
          cat("Deleted first occurrence of", value, ". List size:", .self$size, "\n")
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
      
      current <- .self$head
      for (i in 1:position) {
        current <- current$next_node
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
    
    # Convert list to R vector
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
    
    # Reverse the linked list
    reverse = function() {
      "Reverse the linked list in place"
      if (is.null(.self$head) || is.null(.self$head$next_node)) {
        return()  # Empty or single element list
      }
      
      prev_node <- NULL
      current <- .self$head
      
      while (!is.null(current)) {
        next_temp <- current$next_node
        current$next_node <- prev_node
        prev_node <- current
        current <- next_temp
      }
      
      .self$head <- prev_node
      cat("List reversed successfully\n")
    },
    
    # Find the middle element (useful for algorithms)
    find_middle = function() {
      "Find the middle element using two-pointer technique"
      if (is.null(.self$head)) {
        return(NULL)
      }
      
      slow_ptr <- .self$head
      fast_ptr <- .self$head
      
      while (!is.null(fast_ptr$next_node) && !is.null(fast_ptr$next_node$next_node)) {
        slow_ptr <- slow_ptr$next_node
        fast_ptr <- fast_ptr$next_node$next_node
      }
      
      cat("Middle element:", slow_ptr$data, "\n")
      return(slow_ptr$data)
    },
    
    # Detect cycle in the list (Floyd's algorithm)
    has_cycle = function() {
      "Detect if there's a cycle in the list using Floyd's algorithm"
      if (is.null(.self$head) || is.null(.self$head$next_node)) {
        return(FALSE)
      }
      
      slow_ptr <- .self$head
      fast_ptr <- .self$head
      
      while (!is.null(fast_ptr) && !is.null(fast_ptr$next_node)) {
        slow_ptr <- slow_ptr$next_node
        fast_ptr <- fast_ptr$next_node$next_node
        
        if (identical(slow_ptr, fast_ptr)) {
          return(TRUE)
        }
      }
      
      return(FALSE)
    },
    
    # Print the entire list
    print_list = function() {
      "Print all elements in the list"
      if (.self$is_empty()) {
        cat("List is empty\n")
        return()
      }
      
      cat("Linked List: ")
      current <- .self$head
      elements <- c()
      
      while (!is.null(current)) {
        elements <- c(elements, current$data)
        current <- current$next_node
      }
      
      cat(paste(elements, collapse = " -> "), "-> NULL\n")
      cat("Size:", .self$size, "\n")
    },
    
    # Clear the entire list
    clear = function() {
      "Remove all elements from the list"
      .self$head <- NULL
      .self$size <- 0
      cat("List cleared\n")
    }
  )
)

# ==============================================================================
# EXAMPLES AND DEMONSTRATIONS
# ==============================================================================

demonstrate_singly_linked_list <- function() {
  cat("=================================================================\n")
  cat("SINGLY LINKED LIST - COMPREHENSIVE DEMONSTRATION\n")
  cat("=================================================================\n\n")
  
  # Create a new linked list
  cat("1. CREATING A NEW SINGLY LINKED LIST\n")
  cat("-----------------------------------------------------------------\n")
  my_list <- SinglyLinkedList$new()
  cat("Created empty linked list\n")
  my_list$print_list()
  
  cat("\n2. INSERTION OPERATIONS\n")
  cat("-----------------------------------------------------------------\n")
  
  # Insert at head
  cat("Inserting elements at head: 10, 20, 30\n")
  my_list$insert_at_head(10)
  my_list$insert_at_head(20)
  my_list$insert_at_head(30)
  my_list$print_list()
  
  # Insert at tail
  cat("\nInserting elements at tail: 5, 1\n")
  my_list$insert_at_tail(5)
  my_list$insert_at_tail(1)
  my_list$print_list()
  
  # Insert at specific position
  cat("\nInserting 15 at position 2\n")
  my_list$insert_at_position(15, 2)
  my_list$print_list()
  
  cat("\n3. ACCESS AND SEARCH OPERATIONS\n")
  cat("-----------------------------------------------------------------\n")
  
  # Search for elements
  cat("Searching for elements:\n")
  my_list$search(15)
  my_list$search(99)
  
  # Get elements by position
  cat("\nAccessing elements by position:\n")
  cat("Element at position 0:", my_list$get(0), "\n")
  cat("Element at position 3:", my_list$get(3), "\n")
  
  # Find middle element
  cat("\nFinding middle element:\n")
  my_list$find_middle()
  
  cat("\n4. DELETION OPERATIONS\n")
  cat("-----------------------------------------------------------------\n")
  
  # Delete from head
  cat("Deleting from head:\n")
  deleted <- my_list$delete_from_head()
  cat("Deleted value:", deleted, "\n")
  my_list$print_list()
  
  # Delete from tail
  cat("\nDeleting from tail:\n")
  deleted <- my_list$delete_from_tail()
  cat("Deleted value:", deleted, "\n")
  my_list$print_list()
  
  # Delete by value
  cat("\nDeleting by value (15):\n")
  my_list$delete_by_value(15)
  my_list$print_list()
  
  # Delete at position
  cat("\nDeleting at position 1:\n")
  deleted <- my_list$delete_at_position(1)
  my_list$print_list()
  
  cat("\n5. UTILITY OPERATIONS\n")
  cat("-----------------------------------------------------------------\n")
  
  # Convert to vector
  cat("Converting to R vector:\n")
  vector_form <- my_list$to_vector()
  cat("Vector:", vector_form, "\n")
  
  # Add more elements for reversal demonstration
  cat("\nAdding more elements for reversal demo:\n")
  my_list$insert_at_tail(100)
  my_list$insert_at_tail(200)
  my_list$insert_at_tail(300)
  my_list$print_list()
  
  # Reverse the list
  cat("\nReversing the list:\n")
  my_list$reverse()
  my_list$print_list()
  
  # Check cycle detection
  cat("\nChecking for cycles:\n")
  has_cycle <- my_list$has_cycle()
  cat("Has cycle:", has_cycle, "\n")
  
  cat("\n6. PRACTICAL EXAMPLE: STUDENT GRADES\n")
  cat("-----------------------------------------------------------------\n")
  
  # Create a list for student grades
  grades_list <- SinglyLinkedList$new()
  
  cat("Managing student grades using linked list:\n")
  grades <- c(85, 92, 78, 96, 83, 89)
  for (grade in grades) {
    grades_list$insert_at_tail(grade)
  }
  
  cat("Initial grades: ")
  grades_list$print_list()
  
  # Add a new grade at the beginning (latest grade)
  cat("\nAdding new grade (95) at the beginning:\n")
  grades_list$insert_at_head(95)
  grades_list$print_list()
  
  # Remove the lowest grade (assuming we know it's 78)
  cat("\nRemoving grade 78:\n")
  grades_list$delete_by_value(78)
  grades_list$print_list()
  
  # Find middle grade
  cat("\nMiddle grade in the list:\n")
  middle_grade <- grades_list$find_middle()
  cat("Middle grade value:", middle_grade, "\n")
  
  # Convert to vector for statistical analysis
  grade_vector <- grades_list$to_vector()
  cat("Final grades vector:", grade_vector, "\n")
  cat("Average grade:", mean(grade_vector), "\n")
  
  cat("\n=================================================================\n")
  cat("END OF SINGLY LINKED LIST DEMONSTRATION\n")
  cat("=================================================================\n")
}

# Helper function to create a linked list from vector
create_list_from_vector <- function(vector_data) {
  "Create a linked list from an R vector"
  new_list <- SinglyLinkedList$new()
  for (element in vector_data) {
    new_list$insert_at_tail(element)
  }
  return(new_list)
}

# Examples are available but not run automatically to avoid side effects
# To run examples, execute: demonstrate_singly_linked_list()
if (interactive()) {
  cat("Loading Singly Linked List implementation...\n")
  cat("Run 'demonstrate_singly_linked_list()' to see comprehensive examples.\n")
  cat("Use 'SinglyLinkedList$new()' to create a new linked list.\n")
}

# Uncomment the following line to run examples automatically:
# demonstrate_singly_linked_list()