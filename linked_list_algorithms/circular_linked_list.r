# Circular Linked List Implementation in R
#
# A circular linked list is a variation of linked list where the last node
# points back to the first node, forming a circle. There's no NULL pointer
# at the end, making it truly circular.
#
# Types:
# - Circular Singly Linked List: Last node points to first node
# - Circular Doubly Linked List: Last node points to first, first points to last
#
# Time Complexities:
# - Access: O(n) - must traverse from any starting point
# - Search: O(n) - linear search through nodes
# - Insertion: O(1) at known position, O(n) at arbitrary position
# - Deletion: O(1) at known position, O(n) at arbitrary position
#
# Space Complexity: O(n) where n is number of elements
#
# Advantages:
# - Can start from any node and traverse entire list
# - Useful for round-robin scheduling
# - Efficient for applications needing cyclic traversal
# - No NULL pointers (except when empty)
#
# Disadvantages:
# - Risk of infinite loops if not handled carefully
# - Slightly more complex implementation
# - Detection of end requires careful tracking
#
# Applications:
# - Round-robin CPU scheduling
# - Multiplayer games (turn-based)
# - Circular buffer implementation
# - Music playlist (continuous loop)
# - Josephus problem solution

# Define Node class for circular linked list
CircularNode <- setRefClass("CircularNode",
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
      next_data <- if(is.null(.self$next_node)) "NULL" else .self$next_node$data
      cat("CircularNode(data =", .self$data, ", next =", next_data, ")\n")
    }
  )
)

# Define Circular Linked List class
CircularLinkedList <- setRefClass("CircularLinkedList",
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
      new_node <- CircularNode$new(data = data)
      
      if (is.null(.self$head)) {
        # First node - points to itself
        new_node$next_node <- new_node
        .self$head <- new_node
      } else {
        # Find the last node (points to current head)
        last_node <- .self$head
        while (!identical(last_node$next_node, .self$head)) {
          last_node <- last_node$next_node
        }
        
        new_node$next_node <- .self$head
        last_node$next_node <- new_node
        .self$head <- new_node
      }
      
      .self$size <- .self$size + 1
      cat("Inserted", data, "at head. List size:", .self$size, "\n")
    },
    
    # Insert element at the end of the list
    insert_at_tail = function(data) {
      "Insert a new node at the end of the list"
      if (is.null(.self$head)) {
        .self$insert_at_head(data)
        return()
      }
      
      new_node <- CircularNode$new(data = data)
      
      # Find the last node
      last_node <- .self$head
      while (!identical(last_node$next_node, .self$head)) {
        last_node <- last_node$next_node
      }
      
      new_node$next_node <- .self$head
      last_node$next_node <- new_node
      
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
      
      new_node <- CircularNode$new(data = data)
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
      
      if (.self$size == 1) {
        .self$head <- NULL
      } else {
        # Find the last node
        last_node <- .self$head
        while (!identical(last_node$next_node, .self$head)) {
          last_node <- last_node$next_node
        }
        
        last_node$next_node <- .self$head$next_node
        .self$head <- .self$head$next_node
      }
      
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
      
      if (.self$size == 1) {
        return(.self$delete_from_head())
      }
      
      # Find second-to-last node
      current <- .self$head
      while (!identical(current$next_node$next_node, .self$head)) {
        current <- current$next_node
      }
      
      deleted_data <- current$next_node$data
      current$next_node <- .self$head
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
      
      # Check if head contains the value
      if (.self$head$data == value) {
        .self$delete_from_head()
        return(TRUE)
      }
      
      current <- .self$head
      repeat {
        if (current$next_node$data == value) {
          current$next_node <- current$next_node$next_node
          .self$size <- .self$size - 1
          cat("Deleted first occurrence of", value, ". List size:", .self$size, "\n")
          return(TRUE)
        }
        current <- current$next_node
        
        # Check if we've completed the circle
        if (identical(current, .self$head)) {
          break
        }
      }
      
      cat("Value", value, "not found in list\n")
      return(FALSE)
    },
    
    # Search for an element
    search = function(value) {
      "Search for a value and return its position (0-indexed), -1 if not found"
      if (is.null(.self$head)) {
        cat("List is empty\n")
        return(-1)
      }
      
      current <- .self$head
      position <- 0
      
      repeat {
        if (current$data == value) {
          cat("Found", value, "at position", position, "\n")
          return(position)
        }
        current <- current$next_node
        position <- position + 1
        
        # Check if we've completed the circle
        if (identical(current, .self$head)) {
          break
        }
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
      "Convert circular linked list to R vector"
      if (.self$is_empty()) {
        return(c())
      }
      
      result <- c()
      current <- .self$head
      
      repeat {
        result <- c(result, current$data)
        current <- current$next_node
        
        # Stop when we complete the circle
        if (identical(current, .self$head)) {
          break
        }
      }
      
      return(result)
    },
    
    # Traverse the list n times (demonstrating circular nature)
    traverse_n_times = function(n_cycles = 2) {
      "Demonstrate circular traversal by going around the list multiple times"
      if (.self$is_empty()) {
        cat("List is empty\n")
        return()
      }
      
      cat("Traversing circular list", n_cycles, "times:\n")
      current <- .self$head
      total_visits <- n_cycles * .self$size
      
      for (i in 1:total_visits) {
        cat(current$data)
        current <- current$next_node
        
        if (i < total_visits) {
          cat(" -> ")
        }
        
        # Add cycle markers
        if (i %% .self$size == 0) {
          cat(" [Cycle ", i / .self$size, " complete]")
          if (i < total_visits) {
            cat(" -> ")
          }
        }
      }
      cat("\n")
    },
    
    # Solve Josephus problem using circular linked list
    josephus_problem = function(k) {
      "Solve Josephus problem: eliminate every k-th person until one remains"
      if (.self$is_empty()) {
        cat("List is empty\n")
        return(NULL)
      }
      
      if (k <= 0) {
        stop("k must be positive")
      }
      
      cat("Solving Josephus problem with k =", k, "\n")
      cat("Initial people:", .self$to_vector(), "\n")
      
      current <- .self$head
      
      while (.self$size > 1) {
        # Move k-1 positions
        for (i in 1:(k - 1)) {
          current <- current$next_node
        }
        
        # Eliminate current person
        eliminated <- current$data
        cat("Eliminating person:", eliminated, "\n")
        
        # Find the position and delete
        position <- 0
        temp <- .self$head
        while (!identical(temp, current)) {
          temp <- temp$next_node
          position <- position + 1
        }
        
        # Move to next person before deletion
        next_person <- current$next_node
        .self$delete_at_position(position)
        
        # Update current to next person, but handle head change
        if (.self$size > 0) {
          if (position == 0) {
            current <- .self$head
          } else {
            current <- next_person
          }
        }
      }
      
      survivor <- .self$head$data
      cat("Survivor:", survivor, "\n")
      return(survivor)
    },
    
    # Print the entire list
    print_list = function() {
      "Print all elements in the circular list"
      if (.self$is_empty()) {
        cat("List is empty\n")
        return()
      }
      
      cat("Circular Linked List: ")
      current <- .self$head
      elements <- c()
      
      repeat {
        elements <- c(elements, current$data)
        current <- current$next_node
        
        if (identical(current, .self$head)) {
          break
        }
      }
      
      cat(paste(elements, collapse = " -> "), "-> (back to", .self$head$data, ")\n")
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

demonstrate_circular_linked_list <- function() {
  cat("=================================================================\n")
  cat("CIRCULAR LINKED LIST - COMPREHENSIVE DEMONSTRATION\n")
  cat("=================================================================\n\n")
  
  # Create a new circular linked list
  cat("1. CREATING A NEW CIRCULAR LINKED LIST\n")
  cat("-----------------------------------------------------------------\n")
  cll <- CircularLinkedList$new()
  cat("Created empty circular linked list\n")
  cll$print_list()
  
  cat("\n2. INSERTION OPERATIONS\n")
  cat("-----------------------------------------------------------------\n")
  
  # Insert at head
  cat("Inserting elements at head: 10, 20, 30\n")
  cll$insert_at_head(10)
  cll$insert_at_head(20)
  cll$insert_at_head(30)
  cll$print_list()
  
  # Insert at tail
  cat("\nInserting elements at tail: 5, 1\n")
  cll$insert_at_tail(5)
  cll$insert_at_tail(1)
  cll$print_list()
  
  # Insert at specific position
  cat("\nInserting 15 at position 2\n")
  cll$insert_at_position(15, 2)
  cll$print_list()
  
  cat("\n3. CIRCULAR TRAVERSAL DEMONSTRATION\n")
  cat("-----------------------------------------------------------------\n")
  
  # Demonstrate circular nature
  cat("Normal traversal (one complete cycle):\n")
  cll$print_list()
  
  cat("\nDemonstrating circular nature (2 complete cycles):\n")
  cll$traverse_n_times(2)
  
  cat("\n4. ACCESS AND SEARCH OPERATIONS\n")
  cat("-----------------------------------------------------------------\n")
  
  # Search for elements
  cat("Searching for elements:\n")
  cll$search(15)
  cll$search(99)
  
  # Get elements by position
  cat("\nAccessing elements by position:\n")
  cat("Element at position 0:", cll$get(0), "\n")
  cat("Element at position 3:", cll$get(3), "\n")
  
  cat("\n5. DELETION OPERATIONS\n")
  cat("-----------------------------------------------------------------\n")
  
  # Delete from head
  cat("Deleting from head:\n")
  deleted <- cll$delete_from_head()
  cat("Deleted value:", deleted, "\n")
  cll$print_list()
  
  # Delete from tail
  cat("\nDeleting from tail:\n")
  deleted <- cll$delete_from_tail()
  cat("Deleted value:", deleted, "\n")
  cll$print_list()
  
  # Delete by value
  cat("\nDeleting by value (15):\n")
  cll$delete_by_value(15)
  cll$print_list()
  
  cat("\n6. JOSEPHUS PROBLEM DEMONSTRATION\n")
  cat("-----------------------------------------------------------------\n")
  
  # Create a new list for Josephus problem
  josephus_list <- CircularLinkedList$new()
  
  cat("Creating a circle of 7 people for Josephus problem:\n")
  for (i in 1:7) {
    josephus_list$insert_at_tail(i)
  }
  josephus_list$print_list()
  
  cat("\nSolving Josephus problem with k = 3 (eliminate every 3rd person):\n")
  survivor <- josephus_list$josephus_problem(3)
  cat("Final survivor:", survivor, "\n")
  
  cat("\n7. PRACTICAL EXAMPLE: ROUND-ROBIN SCHEDULING\n")
  cat("-----------------------------------------------------------------\n")
  
  # Simulate round-robin CPU scheduling
  scheduler <- CircularLinkedList$new()
  
  processes <- c("Process_A", "Process_B", "Process_C", "Process_D")
  cat("Setting up round-robin scheduler with processes:\n")
  for (process in processes) {
    scheduler$insert_at_tail(process)
    cat("Added:", process, "\n")
  }
  
  scheduler$print_list()
  
  cat("\nSimulating 8 time slots of round-robin scheduling:\n")
  if (!scheduler$is_empty()) {
    current_process <- scheduler$head
    for (slot in 1:8) {
      cat("Time slot", slot, ": Running", current_process$data, "\n")
      current_process <- current_process$next_node
    }
  }
  
  cat("\n8. PRACTICAL EXAMPLE: MUSIC PLAYLIST\n")
  cat("-----------------------------------------------------------------\n")
  
  # Create a circular music playlist
  playlist <- CircularLinkedList$new()
  
  songs <- c("Song1.mp3", "Song2.mp3", "Song3.mp3", "Song4.mp3")
  cat("Creating circular music playlist:\n")
  for (song in songs) {
    playlist$insert_at_tail(song)
    cat("Added:", song, "\n")
  }
  
  playlist$print_list()
  
  cat("\nPlaylist in repeat mode (playing 6 songs with wraparound):\n")
  if (!playlist$is_empty()) {
    current_song <- playlist$head
    for (play_count in 1:6) {
      cat("Now playing:", current_song$data, "\n")
      current_song <- current_song$next_node
    }
  }
  
  cat("\n9. ADVANTAGES DEMONSTRATION\n")
  cat("-----------------------------------------------------------------\n")
  
  cat("Circular linked list advantages demonstrated:\n")
  cat("- Continuous traversal without NULL checks\n")
  cat("- Perfect for cyclic applications (scheduling, playlists)\n")
  cat("- Efficient round-robin algorithms\n")
  cat("- Elegant solution to Josephus problem\n")
  cat("- No need to track end of list in many algorithms\n")
  
  cat("\n=================================================================\n")
  cat("END OF CIRCULAR LINKED LIST DEMONSTRATION\n")
  cat("=================================================================\n")
}

# Helper function to create a circular linked list from vector
create_circular_list_from_vector <- function(vector_data) {
  "Create a circular linked list from an R vector"
  new_list <- CircularLinkedList$new()
  for (element in vector_data) {
    new_list$insert_at_tail(element)
  }
  return(new_list)
}

# Examples are available but not run automatically to avoid side effects
# To run examples, execute: demonstrate_circular_linked_list()
if (interactive()) {
  cat("Loading Circular Linked List implementation...\n")
  cat("Run 'demonstrate_circular_linked_list()' to see comprehensive examples.\n")
  cat("Use 'CircularLinkedList$new()' to create a new circular linked list.\n")
}

# Uncomment the following line to run examples automatically:
# demonstrate_circular_linked_list()