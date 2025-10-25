# Binary Search Tree (BST) Implementation
#
# A Binary Search Tree is a hierarchical data structure where each node has at most
# two children (left and right), and for every node:
# - All values in the left subtree are less than the node's value
# - All values in the right subtree are greater than the node's value
# - Both subtrees are also binary search trees
#
# Time Complexities (average case):
# - Search: O(log n)  
# - Insert: O(log n)
# - Delete: O(log n)
# - Traversal: O(n)
#
# Worst case: O(n) when tree becomes skewed (like a linked list)
#
# Applications:
# - Database indexing
# - Expression parsing
# - Priority queues
# - File system organization

# Define BST Node structure using R6 class system
if (!require(R6, quietly = TRUE)) {
  cat("Installing R6 package for object-oriented programming...\n")
  install.packages("R6", quiet = TRUE)
  library(R6)
}

# BST Node class
BSTNode <- R6Class("BSTNode",
  public = list(
    value = NULL,
    left = NULL,
    right = NULL,
    
    initialize = function(value) {
      self$value <- value
      self$left <- NULL
      self$right <- NULL
    }
  )
)

# Binary Search Tree class
BST <- R6Class("BST",
  public = list(
    root = NULL,
    size = 0,
    
    initialize = function() {
      self$root <- NULL
      self$size <- 0
    },
    
    # Insert a value into the BST
    insert = function(value) {
      if (is.null(self$root)) {
        self$root <- BSTNode$new(value)
        self$size <- self$size + 1
      } else {
        private$insert_recursive(self$root, value)
      }
    },
    
    # Search for a value in the BST
    search = function(value) {
      return(private$search_recursive(self$root, value))
    },
    
    # Delete a value from the BST
    delete = function(value) {
      if (self$search(value)) {
        self$root <- private$delete_recursive(self$root, value)
        self$size <- self$size - 1
        return(TRUE)
      }
      return(FALSE)
    },
    
    # Find minimum value in the BST
    find_min = function() {
      if (is.null(self$root)) return(NULL)
      return(private$find_min_recursive(self$root)$value)
    },
    
    # Find maximum value in the BST  
    find_max = function() {
      if (is.null(self$root)) return(NULL)
      return(private$find_max_recursive(self$root)$value)
    },
    
    # In-order traversal (left, root, right) - gives sorted output
    inorder_traversal = function() {
      result <- c()
      private$inorder_recursive(self$root, result)
      return(result)
    },
    
    # Pre-order traversal (root, left, right)
    preorder_traversal = function() {
      result <- c()
      private$preorder_recursive(self$root, result)
      return(result)
    },
    
    # Post-order traversal (left, right, root)
    postorder_traversal = function() {
      result <- c()
      private$postorder_recursive(self$root, result)
      return(result)
    },
    
    # Level-order traversal (breadth-first)
    level_order_traversal = function() {
      if (is.null(self$root)) return(c())
      
      result <- c()
      queue <- list(self$root)
      
      while (length(queue) > 0) {
        node <- queue[[1]]
        queue <- queue[-1]
        
        result <- c(result, node$value)
        
        if (!is.null(node$left)) {
          queue <- append(queue, list(node$left))
        }
        if (!is.null(node$right)) {
          queue <- append(queue, list(node$right))
        }
      }
      
      return(result)
    },
    
    # Get height of the tree
    height = function() {
      return(private$height_recursive(self$root))
    },
    
    # Check if tree is valid BST
    is_valid_bst = function() {
      return(private$is_valid_bst_recursive(self$root, -Inf, Inf))
    },
    
    # Get size of the tree
    get_size = function() {
      return(self$size)
    },
    
    # Check if tree is empty
    is_empty = function() {
      return(is.null(self$root))
    },
    
    # Print tree structure
    print_tree = function() {
      if (is.null(self$root)) {
        cat("Empty tree\n")
        return()
      }
      private$print_tree_recursive(self$root, "", TRUE)
    }
  ),
  
  private = list(
    # Recursive helper for insertion
    insert_recursive = function(node, value) {
      if (value < node$value) {
        if (is.null(node$left)) {
          node$left <- BSTNode$new(value)
          self$size <- self$size + 1
        } else {
          private$insert_recursive(node$left, value)
        }
      } else if (value > node$value) {
        if (is.null(node$right)) {
          node$right <- BSTNode$new(value)
          self$size <- self$size + 1
        } else {
          private$insert_recursive(node$right, value)
        }
      }
      # If value == node$value, don't insert (no duplicates)
    },
    
    # Recursive helper for search
    search_recursive = function(node, value) {
      if (is.null(node) || node$value == value) {
        return(!is.null(node))
      }
      
      if (value < node$value) {
        return(private$search_recursive(node$left, value))
      } else {
        return(private$search_recursive(node$right, value))
      }
    },
    
    # Recursive helper for deletion
    delete_recursive = function(node, value) {
      if (is.null(node)) {
        return(NULL)
      }
      
      if (value < node$value) {
        node$left <- private$delete_recursive(node$left, value)
      } else if (value > node$value) {
        node$right <- private$delete_recursive(node$right, value)
      } else {
        # Node to delete found
        if (is.null(node$left)) {
          return(node$right)
        } else if (is.null(node$right)) {
          return(node$left)
        }
        
        # Node has two children - find inorder successor
        successor <- private$find_min_recursive(node$right)
        node$value <- successor$value
        node$right <- private$delete_recursive(node$right, successor$value)
      }
      
      return(node)
    },
    
    # Find minimum node
    find_min_recursive = function(node) {
      while (!is.null(node$left)) {
        node <- node$left
      }
      return(node)
    },
    
    # Find maximum node
    find_max_recursive = function(node) {
      while (!is.null(node$right)) {
        node <- node$right
      }
      return(node)
    },
    
    # In-order traversal helper
    inorder_recursive = function(node, result) {
      if (!is.null(node)) {
        result <<- private$inorder_recursive(node$left, result)
        result <<- c(result, node$value)
        result <<- private$inorder_recursive(node$right, result)
      }
      return(result)
    },
    
    # Pre-order traversal helper
    preorder_recursive = function(node, result) {
      if (!is.null(node)) {
        result <<- c(result, node$value)
        result <<- private$preorder_recursive(node$left, result)
        result <<- private$preorder_recursive(node$right, result)
      }
      return(result)
    },
    
    # Post-order traversal helper
    postorder_recursive = function(node, result) {
      if (!is.null(node)) {
        result <<- private$postorder_recursive(node$left, result)
        result <<- private$postorder_recursive(node$right, result)
        result <<- c(result, node$value)
      }
      return(result)
    },
    
    # Height calculation helper
    height_recursive = function(node) {
      if (is.null(node)) {
        return(-1)  # Height of empty tree is -1
      }
      
      left_height <- private$height_recursive(node$left)
      right_height <- private$height_recursive(node$right)
      
      return(1 + max(left_height, right_height))
    },
    
    # BST validation helper
    is_valid_bst_recursive = function(node, min_val, max_val) {
      if (is.null(node)) {
        return(TRUE)
      }
      
      if (node$value <= min_val || node$value >= max_val) {
        return(FALSE)
      }
      
      return(private$is_valid_bst_recursive(node$left, min_val, node$value) &&
             private$is_valid_bst_recursive(node$right, node$value, max_val))
    },
    
    # Tree printing helper
    print_tree_recursive = function(node, prefix, is_last) {
      if (!is.null(node)) {
        cat(prefix)
        cat(if (is_last) "└── " else "├── ")
        cat(node$value, "\n")
        
        new_prefix <- paste0(prefix, if (is_last) "    " else "│   ")
        
        if (!is.null(node$left) || !is.null(node$right)) {
          if (!is.null(node$left)) {
            private$print_tree_recursive(node$left, new_prefix, is.null(node$right))
          }
          if (!is.null(node$right)) {
            private$print_tree_recursive(node$right, new_prefix, TRUE)
          }
        }
      }
    }
  )
)

# Utility functions for BST operations

# Create BST from array
create_bst_from_array <- function(arr) {
  #' Create a BST from an array of values
  #' @param arr: Array of values to insert
  #' @return: BST object
  
  bst <- BST$new()
  for (value in arr) {
    bst$insert(value)
  }
  return(bst)
}

# Check if two BSTs are identical
are_identical_bsts <- function(bst1, bst2) {
  #' Check if two BSTs have identical structure and values
  #' @param bst1: First BST
  #' @param bst2: Second BST
  #' @return: TRUE if identical, FALSE otherwise
  
  traversal1 <- bst1$preorder_traversal()
  traversal2 <- bst2$preorder_traversal()
  
  return(identical(traversal1, traversal2))
}

# Find kth smallest element in BST
kth_smallest <- function(bst, k) {
  #' Find the kth smallest element in BST
  #' @param bst: BST object
  #' @param k: Position (1-indexed)
  #' @return: kth smallest value or NULL if k is out of bounds
  
  inorder <- bst$inorder_traversal()
  if (k > 0 && k <= length(inorder)) {
    return(inorder[k])
  }
  return(NULL)
}

# Example usage and testing
cat("=== Binary Search Tree (BST) Implementation ===\n\n")

# Test 1: Basic BST operations
cat("1. Basic BST Operations\n")
bst <- BST$new()

# Insert values
values <- c(50, 30, 70, 20, 40, 60, 80)
cat("Inserting values:", paste(values, collapse = ", "), "\n")

for (value in values) {
  bst$insert(value)
}

cat("Tree size:", bst$get_size(), "\n")
cat("Tree height:", bst$height(), "\n")
cat("Is valid BST:", bst$is_valid_bst(), "\n\n")

# Test 2: Tree visualization
cat("2. Tree Structure\n")
bst$print_tree()
cat("\n")

# Test 3: Search operations
cat("3. Search Operations\n")
search_values <- c(40, 25, 80, 100)
for (value in search_values) {
  found <- bst$search(value)
  cat("Search for", value, ":", if (found) "Found" else "Not found", "\n")
}
cat("\n")

# Test 4: Min/Max operations
cat("4. Min/Max Operations\n")
cat("Minimum value:", bst$find_min(), "\n")
cat("Maximum value:", bst$find_max(), "\n\n")

# Test 5: Tree traversals
cat("5. Tree Traversals\n")
cat("In-order (sorted):   ", paste(bst$inorder_traversal(), collapse = ", "), "\n")
cat("Pre-order:           ", paste(bst$preorder_traversal(), collapse = ", "), "\n")
cat("Post-order:          ", paste(bst$postorder_traversal(), collapse = ", "), "\n")
cat("Level-order (BFS):   ", paste(bst$level_order_traversal(), collapse = ", "), "\n\n")

# Test 6: Deletion operations
cat("6. Deletion Operations\n")
delete_values <- c(20, 30, 50)  # Delete leaf, node with one child, root

for (value in delete_values) {
  cat("Deleting", value, ":", if (bst$delete(value)) "Success" else "Failed", "\n")
  cat("Tree after deletion:\n")
  bst$print_tree()
  cat("In-order traversal:", paste(bst$inorder_traversal(), collapse = ", "), "\n")
  cat("Tree size:", bst$get_size(), "\n\n")
}

# Test 7: Create BST from sorted vs unsorted array
cat("7. BST Creation Comparison\n")

# Sorted array (creates skewed tree)
sorted_array <- c(1, 2, 3, 4, 5, 6, 7)
bst_sorted <- create_bst_from_array(sorted_array)
cat("BST from sorted array [1,2,3,4,5,6,7]:\n")
cat("Height:", bst_sorted$height(), "(should be close to log₂(7) ≈ 2.8 for balanced)\n")
bst_sorted$print_tree()

# Shuffled array (more balanced)
shuffled_array <- c(4, 2, 6, 1, 3, 5, 7)
bst_shuffled <- create_bst_from_array(shuffled_array)
cat("BST from shuffled array [4,2,6,1,3,5,7]:\n")
cat("Height:", bst_shuffled$height(), "\n")
bst_shuffled$print_tree()

# Test 8: kth smallest element
cat("8. Finding kth Smallest Elements\n")
for (k in 1:min(5, bst_shuffled$get_size())) {
  kth_val <- kth_smallest(bst_shuffled, k)
  cat(k, "smallest element:", kth_val, "\n")
}
cat("\n")

# Test 9: Edge cases
cat("9. Edge Cases\n")
empty_bst <- BST$new()
cat("Empty BST:\n")
cat("Is empty:", empty_bst$is_empty(), "\n")
cat("Size:", empty_bst$get_size(), "\n")
cat("Height:", empty_bst$height(), "\n")
cat("Min value:", empty_bst$find_min(), "\n")
cat("Search for 5:", empty_bst$search(5), "\n")
cat("Delete 5:", empty_bst$delete(5), "\n")

# Single node BST
single_bst <- BST$new()
single_bst$insert(42)
cat("\nSingle node BST:\n")
cat("Size:", single_bst$get_size(), "\n")
cat("Height:", single_bst$height(), "\n")
cat("Min/Max:", single_bst$find_min(), "/", single_bst$find_max(), "\n")
single_bst$print_tree()

# Test 10: Real-world example - Student grades
cat("\n10. Real-world Example - Student Grade Management\n")
grade_bst <- BST$new()
grades <- c(85, 92, 78, 96, 83, 88, 91, 79, 87, 94)

cat("Student grades:", paste(grades, collapse = ", "), "\n")
for (grade in grades) {
  grade_bst$insert(grade)
}

cat("Grades in ascending order:", paste(grade_bst$inorder_traversal(), collapse = ", "), "\n")
cat("Highest grade:", grade_bst$find_max(), "\n")
cat("Lowest grade:", grade_bst$find_min(), "\n")
cat("Median grade (middle element):", kth_smallest(grade_bst, ceiling(grade_bst$get_size()/2)), "\n")

# Find students above certain grade
threshold <- 90
above_threshold <- grade_bst$inorder_traversal()
above_threshold <- above_threshold[above_threshold >= threshold]
cat("Grades >=", threshold, ":", paste(above_threshold, collapse = ", "), "\n")

cat("\nGrade distribution tree:\n")
grade_bst$print_tree()