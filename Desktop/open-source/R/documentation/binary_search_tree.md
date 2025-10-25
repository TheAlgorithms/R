

``` r
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
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
## logical.return = TRUE, : there is no package called 'R6'
```

```
## Installing R6 package for object-oriented programming...
```

``` r
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
```

```
## === Binary Search Tree (BST) Implementation ===
```

``` r
# Test 1: Basic BST operations
cat("1. Basic BST Operations\n")
```

```
## 1. Basic BST Operations
```

``` r
bst <- BST$new()

# Insert values
values <- c(50, 30, 70, 20, 40, 60, 80)
cat("Inserting values:", paste(values, collapse = ", "), "\n")
```

```
## Inserting values: 50, 30, 70, 20, 40, 60, 80
```

``` r
for (value in values) {
  bst$insert(value)
}

cat("Tree size:", bst$get_size(), "\n")
```

```
## Tree size: 7
```

``` r
cat("Tree height:", bst$height(), "\n")
```

```
## Tree height: 2
```

``` r
cat("Is valid BST:", bst$is_valid_bst(), "\n\n")
```

```
## Is valid BST: TRUE
```

``` r
# Test 2: Tree visualization
cat("2. Tree Structure\n")
```

```
## 2. Tree Structure
```

``` r
bst$print_tree()
```

```
## └── 50 
##     ├── 30 
##     │   ├── 20 
##     │   └── 40 
##     └── 70 
##         ├── 60 
##         └── 80
```

``` r
cat("\n")
```

``` r
# Test 3: Search operations
cat("3. Search Operations\n")
```

```
## 3. Search Operations
```

``` r
search_values <- c(40, 25, 80, 100)
for (value in search_values) {
  found <- bst$search(value)
  cat("Search for", value, ":", if (found) "Found" else "Not found", "\n")
}
```

```
## Search for 40 : Found 
## Search for 25 : Not found 
## Search for 80 : Found 
## Search for 100 : Not found
```

``` r
cat("\n")
```

``` r
# Test 4: Min/Max operations
cat("4. Min/Max Operations\n")
```

```
## 4. Min/Max Operations
```

``` r
cat("Minimum value:", bst$find_min(), "\n")
```

```
## Minimum value: 20
```

``` r
cat("Maximum value:", bst$find_max(), "\n\n")
```

```
## Maximum value: 80
```

``` r
# Test 5: Tree traversals
cat("5. Tree Traversals\n")
```

```
## 5. Tree Traversals
```

``` r
cat("In-order (sorted):   ", paste(bst$inorder_traversal(), collapse = ", "), "\n")
```

```
## In-order (sorted):
```

``` r
cat("Pre-order:           ", paste(bst$preorder_traversal(), collapse = ", "), "\n")
```

```
## Pre-order:
```

``` r
cat("Post-order:          ", paste(bst$postorder_traversal(), collapse = ", "), "\n")
```

```
## Post-order:
```

``` r
cat("Level-order (BFS):   ", paste(bst$level_order_traversal(), collapse = ", "), "\n\n")
```

```
## Level-order (BFS):    50, 30, 70, 20, 40, 60, 80
```

``` r
# Test 6: Deletion operations
cat("6. Deletion Operations\n")
```

```
## 6. Deletion Operations
```

``` r
delete_values <- c(20, 30, 50)  # Delete leaf, node with one child, root

for (value in delete_values) {
  cat("Deleting", value, ":", if (bst$delete(value)) "Success" else "Failed", "\n")
  cat("Tree after deletion:\n")
  bst$print_tree()
  cat("In-order traversal:", paste(bst$inorder_traversal(), collapse = ", "), "\n")
  cat("Tree size:", bst$get_size(), "\n\n")
}
```

```
## Deleting 20 : Success 
## Tree after deletion:
## └── 50 
##     ├── 30 
##     │   └── 40 
##     └── 70 
##         ├── 60 
##         └── 80 
## In-order traversal:  
## Tree size: 6 
## 
## Deleting 30 : Success 
## Tree after deletion:
## └── 50 
##     ├── 40 
##     └── 70 
##         ├── 60 
##         └── 80 
## In-order traversal:  
## Tree size: 5 
## 
## Deleting 50 : Success 
## Tree after deletion:
## └── 60 
##     ├── 40 
##     └── 70 
##         └── 80 
## In-order traversal:  
## Tree size: 4
```

``` r
# Test 7: Create BST from sorted vs unsorted array
cat("7. BST Creation Comparison\n")
```

```
## 7. BST Creation Comparison
```

``` r
# Sorted array (creates skewed tree)
sorted_array <- c(1, 2, 3, 4, 5, 6, 7)
bst_sorted <- create_bst_from_array(sorted_array)
cat("BST from sorted array [1,2,3,4,5,6,7]:\n")
```

```
## BST from sorted array [1,2,3,4,5,6,7]:
```

``` r
cat("Height:", bst_sorted$height(), "(should be close to log₂(7) ≈ 2.8 for balanced)\n")
```

```
## Height: 6 (should be close to log₂(7) ≈ 2.8 for balanced)
```

``` r
bst_sorted$print_tree()
```

```
## └── 1 
##     └── 2 
##         └── 3 
##             └── 4 
##                 └── 5 
##                     └── 6 
##                         └── 7
```

``` r
# Shuffled array (more balanced)
shuffled_array <- c(4, 2, 6, 1, 3, 5, 7)
bst_shuffled <- create_bst_from_array(shuffled_array)
cat("BST from shuffled array [4,2,6,1,3,5,7]:\n")
```

```
## BST from shuffled array [4,2,6,1,3,5,7]:
```

``` r
cat("Height:", bst_shuffled$height(), "\n")
```

```
## Height: 2
```

``` r
bst_shuffled$print_tree()
```

```
## └── 4 
##     ├── 2 
##     │   ├── 1 
##     │   └── 3 
##     └── 6 
##         ├── 5 
##         └── 7
```

``` r
# Test 8: kth smallest element
cat("8. Finding kth Smallest Elements\n")
```

```
## 8. Finding kth Smallest Elements
```

``` r
for (k in 1:min(5, bst_shuffled$get_size())) {
  kth_val <- kth_smallest(bst_shuffled, k)
  cat(k, "smallest element:", kth_val, "\n")
}
```

```
## 1 smallest element: 
## 2 smallest element: 
## 3 smallest element: 
## 4 smallest element: 
## 5 smallest element:
```

``` r
cat("\n")
```

``` r
# Test 9: Edge cases
cat("9. Edge Cases\n")
```

```
## 9. Edge Cases
```

``` r
empty_bst <- BST$new()
cat("Empty BST:\n")
```

```
## Empty BST:
```

``` r
cat("Is empty:", empty_bst$is_empty(), "\n")
```

```
## Is empty: TRUE
```

``` r
cat("Size:", empty_bst$get_size(), "\n")
```

```
## Size: 0
```

``` r
cat("Height:", empty_bst$height(), "\n")
```

```
## Height: -1
```

``` r
cat("Min value:", empty_bst$find_min(), "\n")
```

```
## Min value:
```

``` r
cat("Search for 5:", empty_bst$search(5), "\n")
```

```
## Search for 5: FALSE
```

``` r
cat("Delete 5:", empty_bst$delete(5), "\n")
```

```
## Delete 5: FALSE
```

``` r
# Single node BST
single_bst <- BST$new()
single_bst$insert(42)
cat("\nSingle node BST:\n")
```

```
## 
## Single node BST:
```

``` r
cat("Size:", single_bst$get_size(), "\n")
```

```
## Size: 1
```

``` r
cat("Height:", single_bst$height(), "\n")
```

```
## Height: 0
```

``` r
cat("Min/Max:", single_bst$find_min(), "/", single_bst$find_max(), "\n")
```

```
## Min/Max: 42 / 42
```

``` r
single_bst$print_tree()
```

```
## └── 42
```

``` r
# Test 10: Real-world example - Student grades
cat("\n10. Real-world Example - Student Grade Management\n")
```

```
## 
## 10. Real-world Example - Student Grade Management
```

``` r
grade_bst <- BST$new()
grades <- c(85, 92, 78, 96, 83, 88, 91, 79, 87, 94)

cat("Student grades:", paste(grades, collapse = ", "), "\n")
```

```
## Student grades: 85, 92, 78, 96, 83, 88, 91, 79, 87, 94
```

``` r
for (grade in grades) {
  grade_bst$insert(grade)
}

cat("Grades in ascending order:", paste(grade_bst$inorder_traversal(), collapse = ", "), "\n")
```

```
## Grades in ascending order:
```

``` r
cat("Highest grade:", grade_bst$find_max(), "\n")
```

```
## Highest grade: 96
```

``` r
cat("Lowest grade:", grade_bst$find_min(), "\n")
```

```
## Lowest grade: 78
```

``` r
cat("Median grade (middle element):", kth_smallest(grade_bst, ceiling(grade_bst$get_size()/2)), "\n")
```

```
## Median grade (middle element):
```

``` r
# Find students above certain grade
threshold <- 90
above_threshold <- grade_bst$inorder_traversal()
above_threshold <- above_threshold[above_threshold >= threshold]
cat("Grades >=", threshold, ":", paste(above_threshold, collapse = ", "), "\n")
```

```
## Grades >= 90 :
```

``` r
cat("\nGrade distribution tree:\n")
```

```
## 
## Grade distribution tree:
```

``` r
grade_bst$print_tree()
```

```
## └── 85 
##     ├── 78 
##     │   └── 83 
##     │       └── 79 
##     └── 92 
##         ├── 88 
##         │   ├── 87 
##         │   └── 91 
##         └── 96 
##             └── 94
```

