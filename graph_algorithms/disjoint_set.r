# Disjoint Set Union (Union-Find)
#
# A data structure that keeps track of elements partitioned into disjoint sets.
# Supports union and find operations efficiently using path compression and union by rank.
#
# Time Complexity: Nearly O(1) per operation (amortized with path compression)
# Space Complexity: O(N)
#
# Applications:
# - Detecting cycles in undirected graphs
# - Kruskal's MST algorithm
# - Network connectivity
# - Social network grouping
# - Image segmentation
# - Connected components in graphs

make_set <- function(n) {
  #' Initialize disjoint set with n elements
  #' @param n: Number of elements (0 to n-1)
  #' @return: List containing parent array and rank array
  
  # Each element is initially its own parent
  parent <- 0:(n-1)
  # Initial rank (tree height) is 0 for all elements
  rank <- rep(0, n)
  
  list(parent = parent, rank = rank)
}

find_set <- function(ds, v) {
  #' Find the representative (root) of the set containing v
  #' Uses path compression for efficiency
  #' @param ds: Disjoint set data structure
  #' @param v: Element to find
  #' @return: Representative of v's set
  
  if (ds$parent[v + 1] != v) {
    # Path compression: Make all nodes on path point to root
    ds$parent[v + 1] <- find_set(ds, ds$parent[v + 1])
  }
  ds$parent[v + 1]
}

union_sets <- function(ds, a, b) {
  #' Union two sets by rank
  #' @param ds: Disjoint set data structure
  #' @param a: First element
  #' @param b: Second element
  #' @return: Updated disjoint set structure
  
  a_root <- find_set(ds, a)
  b_root <- find_set(ds, b)
  
  if (a_root != b_root) {
    # Union by rank: Attach smaller rank tree under root of higher rank tree
    if (ds$rank[a_root + 1] < ds$rank[b_root + 1]) {
      # Swap to ensure a_root has higher rank
      temp <- a_root
      a_root <- b_root
      b_root <- temp
    }
    
    ds$parent[b_root + 1] <- a_root
    
    # If ranks are equal, increment the rank of the root
    if (ds$rank[a_root + 1] == ds$rank[b_root + 1]) {
      ds$rank[a_root + 1] <- ds$rank[a_root + 1] + 1
    }
  }
  
  return(ds)
}

# Example Usage
# Test case 1: Basic operations
cat("=== Test Case 1: Basic Union-Find Operations ===\n")
ds1 <- make_set(5)  # Create sets for elements 0-4
cat("Initial parents:", paste(ds1$parent, collapse = " "), "\n")

ds1 <- union_sets(ds1, 0, 1)  # Union first two elements
ds1 <- union_sets(ds1, 2, 3)  # Union next two elements
cat("After unions:", paste(ds1$parent, collapse = " "), "\n")

cat("Find(1):", find_set(ds1, 1), "\n")
cat("Find(2):", find_set(ds1, 2), "\n")

# Test case 2: Path compression
cat("\n=== Test Case 2: Path Compression ===\n")
ds2 <- make_set(6)
ds2 <- union_sets(ds2, 0, 1)
ds2 <- union_sets(ds2, 1, 2)
ds2 <- union_sets(ds2, 2, 3)
cat("Parents before find:", paste(ds2$parent, collapse = " "), "\n")
find_set(ds2, 3)  # This should compress the path
cat("Parents after find:", paste(ds2$parent, collapse = " "), "\n")

# Test case 3: Connected components
cat("\n=== Test Case 3: Finding Connected Components ===\n")
ds3 <- make_set(7)
edges <- list(c(0,1), c(1,2), c(3,4), c(5,6))
for (edge in edges) {
  ds3 <- union_sets(ds3, edge[1], edge[2])
}

# Print connected components
components <- list()
for (i in 0:6) {
  root <- find_set(ds3, i)
  if (is.null(components[[as.character(root)]])) {
    components[[as.character(root)]] <- c()
  }
  components[[as.character(root)]] <- c(components[[as.character(root)]], i)
}

cat("Connected Components:\n")
for (comp in components) {
  cat("  Group:", paste(comp, collapse = " "), "\n")
}