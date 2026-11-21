# Union-Find (Disjoint Set Union) keeps track of elements split into
# non-overlapping sets. It supports two main operations:
# - find:  which set does an element belong to (returns the set representative)
# - union: merge the sets containing two elements
#
# Two optimizations make both operations almost constant time:
# - Path compression: find() points every visited node straight at the root.
# - Union by rank: the shorter tree is attached below the taller one.
#
# Time Complexity: O(alpha(n)) amortized per operation, where alpha is the
# inverse Ackermann function (below 5 for any practical n)
# Space Complexity: O(n)
#
# Applications:
# - Kruskal's minimum spanning tree
# - Detecting cycles in undirected graphs
# - Connected components
# - Percolation and image segmentation
#
# Example usage:
# uf <- uf_new(5)                 # elements 1..5, each in its own set
# uf <- uf_union(uf, 1, 2)
# uf <- uf_union(uf, 2, 3)
# uf_connected(uf, 1, 3)$connected  # TRUE
# uf$count                          # 3 sets remain
#
# Note: R copies values, so functions that change the structure return the
# updated structure. Always reassign the result (uf <- uf_union(...)).

#' Create a union-find structure with n singleton sets
#' @param n Number of elements (labelled 1..n)
#' @return List with parent, rank and count (number of sets)
uf_new <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || n < 0 || n != floor(n)) {
    stop("Error: n must be a non-negative integer")
  }
  list(parent = seq_len(n), rank = integer(n), count = as.integer(n))
}

uf_check <- function(uf, x) {
  if (!is.numeric(x) || length(x) != 1 || x < 1 || x > length(uf$parent) ||
      x != floor(x)) {
    stop(sprintf("Error: element must be an integer between 1 and %d",
                 length(uf$parent)))
  }
}

#' Find the representative (root) of the set containing x
#' @param uf Structure from uf_new()
#' @param x Element to look up
#' @return List with root and the updated structure (after path compression)
uf_find <- function(uf, x) {
  uf_check(uf, x)
  root <- x
  while (uf$parent[root] != root) {
    root <- uf$parent[root]
  }
  # Path compression: point every node on the path directly at the root
  while (uf$parent[x] != root) {
    next_node <- uf$parent[x]
    uf$parent[x] <- root
    x <- next_node
  }
  list(root = root, uf = uf)
}

#' Merge the sets containing x and y
#' @return The updated structure (unchanged if already in the same set)
uf_union <- function(uf, x, y) {
  fx <- uf_find(uf, x)
  fy <- uf_find(fx$uf, y)
  uf <- fy$uf
  rx <- fx$root
  ry <- fy$root
  if (rx == ry) {
    return(uf)
  }
  # Union by rank: attach the shorter tree below the taller one
  if (uf$rank[rx] < uf$rank[ry]) {
    uf$parent[rx] <- ry
  } else if (uf$rank[rx] > uf$rank[ry]) {
    uf$parent[ry] <- rx
  } else {
    uf$parent[ry] <- rx
    uf$rank[rx] <- uf$rank[rx] + 1L
  }
  uf$count <- uf$count - 1L
  uf
}

#' Check whether x and y are in the same set
#' @return List with connected (TRUE or FALSE) and the updated structure
uf_connected <- function(uf, x, y) {
  fx <- uf_find(uf, x)
  fy <- uf_find(fx$uf, y)
  list(connected = fx$root == fy$root, uf = fy$uf)
}

# ===========================
# Example Usage & Testing
# ===========================
uf <- uf_new(8)
stopifnot(uf$count == 8)
stopifnot(!uf_connected(uf, 1, 2)$connected)

uf <- uf_union(uf, 1, 2)
uf <- uf_union(uf, 3, 4)
uf <- uf_union(uf, 2, 3)
stopifnot(uf$count == 5)
stopifnot(uf_connected(uf, 1, 4)$connected)
stopifnot(!uf_connected(uf, 1, 5)$connected)

# Union of elements already in the same set changes nothing
uf <- uf_union(uf, 1, 4)
stopifnot(uf$count == 5)

# Merge everything into one set
for (i in 2:8) {
  uf <- uf_union(uf, 1, i)
}
stopifnot(uf$count == 1)
root <- uf_find(uf, 1)$root
for (i in 1:8) {
  stopifnot(uf_find(uf, i)$root == root)
}

# Invalid element raises an error
stopifnot(inherits(try(uf_find(uf, 9), silent = TRUE), "try-error"))

cat("Number of sets after all unions:", uf$count, "\n")
cat("All union-find tests passed\n")
