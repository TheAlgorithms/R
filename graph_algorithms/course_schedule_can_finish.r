# Course Schedule - Detect Cycle in Directed Graph (Kahn's Algorithm)
#
# Determine if it's possible to finish all courses given prerequisite pairs.
# This is equivalent to checking if a directed graph has a cycle.
# If no cycle exists, a topological ordering exists and all courses can be completed.
#
# Approach: Kahn's Algorithm (BFS-based Topological Sort)
# - Build adjacency list for edges prereq -> course and compute in-degrees
# - Initialize a queue with all nodes having in-degree 0 (no prerequisites)
# - Repeatedly pop, append to order, and decrement neighbors' in-degrees
# - If we processed all nodes, no cycle; otherwise a cycle exists
#
# Time Complexity: O(V + E)
# Space Complexity: O(V + E)
#
# Inputs:
# - num_courses: integer, number of courses (vertices)
# - prerequisites: 2-column structure of pairs (course, prerequisite)
#   Accepted formats: matrix/data.frame with 2 columns, or a flat numeric vector of length 2*m.
#   Indices can be 0-based (common in LeetCode) or 1-based; function normalizes internally to 1..num_courses.
#
# Outputs:
# - can_finish(num_courses, prerequisites): TRUE if all courses can be finished (no cycle), FALSE otherwise
# - find_course_order(num_courses, prerequisites): a topological order vector (length num_courses) if possible, otherwise integer(0)

# Normalize prerequisites to a 2-column integer matrix with 1-based indices
.normalize_prereqs <- function(num_courses, prerequisites) {
  if (is.null(prerequisites)) {
    return(matrix(integer(0), ncol = 2))
  }
  # Coerce to matrix with 2 columns
  if (is.vector(prerequisites)) {
    if (length(prerequisites) %% 2 != 0) {
      stop("prerequisites vector length must be even (pairs of course, prereq)")
    }
    prerequisites <- matrix(prerequisites, ncol = 2, byrow = TRUE)
  } else if (is.data.frame(prerequisites)) {
    if (ncol(prerequisites) != 2) stop("prerequisites data.frame must have exactly 2 columns")
    prerequisites <- as.matrix(prerequisites)
  } else if (is.matrix(prerequisites)) {
    if (ncol(prerequisites) != 2) stop("prerequisites matrix must have exactly 2 columns")
  } else {
    stop("Unsupported prerequisites type; use matrix/data.frame (2 columns) or a flat numeric vector")
  }
  storage.mode(prerequisites) <- "integer"
  if (nrow(prerequisites) == 0) return(prerequisites)
  # Detect 0-based and normalize to 1-based if needed
  min_val <- min(prerequisites)
  max_val <- max(prerequisites)
  if (min_val == 0 && max_val <= (num_courses - 1)) {
    prerequisites <- prerequisites + 1L
  }
  # Validate range
  if (min(prerequisites) < 1L || max(prerequisites) > num_courses) {
    stop("Course indices out of range after normalization. Ensure 0..n-1 or 1..n indices are used.")
  }
  colnames(prerequisites) <- c("course", "prereq")
  prerequisites
}

# Build adjacency list (as named list of integer vectors) and indegree vector
.build_graph <- function(num_courses, prereq_mat) {
  adj <- vector("list", num_courses)
  names(adj) <- as.character(seq_len(num_courses))
  for (i in seq_len(num_courses)) adj[[i]] <- integer(0)
  indegree <- integer(num_courses)
  if (nrow(prereq_mat) > 0) {
    # Each row: (course, prereq) => edge prereq -> course
    for (r in seq_len(nrow(prereq_mat))) {
      course <- prereq_mat[r, 1]
      prereq <- prereq_mat[r, 2]
      adj[[as.character(prereq)]] <- c(adj[[as.character(prereq)]], course)
      indegree[course] <- indegree[course] + 1L
    }
  }
  list(adj = adj, indegree = indegree)
}

# Kahn's algorithm core: returns topological order (possibly empty if cycle exists)
.kahn_topo_order <- function(num_courses, adj, indegree) {
  # Initialize queue with nodes of indegree 0
  queue <- which(indegree == 0L)
  order <- integer(0)
  # Use simple vector as queue: pop from front by indexing
  while (length(queue) > 0) {
    v <- queue[1]
    queue <- queue[-1]
    order <- c(order, v)
    nbrs <- adj[[as.character(v)]]
    if (length(nbrs) > 0) {
      for (u in nbrs) {
        indegree[u] <- indegree[u] - 1L
        if (indegree[u] == 0L) queue <- c(queue, u)
      }
    }
  }
  order
}

# Public API: return TRUE if all courses can be finished (i.e., acyclic)
can_finish <- function(num_courses, prerequisites) {
  if (!is.numeric(num_courses) || length(num_courses) != 1 || num_courses < 0) {
    stop("num_courses must be a single non-negative number")
  }
  num_courses <- as.integer(num_courses)
  if (num_courses <= 1) return(TRUE)
  prereq_mat <- .normalize_prereqs(num_courses, prerequisites)
  g <- .build_graph(num_courses, prereq_mat)
  order <- .kahn_topo_order(num_courses, g$adj, g$indegree)
  length(order) == num_courses
}

# Public API: find a valid order if possible; otherwise integer(0)
find_course_order <- function(num_courses, prerequisites) {
  if (!is.numeric(num_courses) || length(num_courses) != 1 || num_courses < 0) {
    stop("num_courses must be a single non-negative number")
  }
  num_courses <- as.integer(num_courses)
  if (num_courses == 0) return(integer(0))
  if (num_courses == 1) return(1L)
  prereq_mat <- .normalize_prereqs(num_courses, prerequisites)
  g <- .build_graph(num_courses, prereq_mat)
  order <- .kahn_topo_order(num_courses, g$adj, g$indegree)
  if (length(order) == num_courses) order else integer(0)
}

# -----------------------------
# Examples / Demonstration
# -----------------------------
cat("=== Course Schedule - Kahn's Algorithm (Topological Sort) ===\n")

# Example 1: Simple case with 1-based indices, no cycle
# Prereq pairs (course, prereq): to take course 2 you must finish 1, etc.
num_courses_1 <- 4
prereqs_1 <- matrix(c(
  2, 1,  # 1 -> 2
  3, 1,  # 1 -> 3
  4, 3   # 3 -> 4
), byrow = TRUE, ncol = 2)

cat("\nExample 1 (no cycle):\n")
cat("Can finish? ", can_finish(num_courses_1, prereqs_1), "\n")
order_1 <- find_course_order(num_courses_1, prereqs_1)
cat("One valid order: ", paste(order_1, collapse = " -> "), "\n")

# Example 2: 0-based indices input (LeetCode-like), contains a cycle 0->1->0
num_courses_2 <- 2
prereqs_2 <- matrix(c(
  1, 0, # 0 -> 1
  0, 1  # 1 -> 0 (cycle)
), byrow = TRUE, ncol = 2)

cat("\nExample 2 (cycle present, 0-based input):\n")
cat("Can finish? ", can_finish(num_courses_2, prereqs_2), "\n")
order_2 <- find_course_order(num_courses_2, prereqs_2)
if (length(order_2) == 0) {
  cat("No valid order exists due to cycle.\n")
} else {
  cat("Order: ", paste(order_2, collapse = " -> "), "\n")
}

# Example 3: All independent courses (no prerequisites)
num_courses_3 <- 3
prereqs_3 <- matrix(numeric(0), ncol = 2)
cat("\nExample 3 (no prerequisites):\n")
cat("Can finish? ", can_finish(num_courses_3, prereqs_3), "\n")
order_3 <- find_course_order(num_courses_3, prereqs_3)
cat("A valid order (any permutation of 1..3): ", paste(order_3, collapse = " -> "), "\n")