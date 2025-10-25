# Two-Pointer Technique (Fast and Slow Pointers / Tortoise and Hare)
#
# Template and practical application for singly linked lists using the
# fast & slow pointer technique. This file provides:
# - A minimal Node reference-class local to this file
# - Generic helper: build list from a vector
# - detect_cycle(head): returns TRUE/FALSE using tortoise & hare
# - find_middle(head): returns the middle node's data (for even length returns the second middle)
# - nth_from_end(head, n): returns the data of the Nth node from the end (1-based)
#
# Practical application demonstrated: finding the Nth node from the end.

TwoPtrNode <- setRefClass("TwoPtrNode",
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

# Build a singly linked list from an R vector of values. Returns the head node.
build_list_from_vector <- function(vec) {
  if (length(vec) == 0) return(NULL)
  head <- TwoPtrNode$new(data = vec[1])
  current <- head
  if (length(vec) > 1) {
    for (v in vec[-1]) {
      new_node <- TwoPtrNode$new(data = v)
      current$next_node <- new_node
      current <- new_node
    }
  }
  return(head)
}

# Convert linked list to vector for easy printing/inspection
# This is defensive against cycles by truncating after `max_nodes` items.
list_to_vector <- function(head, max_nodes = 1000) {
  out <- c()
  cur <- head
  i <- 0
  while (!is.null(cur) && i < max_nodes) {
    out <- c(out, cur$data)
    cur <- cur$next_node
    i <- i + 1
  }
  if (!is.null(cur)) {
    out <- c(out, "... (truncated or cycle detected)")
  }
  return(out)
}

# Detect cycle using fast and slow pointers (Tortoise & Hare)
detect_cycle <- function(head) {
  if (is.null(head)) return(FALSE)
  slow <- head
  fast <- head
  while (!is.null(fast) && !is.null(fast$next_node)) {
    slow <- slow$next_node
    fast <- fast$next_node$next_node
    if (identical(slow, fast)) return(TRUE)
  }
  return(FALSE)
}

# Find middle node using two pointers. For even-length lists this returns
# the second middle (i.e., for 1->2->3->4 it returns 3).
find_middle <- function(head) {
  if (is.null(head)) return(NULL)
  slow <- head
  fast <- head
  while (!is.null(fast) && !is.null(fast$next_node)) {
    slow <- slow$next_node
    fast <- fast$next_node$next_node
  }
  return(slow)
}

# Return the data of the Nth node from the end (1-based). Throws an error
# if n is invalid or greater than list length.
nth_from_end <- function(head, n) {
  if (is.null(head)) stop("List is empty")
  if (n <= 0) stop("n must be a positive integer")

  fast <- head
  # Advance fast by n steps
  for (i in seq_len(n)) {
    if (is.null(fast)) stop(sprintf("n (%d) is larger than the list length", n))
    fast <- fast$next_node
  }

  slow <- head
  # Move both until fast is NULL; slow will be at the Nth from end
  while (!is.null(fast)) {
    slow <- slow$next_node
    fast <- fast$next_node
  }
  return(slow$data)
}

# ---- Practical demonstration ----
if (sys.nframe() == 0) {
  cat("Two-Pointer Technique demo\n")
  head <- build_list_from_vector(1:7)
  cat("List:", paste(list_to_vector(head), collapse = " -> "), "\n")

  mid <- find_middle(head)
  cat("Middle node data:", ifelse(is.null(mid), "NULL", mid$data), "\n")

  n <- 2
  nth <- nth_from_end(head, n)
  cat(sprintf("%d-th node from the end: %s\n", n, nth))

  cat("Cycle detected?", detect_cycle(head), "\n")

  # Create a cycle for testing (connect tail to node with data=3)
  tail <- head
  while (!is.null(tail$next_node)) tail <- tail$next_node
  cur <- head
  while (!is.null(cur) && cur$data != 3) cur <- cur$next_node
  if (!is.null(cur)) tail$next_node <- cur
  cat("After creating a cycle (tail -> node with data 3):\n")
  cat("Cycle detected?", detect_cycle(head), "\n")
}