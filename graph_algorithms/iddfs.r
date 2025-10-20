# Iterative Deepening Depth-First Search (IDDFS)
#
# IDDFS performs repeated depth-limited DFS from depth = 0..max_depth until a target
# is found. It combines the optimality (in terms of shallowest solution) of BFS with
# the space efficiency of DFS.
#
# Time Complexity: O(b^d) in the worst case, where b is branching factor and d depth
# Space Complexity: O(d) recursion/stack depth

# Formatting
LINE_WIDTH <- 60

print_line <- function(char = "-", width = LINE_WIDTH) {
  cat(strrep(char, width), "\n", sep = "")
}

#' Normalize adjacency list graph
#'
#' Ensures graph is a named list with names "1".."N" where N is the maximum vertex id,
#' and that all adjacency entries are integer vectors (possibly empty). Missing vertices
#' are added with empty adjacency.
#' @param graph A list mapping vertex id (character or numeric) to numeric neighbor vector
#' @return A normalized adjacency list with names "1".."N"
normalize_graph <- function(graph) {
  if (!is.list(graph)) stop("graph must be a list")
  # Collect vertices from names and neighbors
  name_ids <- as.integer(names(graph))
  if (any(is.na(name_ids))) {
    bad_names <- names(graph)[is.na(name_ids)]
    stop(
      sprintf(
        "graph names must be coercible to integers (e.g., '1','2',...). Problematic names: %s",
        paste(bad_names, collapse = ", ")
      )
    )
  }
  neighbor_ids <- unlist(graph, use.names = FALSE)
  if (length(neighbor_ids) == 0) neighbor_ids <- integer(0)
  if (!is.integer(neighbor_ids)) neighbor_ids <- as.integer(neighbor_ids)
  if (any(is.na(neighbor_ids))) stop("neighbors must be numeric/integer ids")
  max_id <- max(c(0L, name_ids, neighbor_ids))
  if (max_id < 1L) {
    # empty graph
    return(setNames(vector("list", 0L), character(0L)))
  }
  adj <- vector("list", max_id)
  for (i in seq_len(max_id)) adj[[i]] <- integer(0)
  # Fill from provided graph
  for (i in seq_along(graph)) {
    vid <- name_ids[i]
    if (!length(graph[[i]])) {
      adj[[vid]] <- integer(0)
    } else {
      nbrs <- as.integer(graph[[i]])
      if (any(is.na(nbrs))) stop("neighbors must be numeric/integer ids")
      # Filter out-of-range neighbors gracefully but warn
      if (length(nbrs)) {
        out_of_range <- nbrs < 1L
        if (any(out_of_range)) {
          warning("Removed neighbors < 1: ", paste(nbrs[out_of_range], collapse = ", "))
          nbrs <- nbrs[!out_of_range]
        }
      }
      adj[[vid]] <- nbrs
    }
  }
  names(adj) <- as.character(seq_len(max_id))
  adj
}

#' Depth-Limited Search (recursive)
#'
#' This version avoids global state and only prevents cycles along the current path.
#' @param graph Normalized adjacency list from normalize_graph
#' @param current Current vertex (integer)
#' @param target Target vertex (integer)
#' @param limit Remaining depth limit (integer >= 0)
#' @param path Vector of vertices along the current path (for cycle avoidance)
#' @return list(found=logical, path=integer vector when found)
.depth_limited_search <- function(graph, current, target, limit, path) {
  # Visit current
  new_path <- c(path, current)
  if (current == target) {
    return(list(found = TRUE, path = new_path))
  }
  if (limit == 0L) {
    return(list(found = FALSE, path = integer(0)))
  }
  # Explore neighbors
  nbrs <- graph[[current]]
  for (nbr in nbrs) {
    # Avoid cycles within the current path
    if (!(nbr %in% new_path)) {
      res <- .depth_limited_search(graph, nbr, target, limit - 1L, new_path)
      if (res$found) return(res)
    }
  }
  return(list(found = FALSE, path = integer(0)))
}

#' Iterative Deepening DFS (IDDFS)
#'
#' @param graph A named list adjacency: names are vertices ("1","2",...), values are integer neighbors
#' @param start Start vertex (integer)
#' @param target Target vertex (integer)
#' @param max_depth Maximum depth to search (integer >= 0)
#' @param verbose If TRUE, prints progress; otherwise silent
#' @return list(found=logical, depth=integer if found, path=integer vector when found)
#' @examples
#' g <- list("1"=c(2,3), "2"=c(4), "3"=c(5), "4"=c(), "5"=c())
#' iddfs(g, start=1, target=5, max_depth=5, verbose=TRUE)
iddfs <- function(graph, start, target, max_depth, verbose = TRUE) {
  adj <- normalize_graph(graph)
  if (!is.numeric(start) || !is.numeric(target)) {
    stop("start and target must be numeric/integer")
  }
  start <- as.integer(start)
  target <- as.integer(target)
  if (length(adj) == 0L) return(list(found = FALSE, depth = NA_integer_, path = integer(0)))
  n <- length(adj)
  if (start < 1L || start > n || target < 1L || target > n) {
    stop(sprintf("start and target must be in [1, %d]", n))
  }
  if (!is.numeric(max_depth) || max_depth < 0) stop("max_depth must be integer >= 0")
  max_depth <- as.integer(max_depth)

  if (verbose) {
    cat("Iterative Deepening DFS\n")
    print_line("=")
  }

  # Early exit if start is target
  if (start == target) {
    if (verbose) {
      cat(sprintf("✓ Target %d found at depth 0\n", target))
      print_line("-")
    }
    return(list(found = TRUE, depth = 0L, path = c(start)))
  }

  for (depth in 0:max_depth) {
    if (verbose) cat(sprintf("Searching at depth limit: %d\n", depth))
    res <- .depth_limited_search(adj, start, target, depth, integer(0))
    if (res$found) {
      if (verbose) {
        cat(sprintf("✓ Target %d found at depth %d\n", target, depth))
        print_line("-")
      }
      return(list(found = TRUE, depth = depth, path = res$path))
    }
  }

  if (verbose) {
    cat(sprintf("✗ Target %d not found up to depth %d\n", target, max_depth))
    print_line("-")
  }
  return(list(found = FALSE, depth = NA_integer_, path = integer(0)))
}

#' Example demonstrations for IDDFS
#' @return NULL (prints results)
example_iddfs <- function() {
  cat("\n========== Example 1: Simple Directed Graph ==========\n")
  graph1 <- list(
    "1" = c(2, 3),
    "2" = c(4),
    "3" = c(5),
    "4" = c(),
    "5" = c()
  )
  print(iddfs(graph1, start = 1, target = 5, max_depth = 5, verbose = TRUE))

  cat("\n========== Example 2: Target Not Found ==========\n")
  graph2 <- list(
    "1" = c(2),
    "2" = c(3),
    "3" = c()
  )
  print(iddfs(graph2, start = 1, target = 6, max_depth = 3, verbose = TRUE))

  cat("\n========== Example 3: Larger Graph ==========\n")
  graph3 <- list(
    "1" = c(2, 3, 4),
    "2" = c(5, 6),
    "3" = c(7),
    "4" = c(8),
    "5" = c(),
    "6" = c(),
    "7" = c(9),
    "8" = c(),
    "9" = c()
  )
  print(iddfs(graph3, start = 1, target = 9, max_depth = 5, verbose = TRUE))

  invisible(NULL)
}

# Uncomment to run examples when sourcing this file interactively
# if (interactive()) example_iddfs()
