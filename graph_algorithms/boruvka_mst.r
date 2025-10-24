# Boruvka's Minimum Spanning Tree (MST) — improved R translation
#
# Converted from an improved Python implementation: adds path compression in
# union-find, returns whether a union happened, and guards against infinite
# loops on disconnected graphs.

create_graph <- function(V) {
  list(V = as.integer(V), edges = data.frame(u = integer(), v = integer(), w = double(), stringsAsFactors = FALSE))
}

add_edge <- function(graph, u, v, w) {
  # Append an edge. Vertices are 1-based indices for consistency.
  graph$edges <- rbind(graph$edges, data.frame(u = as.integer(u), v = as.integer(v), w = as.numeric(w), stringsAsFactors = FALSE))
  graph
}

boruvka_mst <- function(graph) {
  V <- as.integer(graph$V)
  edges <- graph$edges

  # Union-Find arrays
  parent <- seq_len(V)
  rank <- rep(0L, V)

  find_set <- function(i) {
    # Iterative find with path compression
    root <- i
    while (parent[root] != root) {
      root <- parent[root]
    }
    # path compression
    j <- i
    while (parent[j] != root) {
      nextj <- parent[j]
      parent[j] <<- root
      j <- nextj
    }
    root
  }

  union_set <- function(x, y) {
    xroot <- find_set(x)
    yroot <- find_set(y)
    if (xroot == yroot) return(FALSE)
    if (rank[xroot] < rank[yroot]) {
      parent[xroot] <<- yroot
    } else if (rank[xroot] > rank[yroot]) {
      parent[yroot] <<- xroot
    } else {
      parent[yroot] <<- xroot
      rank[xroot] <<- rank[xroot] + 1L
    }
    TRUE
  }

  num_trees <- V
  mst_weight <- 0
  mst_edges <- data.frame(u = integer(), v = integer(), w = double(), stringsAsFactors = FALSE)

  # Edge case: empty graph
  if (nrow(edges) == 0) {
    cat("No edges in graph.\n")
    return(invisible(list(edges = mst_edges, total_weight = 0)))
  }

  while (num_trees > 1) {
    cheapest <- rep(NA_integer_, V)

    # For every edge, check components and record cheapest edge for each component
    for (i in seq_len(nrow(edges))) {
      u <- edges$u[i]
      v <- edges$v[i]
      w <- edges$w[i]
      set_u <- find_set(u)
      set_v <- find_set(v)

      if (set_u == set_v) next

      if (is.na(cheapest[set_u]) || edges$w[cheapest[set_u]] > w) {
        cheapest[set_u] <- i
      }
      if (is.na(cheapest[set_v]) || edges$w[cheapest[set_v]] > w) {
        cheapest[set_v] <- i
      }
    }

    any_added <- FALSE

    # Add the cheapest edges to MST
    for (node in seq_len(V)) {
      idx <- cheapest[node]
      if (is.na(idx)) next

      u <- edges$u[idx]
      v <- edges$v[idx]
      w <- edges$w[idx]
      set_u <- find_set(u)
      set_v <- find_set(v)

      if (set_u != set_v) {
        if (union_set(set_u, set_v)) {
          mst_weight <- mst_weight + w
          mst_edges <- rbind(mst_edges, data.frame(u = u, v = v, w = w, stringsAsFactors = FALSE))
          num_trees <- num_trees - 1L
          any_added <- TRUE
        }
      }
    }

    # If no edges were added in this pass, the graph is disconnected
    if (!any_added) {
      cat("Graph appears disconnected; stopping. No spanning tree exists that connects all vertices.\n")
      break
    }
  }

  cat("Edges in MST:\n")
  if (nrow(mst_edges) > 0) {
    for (i in seq_len(nrow(mst_edges))) {
      cat(mst_edges$u[i], "--", mst_edges$v[i], "==", mst_edges$w[i], "\n")
    }
  } else {
    cat("(none)\n")
  }
  cat("Total weight of MST:", mst_weight, "\n")

  invisible(list(edges = mst_edges, total_weight = mst_weight))
}

# Example usage and test
cat("=== Boruvka's MST Algorithm (improved) ===\n")
g <- create_graph(4)
g <- add_edge(g, 1, 2, 10)
g <- add_edge(g, 1, 3, 6)
g <- add_edge(g, 1, 4, 5)
g <- add_edge(g, 2, 4, 15)
g <- add_edge(g, 3, 4, 4)

cat("Graph edges:\n")
print(g$edges)
cat("\nComputing MST...\n")
boruvka_mst(g)
