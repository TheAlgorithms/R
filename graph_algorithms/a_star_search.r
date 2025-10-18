# A* (A-star) Search Algorithm
#
# A* finds the least-cost path from a start node to a target node in a weighted graph.
# It combines the path cost from the start (g-score) with a heuristic estimate (h-score)
# to the goal: f = g + h. With an admissible and consistent heuristic, A* is optimal.
#
# Time Complexity: O((V + E) log V) with a binary heap priority queue
# Space Complexity: O(V)
#
# Graph Input: adjacency list like other files in this folder, where each entry is a list of
#              edges with fields `vertex` and `weight`. Vertices are numeric indices.
# Heuristic: a function h(v) that estimates the remaining cost from v to the goal. By default
#            it is 0 for all vertices (A* reduces to Dijkstra).
# Output: A list containing g_scores (distances), f_scores, predecessor, a found flag, and path.

# ---------------------------
# Priority queue (min-heap-ish) using data.frame for clarity (educational)
# ---------------------------
create_priority_queue <- function() {
  list(
    elements = data.frame(vertex = integer(0), f = numeric(0), g = numeric(0), h = numeric(0)),
    size = 0
  )
}

pq_insert <- function(pq, vertex, f, g, h) {
  pq$elements <- rbind(pq$elements, data.frame(vertex = vertex, f = f, g = g, h = h))
  pq$size <- pq$size + 1
  pq
}

pq_extract_min <- function(pq) {
  if (pq$size == 0) {
    return(list(pq = pq, min_element = NULL))
  }
  min_idx <- which.min(pq$elements$f)
  min_element <- pq$elements[min_idx, ]
  pq$elements <- pq$elements[-min_idx, ]
  pq$size <- pq$size - 1
  list(pq = pq, min_element = min_element)
}

pq_is_empty <- function(pq) {
  pq$size == 0
}

# ---------------------------
# Main A* implementation over adjacency-list graph
# ---------------------------
a_star_search <- function(graph, start, goal, heuristic = function(v) 0) {
  # Collect all vertices (numeric indices expected as in other files)
  all_vertices <- unique(c(names(graph), unlist(lapply(graph, function(x) sapply(x, function(e) e$vertex)))))
  all_vertices <- as.numeric(all_vertices)
  num_vertices <- max(all_vertices)

  # Initialize scores and bookkeeping
  g_scores <- rep(Inf, num_vertices)   # cost from start
  f_scores <- rep(Inf, num_vertices)   # g + h
  predecessor <- rep(-1, num_vertices)
  closed <- rep(FALSE, num_vertices)

  g_scores[start] <- 0
  f_scores[start] <- heuristic(start)

  pq <- create_priority_queue()
  pq <- pq_insert(pq, start, f_scores[start], g_scores[start], heuristic(start))

  found <- FALSE

  while (!pq_is_empty(pq)) {
    res <- pq_extract_min(pq)
    pq <- res$pq
    current <- res$min_element
    if (is.null(current)) break

    u <- as.integer(current$vertex)
    if (closed[u]) next

    # If target popped, we're done
    if (u == goal) {
      found <- TRUE
      break
    }

    closed[u] <- TRUE

    # Explore neighbors
    if (as.character(u) %in% names(graph)) {
      for (edge in graph[[as.character(u)]]) {
        v <- edge$vertex
        w <- edge$weight

        if (closed[v]) next

        tentative_g <- g_scores[u] + w
        if (tentative_g < g_scores[v]) {
          predecessor[v] <- u
          g_scores[v] <- tentative_g
          f_scores[v] <- tentative_g + heuristic(v)
          pq <- pq_insert(pq, v, f_scores[v], g_scores[v], heuristic(v))
        }
      }
    }
  }

  # Build path if found or reachable
  path <- NULL
  if (is.finite(g_scores[goal])) {
    found <- TRUE
    path <- reconstruct_a_star_path(predecessor, start, goal)
  }

  list(
    g_scores = g_scores,
    f_scores = f_scores,
    predecessor = predecessor,
    found = found,
    path = path
  )
}

reconstruct_a_star_path <- function(predecessor, start, goal) {
  path <- c()
  cur <- goal
  while (cur != -1) {
    path <- c(cur, path)
    if (cur == start) break
    cur <- predecessor[cur]
  }
  path
}

# ---------------------------
# Grid helpers (to mirror the provided Java example on a 2D grid)
# ---------------------------
# Convert a 2D grid (matrix with 1 as free cell, 0 as blocked) to an adjacency list graph
# Returns: list(graph = adjacency_list, index_of = function(r,c) -> vertex, coords = data.frame(row,col))
grid_to_graph <- function(grid) {
  stopifnot(is.matrix(grid))
  nrow_g <- nrow(grid)
  ncol_g <- ncol(grid)

  index_of <- function(r, c) {
    # 1-based indexing for vertices
    (r - 1) * ncol_g + c
  }

  coords <- data.frame(row = integer(), col = integer())
  adj <- list()

  dirs <- rbind(c(-1, 0), c(1, 0), c(0, -1), c(0, 1)) # up, down, left, right

  for (r in 1:nrow_g) {
    for (c in 1:ncol_g) {
      v <- index_of(r, c)
      coords[v, c("row", "col")] <- c(r, c)
      if (grid[r, c] == 1) {
        edges <- list()
        for (k in 1:nrow(dirs)) {
          nr <- r + dirs[k, 1]
          nc <- c + dirs[k, 2]
          if (nr >= 1 && nr <= nrow_g && nc >= 1 && nc <= ncol_g && grid[nr, nc] == 1) {
            edges[[length(edges) + 1]] <- list(vertex = index_of(nr, nc), weight = 1)
          }
        }
        adj[[as.character(v)]] <- edges
      } else {
        adj[[as.character(v)]] <- list()
      }
    }
  }

  list(graph = adj, index_of = index_of, coords = coords)
}

# Manhattan heuristic factory for grid graphs
make_manhattan_heuristic <- function(goal_vertex, coords) {
  function(v) {
    dv <- coords[v, ]
    dg <- coords[goal_vertex, ]
    abs(dv$row - dg$row) + abs(dv$col - dg$col)
  }
}

# ---------------------------
# Example usage and tests
# ---------------------------
cat("=== A* (A-star) Search Algorithm ===\n")

# Example 1: Grid-based example mirroring the provided Java snippet
cat("\n-- Grid example (5x5, 4-neighborhood, Manhattan heuristic) --\n")
grid <- matrix(
  c(1, 1, 1, 1, 1,
    0, 1, 0, 1, 0,
    1, 1, 1, 1, 1,
    1, 0, 0, 0, 1,
    1, 1, 1, 1, 1),
  nrow = 5, ncol = 5, byrow = TRUE
)

gg <- grid_to_graph(grid)
start_rc <- c(1, 1)  # row, col (1-based)
goal_rc  <- c(5, 5)
start_v <- gg$index_of(start_rc[1], start_rc[2])
goal_v  <- gg$index_of(goal_rc[1], goal_rc[2])
h_manhattan <- make_manhattan_heuristic(goal_v, gg$coords)

cat("Running A* from (", start_rc[1], ", ", start_rc[2], ") to (", goal_rc[1], ", ", goal_rc[2], ")\n", sep = "")
astar_res <- a_star_search(gg$graph, start_v, goal_v, heuristic = h_manhattan)

if (astar_res$found && !is.null(astar_res$path)) {
  cat("Path found (as grid coordinates):\n")
  for (v in astar_res$path) {
    rc <- gg$coords[v, ]
    cat("(", rc$row, ", ", rc$col, ")\n", sep = "")
  }
  cat("Total steps:", length(astar_res$path) - 1, "\n")
} else {
  cat("No path found!\n")
}

# Example 2: Generic adjacency list (A* with zero heuristic behaves like Dijkstra)
cat("\n-- Adjacency list example (zero heuristic -> Dijkstra behavior) --\n")
weighted_graph <- list(
  "1" = list(list(vertex = 2, weight = 1), list(vertex = 3, weight = 4)),
  "2" = list(list(vertex = 3, weight = 2), list(vertex = 4, weight = 5)),
  "3" = list(list(vertex = 4, weight = 1)),
  "4" = list()
)

start <- 1
goal <- 4
res2 <- a_star_search(weighted_graph, start, goal)
if (res2$found) {
  cat("Shortest path from", start, "to", goal, ": ", paste(res2$path, collapse = " -> "),
      " (distance:", res2$g_scores[goal], ")\n", sep = "")
}
