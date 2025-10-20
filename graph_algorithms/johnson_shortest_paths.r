# Johnson's All-Pairs Shortest Paths Algorithm
#
# Johnson's algorithm computes shortest paths between all pairs of vertices
# in a sparse, weighted directed graph that may have negative edge weights
# (but no negative cycles). It reweights edges using Bellman-Ford to remove
# negative weights and then runs Dijkstra from each vertex.
#
# Time Complexity: O(V * E + V * (E log V)) with a binary heap priority queue
# Space Complexity: O(V + E)
#
# Graph representation matches other files in this folder:
# - Adjacency list: a named list where each name is a vertex index as a string
# - Each entry is a list of edges: list(vertex = <int>, weight = <numeric>)

# ----------------------------
# Priority Queue (simple) API
# ----------------------------
create_priority_queue <- function() {
  list(
    elements = data.frame(vertex = integer(0), distance = numeric(0)),
    size = 0
  )
}

pq_insert <- function(pq, vertex, distance) {
  pq$elements <- rbind(pq$elements, data.frame(vertex = vertex, distance = distance))
  pq$size <- pq$size + 1
  return(pq)
}

pq_extract_min <- function(pq) {
  if (pq$size == 0) {
    return(list(pq = pq, min_element = NULL))
  }
  min_idx <- which.min(pq$elements$distance)
  min_element <- pq$elements[min_idx, ]
  pq$elements <- pq$elements[-min_idx, ]
  pq$size <- pq$size - 1
  return(list(pq = pq, min_element = min_element))
}

pq_is_empty <- function(pq) {
  return(pq$size == 0)
}

# -----------------------------------------------
# Bellman-Ford potentials (no explicit supersource)
# -----------------------------------------------
# Equivalent to adding a new super-source s with 0-weight edges to all vertices,
# by initializing h[i] = 0 for all i and relaxing edges (V-1) times.
bellman_ford_potentials <- function(graph) {
  # Collect all vertices appearing as sources or targets
  all_vertices <- unique(c(names(graph), unlist(lapply(graph, function(x) sapply(x, function(e) e$vertex)))))
  all_vertices <- as.numeric(all_vertices)
  V <- max(all_vertices)

  # Initialize h with zeros (super-source trick)
  h <- rep(0, V)

  # Relax edges V-1 times
  for (i in 1:(V - 1)) {
    updated <- FALSE
    for (u_char in as.character(1:V)) {
      if (!(u_char %in% names(graph))) next
      u <- as.numeric(u_char)
      for (edge in graph[[u_char]]) {
        v <- edge$vertex
        w <- edge$weight
        if (h[u] + w < h[v]) {
          h[v] <- h[u] + w
          updated <- TRUE
        }
      }
    }
    if (!updated) break
  }

  # Check for negative-weight cycles
  negative_cycle <- FALSE
  for (u_char in as.character(1:V)) {
    if (!(u_char %in% names(graph))) next
    u <- as.numeric(u_char)
    for (edge in graph[[u_char]]) {
      v <- edge$vertex
      w <- edge$weight
      if (h[u] + w < h[v]) {
        negative_cycle <- TRUE
        break
      }
    }
    if (negative_cycle) break
  }

  list(h = h, V = V, negative_cycle = negative_cycle)
}

# ---------------------------
# Dijkstra on reweighted graph
# ---------------------------
dijkstra_on_adj <- function(graph, source, V) {
  distances <- rep(Inf, V)
  previous <- rep(-1, V)
  visited <- rep(FALSE, V)

  distances[source] <- 0
  pq <- create_priority_queue()
  pq <- pq_insert(pq, source, 0)

  while (!pq_is_empty(pq)) {
    res <- pq_extract_min(pq)
    pq <- res$pq
    cur <- res$min_element
    if (is.null(cur)) break

    u <- cur$vertex
    if (visited[u]) next
    visited[u] <- TRUE

    u_char <- as.character(u)
    if (u_char %in% names(graph)) {
      for (edge in graph[[u_char]]) {
        v <- edge$vertex
        w <- edge$weight
        if (!visited[v] && distances[u] + w < distances[v]) {
          distances[v] <- distances[u] + w
          previous[v] <- u
          pq <- pq_insert(pq, v, distances[v])
        }
      }
    }
  }

  list(distances = distances, previous = previous)
}

# ---------------------
# Johnson's main driver
# ---------------------
johnson_shortest_paths <- function(graph) {
  # Ensure all vertices 1..V exist as keys (even if empty list)
  all_vertices <- unique(c(names(graph), unlist(lapply(graph, function(x) sapply(x, function(e) e$vertex)))))
  all_vertices <- as.numeric(all_vertices)
  V <- max(all_vertices)
  for (u in as.character(1:V)) {
    if (!(u %in% names(graph))) graph[[u]] <- list()
  }

  # Step 1: Bellman-Ford to get potentials h
  bf <- bellman_ford_potentials(graph)
  if (bf$negative_cycle) {
    return(list(
      distances = NULL,
      negative_cycle = TRUE,
      message = "Graph contains a negative-weight cycle; shortest paths undefined"
    ))
  }
  h <- bf$h

  # Step 2: Reweight edges to eliminate negative weights
  reweighted <- vector("list", V)
  names(reweighted) <- as.character(1:V)
  for (i in 1:V) reweighted[[i]] <- list()
  for (u_char in as.character(1:V)) {
    u <- as.numeric(u_char)
    for (edge in graph[[u_char]]) {
      v <- edge$vertex
      w <- edge$weight
      w_prime <- w + h[u] - h[v]
      reweighted[[u_char]] <- append(reweighted[[u_char]], list(list(vertex = v, weight = w_prime)))
    }
  }

  # Step 3: Run Dijkstra from each vertex on the reweighted graph
  dist_matrix <- matrix(Inf, nrow = V, ncol = V)
  for (s in 1:V) {
    dj <- dijkstra_on_adj(reweighted, s, V)
    # Convert distances back to original weights
    for (v in 1:V) {
      if (!is.infinite(dj$distances[v])) {
        dist_matrix[s, v] <- dj$distances[v] - h[s] + h[v]
      }
    }
  }

  list(
    distances = dist_matrix,
    negative_cycle = FALSE
  )
}

# -----------------
# Example / Demo
# -----------------
cat("=== Johnson's All-Pairs Shortest Paths Algorithm ===\n")

# Convert the Java example (0-based) to 1-based indices used here
# Java edges:
# 0->1(3), 0->2(8), 0->4(-4), 1->3(1), 1->4(7), 2->1(4), 3->0(2), 3->2(-5), 4->3(6)
j_graph <- list(
  "1" = list(list(vertex = 2, weight = 3), list(vertex = 3, weight = 8), list(vertex = 5, weight = -4)),
  "2" = list(list(vertex = 4, weight = 1), list(vertex = 5, weight = 7)),
  "3" = list(list(vertex = 2, weight = 4)),
  "4" = list(list(vertex = 1, weight = 2), list(vertex = 3, weight = -5)),
  "5" = list(list(vertex = 4, weight = 6))
)

cat("Running Johnson on 5-vertex graph (with negative edges, no negative cycles) ...\n")
j_res <- johnson_shortest_paths(j_graph)
if (isTRUE(j_res$negative_cycle)) {
  cat("Negative cycle detected. Shortest paths undefined.\n")
} else {
  cat("All-pairs shortest path distance matrix (rows = sources, cols = targets):\n")
  print(j_res$distances)
}
