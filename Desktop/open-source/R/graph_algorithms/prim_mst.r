# Prim's Minimum Spanning Tree Algorithm
#
# The Prim algorithm finds the Minimum Spanning Tree (MST) of a connected, undirected, weighted graph.
# It starts from a source vertex and grows the MST by adding the smallest-weight edge connecting
# the MST to a new vertex.
#
# Time Complexity: O(V^2) with adjacency matrix, can be O(E log V) with priority queue
# Space Complexity: O(V)
#
# Input: graph as an adjacency list where each entry is a list of edges with fields
#        `vertex` and `weight`, and `source` vertex index (integer)
# Output: A list containing MST edges, total weight

prim_mst <- function(graph, source = 1) {
  all_vertices <- unique(c(names(graph), unlist(lapply(graph, function(x) sapply(x, function(e) e$vertex)))))
  all_vertices <- as.numeric(all_vertices)
  num_vertices <- max(all_vertices)

  in_mst <- rep(FALSE, num_vertices)
  key <- rep(Inf, num_vertices)
  parent <- rep(-1, num_vertices)

  key[source] <- 0

  for (i in 1:num_vertices) {
    # Pick the minimum key vertex not in MST
    u <- which.min(ifelse(in_mst, Inf, key))

    in_mst[u] <- TRUE

    # Update keys and parents for adjacent vertices
    for (edge in graph[[as.character(u)]]) {
      v <- edge$vertex
      w <- edge$weight
      if (!in_mst[v] && w < key[v]) {
        key[v] <- w
        parent[v] <- u
      }
    }
  }

  # Build MST edges
  mst_edges <- list()
  total_weight <- 0
  for (v in 1:num_vertices) {
    if (parent[v] != -1) {
      mst_edges <- append(mst_edges, list(list(from = parent[v], to = v, weight = key[v])))
      total_weight <- total_weight + key[v]
    }
  }

  return(list(
    edges = mst_edges,
    total_weight = total_weight
  ))
}

# Example usage
cat("=== Prim's Minimum Spanning Tree Algorithm ===\n")

# Example undirected graph
# Graph structure:
# 1 --2--> 2
# 1 --3--> 3
# 2 --1--> 3
# 2 --4--> 4
# 3 --5--> 4
prim_graph <- list(
  "1" = list(list(vertex = 2, weight = 2), list(vertex = 3, weight = 3)),
  "2" = list(list(vertex = 1, weight = 2), list(vertex = 3, weight = 1), list(vertex = 4, weight = 4)),
  "3" = list(list(vertex = 1, weight = 3), list(vertex = 2, weight = 1), list(vertex = 4, weight = 5)),
  "4" = list(list(vertex = 2, weight = 4), list(vertex = 3, weight = 5))
)

cat("Graph (adjacency list):\n")
for (v in names(prim_graph)) {
  edges <- prim_graph[[v]]
  edge_strs <- sapply(edges, function(e) paste0(e$vertex, "(", e$weight, ")"))
  cat("Vertex", v, "-> [", paste(edge_strs, collapse = ", "), "]\n")
}

cat("\nRunning Prim's MST from vertex 1:\n")
mst_result <- prim_mst(prim_graph, 1)
cat("MST edges:\n")
for (edge in mst_result$edges) {
  cat(edge$from, "--", edge$weight, "-->", edge$to, "\n")
}
cat("Total weight of MST:", mst_result$total_weight, "\n")
