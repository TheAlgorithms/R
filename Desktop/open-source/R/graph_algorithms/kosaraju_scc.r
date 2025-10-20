# Kosaraju's Strongly Connected Components (SCC) Algorithm
#
# Kosaraju's algorithm finds all strongly connected components in a directed graph.
# It performs two depth-first searches (DFS):
#   1. DFS on the original graph to compute finishing times of vertices.
#   2. DFS on the transposed graph in the order of decreasing finishing times.
#
# Time Complexity: O(V + E)
# Space Complexity: O(V + E)
#
# Input: graph as an adjacency list where each entry is a list of vertices it points to
# Output: A list of SCCs (each SCC is a vector of vertices)

kosaraju_scc <- function(graph) {
  
  num_vertices <- max(as.numeric(names(graph)))
  visited <- rep(FALSE, num_vertices)
  finish_stack <- c()
  
  # DFS to compute finishing times
  dfs1 <- function(u) {
    visited[u] <<- TRUE
    for (v in graph[[as.character(u)]]) {
      if (!visited[v]) dfs1(v)
    }
    finish_stack <<- c(u, finish_stack)  # push to stack
  }
  
  # Step 1: Run DFS on all vertices
  for (u in 1:num_vertices) {
    if (!visited[u]) dfs1(u)
  }
  
  # Step 2: Transpose the graph
  transposed <- vector("list", num_vertices)
  for (i in 1:num_vertices) transposed[[i]] <- c()
  
  for (u_char in names(graph)) {
    u <- as.numeric(u_char)
    for (v in graph[[u_char]]) {
      transposed[[v]] <- c(transposed[[v]], u)
    }
  }
  
  # Step 3: DFS on transposed graph in order of decreasing finish time
  visited <- rep(FALSE, num_vertices)
  scc_list <- list()
  
  dfs2 <- function(u, current_scc) {
    visited[u] <<- TRUE
    current_scc <<- c(current_scc, u)
    for (v in transposed[[u]]) {
      if (!visited[v]) current_scc <<- dfs2(v, current_scc)
    }
    return(current_scc)
  }
  
  for (u in finish_stack) {
    if (!visited[u]) {
      scc <- dfs2(u, c())
      scc_list <- append(scc_list, list(scc))
    }
  }
  
  return(scc_list)
}

# Example usage
cat("=== Kosaraju's Strongly Connected Components (SCC) Algorithm ===\n")

# Example directed graph
# Graph structure:
# 1 -> 2
# 2 -> 3
# 3 -> 1, 4
# 4 -> 5
# 5 -> 6
# 6 -> 4, 7
# 7 -> (none)
kosaraju_graph <- list(
  "1" = c(2),
  "2" = c(3),
  "3" = c(1, 4),
  "4" = c(5),
  "5" = c(6),
  "6" = c(4, 7),
  "7" = c()
)

cat("Graph (adjacency list):\n")
for (v in names(kosaraju_graph)) {
  edges <- kosaraju_graph[[v]]
  if (length(edges) > 0) {
    cat("Vertex", v, "-> [", paste(edges, collapse = ", "), "]\n")
  } else {
    cat("Vertex", v, "-> []\n")
  }
}

cat("\nFinding strongly connected components:\n")
scc_result <- kosaraju_scc(kosaraju_graph)
for (i in 1:length(scc_result)) {
  cat("SCC", i, ":", paste(scc_result[[i]], collapse = ", "), "\n")
}
