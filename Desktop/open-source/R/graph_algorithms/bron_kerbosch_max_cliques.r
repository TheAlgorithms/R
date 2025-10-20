# Bron-Kerbosch Algorithm
#
# Finds all maximal cliques in an undirected graph.
# A clique is a set of vertices where every pair is connected by an edge.
# A maximal clique is a clique that cannot be extended by including an adjacent vertex.

bron_kerbosch <- function(graph) {
  num_vertices <- max(as.numeric(names(graph)))
  cliques <- list()
  
  # Recursive function
  bk <- function(R, P, X) {
    if(length(P) == 0 && length(X) == 0) {
      cliques <<- append(cliques, list(R))
      return()
    }
    for (v in P) {
      N_v <- graph[[as.character(v)]]
      bk(c(R, v), intersect(P, N_v), intersect(X, N_v))
      P <- setdiff(P, v)
      X <- c(X, v)
    }
  }
  
  vertices <- as.numeric(names(graph))
  bk(c(), vertices, c())
  return(cliques)
}

# Example usage
cat("=== Bron-Kerbosch Maximal Cliques Algorithm ===\n")

# Example undirected graph
# 1-2, 1-3, 2-3, 2-4, 3-4
bron_graph <- list(
  "1" = c(2,3),
  "2" = c(1,3,4),
  "3" = c(1,2,4),
  "4" = c(2,3)
)

cat("Graph (adjacency list):\n")
for (v in names(bron_graph)) {
  cat("Vertex", v, "-> [", paste(bron_graph[[v]], collapse = ", "), "]\n")
}

cat("\nFinding all maximal cliques:\n")
cliques <- bron_kerbosch(bron_graph)
for (i in 1:length(cliques)) {
  cat("Clique", i, ":", paste(cliques[[i]], collapse = ", "), "\n")
}
