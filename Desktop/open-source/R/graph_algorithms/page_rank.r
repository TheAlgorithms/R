# PageRank Algorithm
#
# PageRank computes the importance of each vertex (web page) in a directed graph
# based on the link structure. Pages linked by important pages get higher rank.
#
# Time Complexity: O(V^2) for dense graphs (can be O(E) for sparse graphs using adjacency lists)
# Space Complexity: O(V)
#
# Input: graph as an adjacency list where each entry is a vector of outgoing links
#        damping_factor (default 0.85), max_iterations, tolerance for convergence
# Output: PageRank vector with scores for each vertex

page_rank <- function(graph, damping_factor=0.85, max_iterations=100, tol=1e-6) {
  num_vertices <- max(as.numeric(names(graph)))
  pr <- rep(1/num_vertices, num_vertices)
  
  out_degree <- sapply(graph, length)
  
  for (iter in 1:max_iterations) {
    pr_new <- rep((1 - damping_factor)/num_vertices, num_vertices)
    for (u_char in names(graph)) {
      u <- as.numeric(u_char)
      neighbors <- graph[[u_char]]
      if (length(neighbors) > 0) {
        for (v in neighbors) {
          pr_new[v] <- pr_new[v] + damping_factor * pr[u] / out_degree[u]
        }
      } else {
        # Handle dangling nodes (no outgoing edges)
        pr_new <- pr_new + damping_factor * pr[u] / num_vertices
      }
    }
    if (sum(abs(pr_new - pr)) < tol) break
    pr <- pr_new
  }
  
  return(pr)
}

# Example usage
cat("=== PageRank Algorithm ===\n")

# Example directed graph (adjacency list)
# Graph structure:
# 1 -> 2,3
# 2 -> 3
# 3 -> 1
# 4 -> 3
pr_graph <- list(
  "1" = c(2,3),
  "2" = c(3),
  "3" = c(1),
  "4" = c(3)
)

cat("Graph (adjacency list):\n")
for (v in names(pr_graph)) {
  edges <- pr_graph[[v]]
  if (length(edges) > 0) {
    cat("Vertex", v, "-> [", paste(edges, collapse = ", "), "]\n")
  } else {
    cat("Vertex", v, "-> []\n")
  }
}

cat("\nComputing PageRank:\n")
pr_result <- page_rank(pr_graph)
for (i in 1:length(pr_result)) {
  cat("Vertex", i, ": PageRank =", round(pr_result[i], 4), "\n")
}
