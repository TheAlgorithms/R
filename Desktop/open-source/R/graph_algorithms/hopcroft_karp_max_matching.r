# Hopcroft-Karp Algorithm for Maximum Bipartite Matching
#
# The Hopcroft-Karp algorithm finds the maximum matching in a bipartite graph.
# It alternates between BFS to find shortest augmenting paths and DFS to augment them.
#
# Time Complexity: O(E * sqrt(V))
# Space Complexity: O(V + E)
#
# Input: bipartite graph as adjacency list (from left partition to right partition)
# Output: Matching as a list where match_left[i] = matched vertex on right (or 0 if unmatched)

hopcroft_karp <- function(graph, num_left, num_right) {
  INF <- 1e9
  match_left <- rep(0, num_left)
  match_right <- rep(0, num_right)
  dist <- rep(0, num_left)
  
  bfs <- function() {
    queue <- c()
    for (u in 1:num_left) {
      if (match_left[u] == 0) {
        dist[u] <<- 0
        queue <- c(queue, u)
      } else {
        dist[u] <<- INF
      }
    }
    found <- FALSE
    while (length(queue) > 0) {
      u <- queue[1]; queue <- queue[-1]
      for (v in graph[[as.character(u)]]) {
        mv <- match_right[v]
        if (mv == 0) {
          found <- TRUE
        } else if (dist[mv] == INF) {
          dist[mv] <<- dist[u] + 1
          queue <- c(queue, mv)
        }
      }
    }
    return(found)
  }
  
  dfs <- function(u) {
    for (v in graph[[as.character(u)]]) {
      mv <- match_right[v]
      if (mv == 0 || (dist[mv] == dist[u] + 1 && dfs(mv))) {
        match_left[u] <<- v
        match_right[v] <<- u
        return(TRUE)
      }
    }
    dist[u] <<- INF
    return(FALSE)
  }
  
  matching <- 0
  while (bfs()) {
    for (u in 1:num_left) {
      if (match_left[u] == 0 && dfs(u)) {
        matching <- matching + 1
      }
    }
  }
  
  return(list(match_left = match_left, match_right = match_right, max_matching = matching))
}

# Example usage
cat("=== Hopcroft-Karp Maximum Bipartite Matching ===\n")

# Bipartite graph: left partition 1..4, right partition 1..4
# Graph edges: 1->1,1->2, 2->1, 3->2,3->3, 4->3,4->4
bipartite_graph <- list(
  "1" = c(1,2),
  "2" = c(1),
  "3" = c(2,3),
  "4" = c(3,4)
)

num_left <- 4
num_right <- 4

cat("Graph (adjacency list from left partition):\n")
for (u in names(bipartite_graph)) {
  edges <- bipartite_graph[[u]]
  cat("Vertex", u, "-> [", paste(edges, collapse = ", "), "]\n")
}

cat("\nRunning Hopcroft-Karp algorithm:\n")
hk_result <- hopcroft_karp(bipartite_graph, num_left, num_right)
cat("Maximum Matching Size:", hk_result$max_matching, "\n")

cat("Matching from left partition:\n")
for (i in 1:num_left) {
  if (hk_result$match_left[i] != 0) {
    cat("Left vertex", i, "-> Right vertex", hk_result$match_left[i], "\n")
  } else {
    cat("Left vertex", i, "is unmatched\n")
  }
}
