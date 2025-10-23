# Louvain Method for Community Detection
#
# Detects communities (clusters) in a weighted or unweighted undirected graph.
# It optimizes modularity by iteratively merging nodes into communities
# to maximize modularity gain.

louvain_community <- function(graph, max_iter = 100) {
  num_vertices <- max(as.numeric(names(graph)))
  community <- 1:num_vertices
  
  modularity <- function(graph, community) {
    m <- 0
    e_in <- 0
    deg <- rep(0, length(graph))
    
    for (i in names(graph)) {
      deg[i] <- sum(graph[[i]])
      m <- m + sum(graph[[i]])
    }
    m <- m / 2
    
    for (i in names(graph)) {
      for (j in graph[[i]]) {
        if (community[as.numeric(i)] == community[j]) {
          e_in <- e_in + 1
        }
      }
    }
    e_in <- e_in / 2
    Q <- e_in / m - sum(sapply(unique(community), function(c) {
      sum(deg[community == c]) / (2*m)
    })^2)
    return(Q)
  }
  
  for (iter in 1:max_iter) {
    changed <- FALSE
    for (i in 1:num_vertices) {
      best_comm <- community[i]
      best_gain <- 0
      comms <- unique(community)
      for (c in comms) {
        temp_comm <- community
        temp_comm[i] <- c
        gain <- modularity(graph, temp_comm) - modularity(graph, community)
        if (gain > best_gain) {
          best_gain <- gain
          best_comm <- c
        }
      }
      if (community[i] != best_comm) {
        community[i] <- best_comm
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  
  return(community)
}

# Example usage
cat("=== Louvain Method for Community Detection ===\n")

# Example undirected graph as adjacency list
# 1-2, 1-3, 2-3, 2-4, 3-4, 4-5
louvain_graph <- list(
  "1" = c(2,3),
  "2" = c(1,3,4),
  "3" = c(1,2,4),
  "4" = c(2,3,5),
  "5" = c(4)
)

cat("Graph (adjacency list):\n")
for (v in names(louvain_graph)) {
  cat("Vertex", v, "-> [", paste(louvain_graph[[v]], collapse = ", "), "]\n")
}

cat("\nDetecting communities:\n")
communities <- louvain_community(louvain_graph)
for (i in 1:length(communities)) {
  cat("Vertex", i, "-> Community", communities[i], "\n")
}
