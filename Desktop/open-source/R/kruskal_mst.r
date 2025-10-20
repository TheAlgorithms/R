# Kruskal's Minimum Spanning Tree (MST) Algorithm
#
# Kruskal's algorithm finds a subset of edges that connects all vertices in a graph
# without any cycles and with the minimum possible total edge weight.
#
# It uses the Disjoint Set Union (DSU) or Union-Find data structure to detect cycles efficiently.
#
# Time Complexity: O(E log E) — dominated by sorting edges
# Space Complexity: O(V)
#
# Input: graph as a list of edges (each edge has `u`, `v`, and `weight`)
# Output: List containing MST edges and total minimum cost

# -------------------------
# Helper: Disjoint Set (Union-Find)
# -------------------------

make_set <- function(n) {
  parent <- 1:n
  rank <- rep(0, n)
  return(list(parent = parent, rank = rank))
}

find_set <- function(parent, v) {
  if (parent[v] != v) {
    parent[v] <- find_set(parent, parent[v])  # Path compression
  }
  return(parent[v])
}

union_sets <- function(parent, rank, a, b) {
  a_root <- find_set(parent, a)
  b_root <- find_set(parent, b)
  
  if (a_root != b_root) {
    if (rank[a_root] < rank[b_root]) {
      temp <- a_root
      a_root <- b_root
      b_root <- temp
    }
    parent[b_root] <- a_root
    if (rank[a_root] == rank[b_root]) {
      rank[a_root] <- rank[a_root] + 1
    }
  }
  return(list(parent = parent, rank = rank))
}

# -------------------------
# Kruskal's MST Algorithm
# -------------------------

kruskal_mst <- function(edges, num_vertices) {
  # Sort edges by weight
  edges <- edges[order(sapply(edges, function(e) e$weight))]
  
  # Initialize disjoint sets
  dsu <- make_set(num_vertices)
  parent <- dsu$parent
  rank <- dsu$rank
  
  mst_edges <- list()
  total_cost <- 0
  
  # Process each edge
  for (edge in edges) {
    u <- edge$u
    v <- edge$v
    w <- edge$weight
    
    u_root <- find_set(parent, u)
    v_root <- find_set(parent, v)
    
    # If u and v are in different sets, include this edge
    if (u_root != v_root) {
      mst_edges <- append(mst_edges, list(edge))
      total_cost <- total_cost + w
      merged <- union_sets(parent, rank, u_root, v_root)
      parent <- merged$parent
      rank <- merged$rank
    }
  }
  
  return(list(
    mst_edges = mst_edges,
    total_cost = total_cost
  ))
}

# -------------------------
# Example usage and testing
# -------------------------

cat("=== Kruskal's Minimum Spanning Tree Algorithm ===\n")

# Example undirected weighted graph (edges list)
# Graph:
# 1 --(4)-- 2
# 1 --(3)-- 3
# 2 --(1)-- 3
# 2 --(2)-- 4
# 3 --(5)-- 4
edges <- list(
  list(u = 1, v = 2, weight = 4),
  list(u = 1, v = 3, weight = 3),
  list(u = 2, v = 3, weight = 1),
  list(u = 2, v = 4, weight = 2),
  list(u = 3, v = 4, weight = 5)
)

cat("Graph edges:\n")
for (e in edges) {
  cat(paste0("(", e$u, " -- ", e$v, ") weight = ", e$weight, "\n"))
}

cat("\nRunning Kruskal's MST:\n")
result <- kruskal_mst(edges, num_vertices = 4)

cat("MST Edges:\n")
for (e in result$mst_edges) {
  cat(paste0("(", e$u, " -- ", e$v, ") weight = ", e$weight, "\n"))
}

cat("Total Minimum Cost:", result$total_cost, "\n")
