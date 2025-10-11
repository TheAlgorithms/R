# Kruskal's Minimum Spanning Tree (base R, small)
# edges: data.frame or matrix with columns u, v, w (1-based node ids)
# n: number of vertices
kruskalMST <- function(edges, n) {
  edges <- as.data.frame(edges)
  names(edges) <- c("u", "v", "w")
  edges <- edges[order(edges$w), ]

  parent <- seq_len(n)
  rank <- integer(n)

  find <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]  # path compression
      x <- parent[x]
    }
    x
  }
  unite <- function(a, b) {
    ra <- find(a); rb <- find(b)
    if (ra == rb) return(FALSE)
    if (rank[ra] < rank[rb]) parent[ra] <<- rb
    else if (rank[ra] > rank[rb]) parent[rb] <<- ra
    else { parent[rb] <<- ra; rank[ra] <<- rank[ra] + 1L }
    TRUE
  }

  mst <- edges[0, ]
  total <- 0
  for (i in seq_len(nrow(edges))) {
    if (unite(edges$u[i], edges$v[i])) {
      mst <- rbind(mst, edges[i, ])
      total <- total + edges$w[i]
      if (nrow(mst) == n - 1) break
    }
  }
  list(total_weight = total, edges = mst)
}

# --- tiny demo ---
# Graph: 1--(1)--2, 1--(3)--3, 2--(2)--3, 2--(4)--4, 3--(5)--4
E <- data.frame(
  u = c(1, 1, 2, 2, 3),
  v = c(2, 3, 3, 4, 4),
  w = c(1, 3, 2, 4, 5)
)
res <- kruskalMST(E, n = 4)
print(res$total_weight)  # expected 1 + 2 + 4 = 7
print(res$edges)
