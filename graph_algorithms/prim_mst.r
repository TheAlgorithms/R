# Prim's Algorithm for Minimum Spanning Tree (MST)
#
# Prim's algorithm finds the MST of a connected, weighted, undirected graph.
# It starts from an arbitrary node and repeatedly adds the smallest edge
# connecting the growing MST to a new vertex.
#
# Time Complexity: O(V^2) using adjacency matrix, O((V + E) log V) with priority queue
# Space Complexity: O(V) for key, parent, and visited arrays
#
# Applications:
# - Network design (telecommunications, computer networks)
# - Circuit design and electrical grids
# - Clustering algorithms in machine learning
# - Transportation and logistics planning
# - Approximation algorithms for NP-hard problems

#' Compute the Minimum Spanning Tree using Prim's algorithm
#' @param graph Adjacency matrix of the graph (use 0 or Inf for no edge)
#' @param start_vertex Starting vertex (default is 1)
#' @return List with MST edges, total weight, and parent array
prim_mst <- function(graph, start_vertex = 1) {
  # Validate input
  if (!is.matrix(graph)) {
    stop("Error: Input must be a matrix")
  }
  
  if (nrow(graph) != ncol(graph)) {
    stop("Error: Graph must be a square adjacency matrix")
  }
  
  n <- nrow(graph)
  
  if (start_vertex < 1 || start_vertex > n) {
    stop(sprintf("Error: start_vertex must be between 1 and %d", n))
  }
  
  # Initialize arrays
  key <- rep(Inf, n)          # Minimum weight to include vertex
  parent <- rep(NA, n)        # Store MST edges
  in_mst <- rep(FALSE, n)     # Vertices included in MST
  
  # Start from specified vertex
  key[start_vertex] <- 0
  parent[start_vertex] <- -1  # Root of MST
  
  # Build MST with n vertices
  for (count in 1:n) {
    # Pick vertex u not in MST with minimum key value
    min_key <- Inf
    u <- NA
    
    for (v in 1:n) {
      if (!in_mst[v] && key[v] < min_key) {
        min_key <- key[v]
        u <- v
      }
    }
    
    # Check if graph is disconnected
    if (is.na(u)) {
      warning("Graph is disconnected. MST is incomplete.")
      break
    }
    
    # Include u in MST
    in_mst[u] <- TRUE
    
    # Update key and parent for adjacent vertices of u
    for (v in 1:n) {
      # Check if there's an edge, v is not in MST, and edge weight is smaller
      if (graph[u, v] != 0 && graph[u, v] != Inf && 
          !in_mst[v] && graph[u, v] < key[v]) {
        key[v] <- graph[u, v]
        parent[v] <- u
      }
    }
  }
  
  # Construct MST edges and calculate total weight
  mst_edges <- list()
  total_weight <- 0
  edge_count <- 0
  
  for (v in 1:n) {
    if (!is.na(parent[v]) && parent[v] != -1) {
      edge_count <- edge_count + 1
      mst_edges[[edge_count]] <- list(
        from = parent[v],
        to = v,
        weight = graph[parent[v], v]
      )
      total_weight <- total_weight + graph[parent[v], v]
    }
  }
  
  return(list(
    edges = mst_edges,
    total_weight = total_weight,
    parent = parent,
    num_edges = edge_count
  ))
}

#' Print MST in a formatted way
#' @param mst Result from prim_mst function
#' @param graph Original graph (for verification)
print_mst <- function(mst, graph = NULL) {
  cat("Minimum Spanning Tree:\n")
  cat(strrep("=", 50), "\n\n")
  
  if (length(mst$edges) == 0) {
    cat("No edges in MST (graph may be disconnected)\n")
    return()
  }
  
  cat(sprintf("%-10s %-10s %-10s\n", "Edge", "Vertices", "Weight"))
  cat(strrep("-", 50), "\n")
  
  for (i in seq_along(mst$edges)) {
    edge <- mst$edges[[i]]
    cat(sprintf("%-10d %-10s %-10g\n", 
                i, 
                paste0(edge$from, " -- ", edge$to),
                edge$weight))
  }
  
  cat(strrep("-", 50), "\n")
  cat(sprintf("Total Weight: %g\n", mst$total_weight))
  cat(sprintf("Number of Edges: %d\n", mst$num_edges))
  
  # Verify MST properties
  if (!is.null(graph)) {
    n <- nrow(graph)
    expected_edges <- n - 1
    if (mst$num_edges == expected_edges) {
      cat(sprintf("✓ MST has correct number of edges (%d)\n", expected_edges))
    } else {
      cat(sprintf("[WARN] MST has %d edges, expected %d (graph may be disconnected)\n", 
                  mst$num_edges, expected_edges))
    }
  }
  cat("\n")
}

#' Create adjacency matrix from edge list
#' @param edges List of edges, each with from, to, and weight
#' @param n_vertices Number of vertices
#' @return Adjacency matrix
create_graph_from_edges <- function(edges, n_vertices) {
  graph <- matrix(0, nrow = n_vertices, ncol = n_vertices)
  
  for (edge in edges) {
    graph[edge$from, edge$to] <- edge$weight
    graph[edge$to, edge$from] <- edge$weight  # Undirected graph
  }
  
  return(graph)
}

# ========== Example 1: Basic 5-vertex graph ==========
cat("========== Example 1: Basic 5-Vertex Graph ==========\n\n")
graph1 <- matrix(c(
  0, 2, 0, 6, 0,
  2, 0, 3, 8, 5,
  0, 3, 0, 0, 7,
  6, 8, 0, 0, 9,
  0, 5, 7, 9, 0
), nrow = 5, byrow = TRUE)
cat("Graph adjacency matrix:\n")
print(graph1)
cat("\n")
mst1 <- prim_mst(graph1)
print_mst(mst1, graph1)

# ========== Example 2: 6-vertex graph ==========
cat("========== Example 2: 6-Vertex Graph ==========\n\n")
graph2 <- matrix(c(
  0, 4, 0, 0, 0, 0,
  4, 0, 8, 0, 0, 0,
  0, 8, 0, 7, 0, 4,
  0, 0, 7, 0, 9, 14,
  0, 0, 0, 9, 0, 10,
  0, 0, 4, 14, 10, 0
), nrow = 6, byrow = TRUE)
cat("Graph adjacency matrix:\n")
print(graph2)
cat("\n")
mst2 <- prim_mst(graph2, start_vertex = 1)
print_mst(mst2, graph2)

# ========== Example 3: Using edge list representation ==========
cat("========== Example 3: Graph from Edge List ==========\n\n")
edges <- list(
  list(from = 1, to = 2, weight = 1),
  list(from = 1, to = 3, weight = 4),
  list(from = 2, to = 3, weight = 2),
  list(from = 2, to = 4, weight = 5),
  list(from = 3, to = 4, weight = 3)
)
graph3 <- create_graph_from_edges(edges, 4)
cat("Graph from edge list:\n")
for (edge in edges) {
  cat(sprintf("%d -- %d : %g\n", edge$from, edge$to, edge$weight))
}
cat("\nAdjacency matrix:\n")
print(graph3)
cat("\n")
mst3 <- prim_mst(graph3)
print_mst(mst3, graph3)

# ========== Example 4: Disconnected graph ==========
cat("========== Example 4: Disconnected Graph ==========\n\n")
graph4 <- matrix(c(
  0, 1, 0, 0,
  1, 0, 0, 0,
  0, 0, 0, 2,
  0, 0, 2, 0
), nrow = 4, byrow = TRUE)
cat("Disconnected graph adjacency matrix:\n")
print(graph4)
cat("\n")
mst4 <- prim_mst(graph4)
print_mst(mst4, graph4)

# ========== Example 5: Complete graph K4 ==========
cat("========== Example 5: Complete Graph K4 ==========\n\n")
graph5 <- matrix(c(
  0, 1, 2, 3,
  1, 0, 4, 5,
  2, 4, 0, 6,
  3, 5, 6, 0
), nrow = 4, byrow = TRUE)
cat("Complete graph K4:\n")
print(graph5)
cat("\n")
mst5 <- prim_mst(graph5)
print_mst(mst5, graph5)

# ========== Notes ==========
cat("========== Algorithm Properties ==========\n\n")
cat("1. Prim's algorithm guarantees finding the MST for connected graphs\n")
cat("2. Works only on undirected graphs with non-negative weights\n")
cat("3. MST has exactly (V-1) edges for a connected graph with V vertices\n")
cat("4. Total weight of MST is unique, but MST itself may not be unique\n")
cat("5. Starting vertex doesn't affect the total weight of the MST\n\n")

cat("========== Comparison with Kruskal's Algorithm ==========\n\n")
cat("Prim's Algorithm:\n")
cat("  - Starts from a vertex and grows the tree\n")
cat("  - Better for dense graphs (many edges)\n")
cat("  - O(V^2) with simple implementation\n\n")
cat("Kruskal's Algorithm:\n")
cat("  - Starts with edges sorted by weight\n")
cat("  - Better for sparse graphs (few edges)\n")
cat("  - O(E log E) or O(E log V)\n")