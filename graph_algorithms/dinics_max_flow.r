# Dinic's Maximum Flow Algorithm
#
# Dinic's algorithm finds the maximum flow in a flow network using blocking flows.
# It builds level graphs using BFS and finds blocking flows using DFS, achieving
# better performance than Ford-Fulkerson on many graphs.
#
# Time Complexity: O(V^2 * E)
# Space Complexity: O(V + E)
#
# Input: Flow network with capacities, source and sink vertices
# Output: Maximum flow value from source to sink

# Edge structure for flow network
create_edge <- function(to, capacity, flow = 0, rev_idx = NULL) {
  list(to = to, capacity = capacity, flow = flow, rev_idx = rev_idx)
}

# Flow network structure
create_flow_network <- function(n) {
  graph <- vector("list", n)
  for (i in 1:n) {
    graph[[i]] <- list()
  }
  list(graph = graph, n = n)
}

# Add directed edge with capacity
add_edge <- function(network, u, v, capacity) {
  u_edges <- network$graph[[u]]
  v_edges <- network$graph[[v]]
  
  # Forward edge
  forward_idx <- length(u_edges) + 1
  reverse_idx <- length(v_edges) + 1
  
  # Add forward edge (u -> v) with capacity
  network$graph[[u]][[forward_idx]] <- create_edge(v, capacity, 0, reverse_idx)
  
  # Add reverse edge (v -> u) with 0 capacity (for residual graph)
  network$graph[[v]][[reverse_idx]] <- create_edge(u, 0, 0, forward_idx)
  
  return(network)
}

# BFS to construct level graph
bfs_level_graph <- function(network, source, sink) {
  n <- network$n
  level <- rep(-1, n)
  level[source] <- 0
  
  queue <- c(source)
  head_idx <- 1
  
  while (head_idx <= length(queue)) {
    u <- queue[head_idx]
    head_idx <- head_idx + 1
    
    if (u == sink) break
    
    for (edge in network$graph[[u]]) {
      v <- edge$to
      # Check if edge has residual capacity and v is not visited
      if (level[v] == -1 && edge$capacity > edge$flow) {
        level[v] <- level[u] + 1
        queue <- c(queue, v)
      }
    }
  }
  
  return(level)
}

# DFS to find blocking flow
dfs_blocking_flow <- function(network, u, sink, pushed, level, iter) {
  if (u == sink) {
    return(pushed)
  }
  
  # Try all edges from current vertex
  while (iter[u] <= length(network$graph[[u]])) {
    edge_idx <- iter[u]
    edge <- network$graph[[u]][[edge_idx]]
    v <- edge$to
    
    # Check if edge is in level graph and has residual capacity
    if (level[v] == level[u] + 1 && edge$capacity > edge$flow) {
      # Calculate minimum flow we can push
      flow <- min(pushed, edge$capacity - edge$flow)
      
      # Recursively push flow
      result <- dfs_blocking_flow(network, v, sink, flow, level, iter)
      
      if (result > 0) {
        # Update flow on forward edge
        network$graph[[u]][[edge_idx]]$flow <<- edge$flow + result
        
        # Update flow on reverse edge
        rev_idx <- edge$rev_idx
        rev_edge <- network$graph[[v]][[rev_idx]]
        network$graph[[v]][[rev_idx]]$flow <<- rev_edge$flow - result
        
        return(result)
      }
    }
    
    iter[u] <- iter[u] + 1
  }
  
  return(0)
}

# Dinic's maximum flow algorithm
dinic_max_flow <- function(network, source, sink) {
  if (source == sink) {
    return(0)
  }
  
  max_flow <- 0
  
  # Repeat until no augmenting path exists
  while (TRUE) {
    # Build level graph using BFS
    level <- bfs_level_graph(network, source, sink)
    
    # If sink is not reachable, no more augmenting paths
    if (level[sink] == -1) {
      break
    }
    
    # Find blocking flows using DFS
    iter <- rep(1, network$n)
    
    while (TRUE) {
      pushed <- dfs_blocking_flow(network, source, sink, Inf, level, iter)
      
      if (pushed == 0) {
        break
      }
      
      max_flow <- max_flow + pushed
    }
  }
  
  return(max_flow)
}

# Get minimum cut (vertices reachable from source in residual graph)
get_min_cut <- function(network, source) {
  n <- network$n
  visited <- rep(FALSE, n)
  visited[source] <- TRUE
  
  queue <- c(source)
  head_idx <- 1
  
  while (head_idx <= length(queue)) {
    u <- queue[head_idx]
    head_idx <- head_idx + 1
    
    for (edge in network$graph[[u]]) {
      v <- edge$to
      if (!visited[v] && edge$capacity > edge$flow) {
        visited[v] <- TRUE
        queue <- c(queue, v)
      }
    }
  }
  
  return(which(visited))
}

# Get edges in minimum cut
get_min_cut_edges <- function(network, source) {
  reachable <- get_min_cut(network, source)
  reachable_set <- rep(FALSE, network$n)
  reachable_set[reachable] <- TRUE
  
  cut_edges <- list()
  
  for (u in reachable) {
    for (edge in network$graph[[u]]) {
      v <- edge$to
      if (!reachable_set[v] && edge$capacity > 0) {
        cut_edges[[length(cut_edges) + 1]] <- list(from = u, to = v, capacity = edge$capacity)
      }
    }
  }
  
  return(cut_edges)
}

# Example usage and tests
cat("=== Dinic's Maximum Flow Algorithm ===\n\n")

# Example 1: Simple network
cat("Example 1: Simple Flow Network (6 vertices)\n")
cat("Network structure:\n")
cat("  1 --[10]--> 2 --[10]--> 5\n")
cat("  |           |           ^\n")
cat(" [10]        [2]         [10]\n")
cat("  |           |           |\n")
cat("  v           v           |\n")
cat("  3 --[4]---> 4 --[10]----+\n")
cat("  |                       ^\n")
cat(" [10]                     |\n")
cat("  +----------[10]---------+\n\n")

network1 <- create_flow_network(6)
network1 <- add_edge(network1, 1, 2, 10)
network1 <- add_edge(network1, 1, 3, 10)
network1 <- add_edge(network1, 2, 4, 2)
network1 <- add_edge(network1, 2, 5, 10)
network1 <- add_edge(network1, 3, 4, 4)
network1 <- add_edge(network1, 3, 5, 10)
network1 <- add_edge(network1, 4, 5, 10)

max_flow1 <- dinic_max_flow(network1, 1, 5)
cat("Maximum flow from vertex 1 to vertex 5:", max_flow1, "\n")

min_cut1 <- get_min_cut(network1, 1)
cat("Minimum cut (source side vertices):", paste(min_cut1, collapse = ", "), "\n")

cut_edges1 <- get_min_cut_edges(network1, 1)
cat("Cut edges:\n")
for (edge in cut_edges1) {
  cat(sprintf("  %d -> %d (capacity: %d)\n", edge$from, edge$to, edge$capacity))
}

# Example 2: Bipartite matching
cat("\n\nExample 2: Bipartite Matching Problem\n")
cat("Left set: {1, 2, 3}, Right set: {4, 5, 6}\n")
cat("Edges: 1-4, 1-5, 2-5, 2-6, 3-4, 3-6\n")

network2 <- create_flow_network(8)
# Add source (vertex 7) to left partition
network2 <- add_edge(network2, 7, 1, 1)
network2 <- add_edge(network2, 7, 2, 1)
network2 <- add_edge(network2, 7, 3, 1)

# Add edges between partitions
network2 <- add_edge(network2, 1, 4, 1)
network2 <- add_edge(network2, 1, 5, 1)
network2 <- add_edge(network2, 2, 5, 1)
network2 <- add_edge(network2, 2, 6, 1)
network2 <- add_edge(network2, 3, 4, 1)
network2 <- add_edge(network2, 3, 6, 1)

# Add right partition to sink (vertex 8)
network2 <- add_edge(network2, 4, 8, 1)
network2 <- add_edge(network2, 5, 8, 1)
network2 <- add_edge(network2, 6, 8, 1)

max_flow2 <- dinic_max_flow(network2, 7, 8)
cat("Maximum matching size:", max_flow2, "\n")

# Example 3: Complex network with multiple paths
cat("\n\nExample 3: Complex Network with Multiple Paths\n")
network3 <- create_flow_network(6)
network3 <- add_edge(network3, 1, 2, 16)
network3 <- add_edge(network3, 1, 3, 13)
network3 <- add_edge(network3, 2, 3, 10)
network3 <- add_edge(network3, 2, 4, 12)
network3 <- add_edge(network3, 3, 2, 4)
network3 <- add_edge(network3, 3, 5, 14)
network3 <- add_edge(network3, 4, 3, 9)
network3 <- add_edge(network3, 4, 6, 20)
network3 <- add_edge(network3, 5, 4, 7)
network3 <- add_edge(network3, 5, 6, 4)

max_flow3 <- dinic_max_flow(network3, 1, 6)
cat("Maximum flow from vertex 1 to vertex 6:", max_flow3, "\n")

min_cut3 <- get_min_cut(network3, 1)
cat("Minimum cut (source side vertices):", paste(min_cut3, collapse = ", "), "\n")

# Example 4: Network with bottleneck
cat("\n\nExample 4: Network with Bottleneck Edge\n")
network4 <- create_flow_network(5)
network4 <- add_edge(network4, 1, 2, 100)
network4 <- add_edge(network4, 1, 3, 100)
network4 <- add_edge(network4, 2, 4, 5)  # Bottleneck
network4 <- add_edge(network4, 3, 4, 5)  # Bottleneck
network4 <- add_edge(network4, 4, 5, 100)

max_flow4 <- dinic_max_flow(network4, 1, 5)
cat("Maximum flow from vertex 1 to vertex 5:", max_flow4, "\n")
cat("Note: Flow limited by bottleneck edges (total capacity = 10)\n")

# Example 5: Multi-source multi-sink (converted to single source-sink)
cat("\n\nExample 5: Multi-Source Multi-Sink Problem\n")
cat("Sources: {2, 3}, Sinks: {5, 6}\n")

network5 <- create_flow_network(8)
# Super source (1) to sources
network5 <- add_edge(network5, 1, 2, 15)
network5 <- add_edge(network5, 1, 3, 15)

# Internal edges
network5 <- add_edge(network5, 2, 4, 10)
network5 <- add_edge(network5, 3, 4, 10)
network5 <- add_edge(network5, 4, 5, 8)
network5 <- add_edge(network5, 4, 6, 8)

# Sinks to super sink (7)
network5 <- add_edge(network5, 5, 7, 10)
network5 <- add_edge(network5, 6, 7, 10)

max_flow5 <- dinic_max_flow(network5, 1, 7)
cat("Maximum flow from super-source to super-sink:", max_flow5, "\n")

cat("\n=== All tests completed successfully ===\n")
