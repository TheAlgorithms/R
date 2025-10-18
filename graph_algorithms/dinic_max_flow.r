# Dinic's Algorithm for Maximum Flow
#
# Dinic's algorithm finds the maximum flow in a flow network from source to sink.
# It uses level graphs and blocking flows to achieve better time complexity than
# Ford-Fulkerson. The algorithm repeatedly finds shortest augmenting paths using BFS
# and then uses DFS to send flow along multiple paths in each iteration.
#
# Time Complexity: O(V^2 * E) general case, O(E * sqrt(V)) for unit capacity networks
# Space Complexity: O(V + E) for adjacency list and level array
#
# Applications:
# - Network routing and traffic optimization
# - Bipartite matching problems
# - Image segmentation
# - Airline scheduling
# - Project selection and resource allocation

#' Create an empty flow network
#' @param n: Number of vertices
#' @return: Flow network structure with adjacency list and capacity matrix
create_flow_network <- function(n) {
  list(
    n = n,
    graph = vector("list", n),  # Adjacency list
    capacity = matrix(0, nrow = n, ncol = n),  # Capacity matrix
    flow = matrix(0, nrow = n, ncol = n)  # Flow matrix
  )
}

#' Add edge to flow network
#' @param network: Flow network structure
#' @param from: Source vertex (1-indexed)
#' @param to: Destination vertex (1-indexed)
#' @param capacity: Edge capacity
#' @return: Updated network
add_edge <- function(network, from, to, capacity) {
  # Add forward edge
  network$graph[[from]] <- c(network$graph[[from]], to)
  network$capacity[from, to] <- network$capacity[from, to] + capacity
  
  # Add reverse edge for residual graph (with 0 capacity initially)
  if (!(from %in% network$graph[[to]])) {
    network$graph[[to]] <- c(network$graph[[to]], from)
  }
  
  return(network)
}

#' Build level graph using BFS
#' @param network: Flow network
#' @param source: Source vertex
#' @param sink: Sink vertex
#' @return: Level array (-1 if vertex not reachable)
bfs_level_graph <- function(network, source, sink) {
  n <- network$n
  level <- rep(-1, n)
  level[source] <- 0
  
  queue <- c(source)
  
  while (length(queue) > 0) {
    u <- queue[1]
    queue <- queue[-1]
    
    for (v in network$graph[[u]]) {
      # Check if edge has residual capacity and v is not visited
      residual_capacity <- network$capacity[u, v] - network$flow[u, v]
      
      if (level[v] == -1 && residual_capacity > 0) {
        level[v] <- level[u] + 1
        queue <- c(queue, v)
      }
    }
  }
  
  return(level)
}

#' Send flow using DFS (blocking flow)
#' @param network: Flow network
#' @param u: Current vertex
#' @param sink: Sink vertex
#' @param level: Level array from BFS
#' @param flow: Flow to push
#' @param start: Array tracking next edge to try from each vertex
#' @return: Flow pushed
dfs_send_flow <- function(network, u, sink, level, flow, start) {
  # Reached sink
  if (u == sink) {
    return(flow)
  }
  
  # Try all edges from current vertex
  while (start[u] <= length(network$graph[[u]])) {
    v <- network$graph[[u]][start[u]]
    residual_capacity <- network$capacity[u, v] - network$flow[u, v]
    
    # Check if this edge can be used (level increases and has capacity)
    if (level[v] == level[u] + 1 && residual_capacity > 0) {
      # Recursively send flow
      pushed_flow <- dfs_send_flow(
        network, 
        v, 
        sink, 
        level, 
        min(flow, residual_capacity),
        start
      )
      
      if (pushed_flow > 0) {
        # Update flow
        network$flow[u, v] <<- network$flow[u, v] + pushed_flow
        network$flow[v, u] <<- network$flow[v, u] - pushed_flow
        return(pushed_flow)
      }
    }
    
    start[u] <- start[u] + 1
  }
  
  return(0)
}

#' Dinic's algorithm to find maximum flow
#' @param network: Flow network structure
#' @param source: Source vertex (1-indexed)
#' @param sink: Sink vertex (1-indexed)
#' @return: List with max_flow value, flow matrix, and flow decomposition
dinic_max_flow <- function(network, source, sink) {
  if (source < 1 || source > network$n || sink < 1 || sink > network$n) {
    stop("Error: Source and sink must be between 1 and n")
  }
  
  if (source == sink) {
    stop("Error: Source and sink must be different")
  }
  
  max_flow <- 0
  iterations <- 0
  
  # Repeat until no augmenting path exists
  repeat {
    iterations <- iterations + 1
    
    # Build level graph using BFS
    level <- bfs_level_graph(network, source, sink)
    
    # If sink is not reachable, no more augmenting paths
    if (level[sink] == -1) {
      break
    }
    
    # Send flow using DFS until blocking flow is achieved
    start <- rep(1, network$n)  # Track next edge to try from each vertex
    
    repeat {
      flow <- dfs_send_flow(network, source, sink, level, Inf, start)
      
      if (flow == 0) {
        break
      }
      
      max_flow <- max_flow + flow
    }
  }
  
  # Find minimum cut
  level_final <- bfs_level_graph(network, source, sink)
  source_side <- which(level_final != -1)
  sink_side <- which(level_final == -1)
  
  # Find edges in the minimum cut
  min_cut_edges <- list()
  for (u in source_side) {
    for (v in network$graph[[u]]) {
      if (v %in% sink_side && network$capacity[u, v] > 0) {
        min_cut_edges <- c(min_cut_edges, list(list(from = u, to = v, capacity = network$capacity[u, v])))
      }
    }
  }
  
  return(list(
    max_flow = max_flow,
    flow_matrix = network$flow,
    iterations = iterations,
    min_cut_edges = min_cut_edges,
    source_partition = source_side,
    sink_partition = sink_side
  ))
}

#' Print maximum flow results
#' @param result: Result from dinic_max_flow
#' @param network: Original network (optional, for displaying edges)
print_max_flow <- function(result, network = NULL) {
  cat("Maximum Flow Result:\n")
  cat(strrep("=", 60), "\n\n")
  cat(sprintf("Maximum Flow: %g\n", result$max_flow))
  cat(sprintf("Iterations: %d\n\n", result$iterations))
  
  cat("Minimum Cut Edges:\n")
  cat(strrep("-", 60), "\n")
  if (length(result$min_cut_edges) > 0) {
    cat(sprintf("%-15s %-15s %-15s\n", "From", "To", "Capacity"))
    cat(strrep("-", 60), "\n")
    total_cut_capacity <- 0
    for (edge in result$min_cut_edges) {
      cat(sprintf("%-15d %-15d %-15g\n", edge$from, edge$to, edge$capacity))
      total_cut_capacity <- total_cut_capacity + edge$capacity
    }
    cat(strrep("-", 60), "\n")
    cat(sprintf("Total Cut Capacity: %g\n", total_cut_capacity))
  } else {
    cat("No cut edges found\n")
  }
  
  cat(sprintf("\nSource partition: {%s}\n", paste(result$source_partition, collapse = ", ")))
  cat(sprintf("Sink partition: {%s}\n", paste(result$sink_partition, collapse = ", ")))
  cat("\n")
}

#' Print flow on edges
#' @param network: Flow network with computed flows
print_flow_edges <- function(network) {
  cat("Flow on Edges:\n")
  cat(strrep("-", 60), "\n")
  cat(sprintf("%-10s %-10s %-12s %-12s\n", "From", "To", "Flow", "Capacity"))
  cat(strrep("-", 60), "\n")
  
  for (u in 1:network$n) {
    for (v in network$graph[[u]]) {
      if (network$capacity[u, v] > 0 && network$flow[u, v] > 0) {
        cat(sprintf("%-10d %-10d %-12g %-12g\n", 
                    u, v, network$flow[u, v], network$capacity[u, v]))
      }
    }
  }
  cat(strrep("-", 60), "\n\n")
}

# ========== Example 1: Basic 6-Vertex Network ==========

cat("========== Example 1: Basic 6-Vertex Flow Network ==========\n\n")

# Create network with 6 vertices
network1 <- create_flow_network(6)

# Add edges (from, to, capacity)
network1 <- add_edge(network1, 1, 2, 16)
network1 <- add_edge(network1, 1, 3, 13)
network1 <- add_edge(network1, 2, 3, 10)
network1 <- add_edge(network1, 2, 4, 12)
network1 <- add_edge(network1, 3, 2, 4)
network1 <- add_edge(network1, 3, 5, 14)
network1 <- add_edge(network1, 4, 3, 9)
network1 <- add_edge(network1, 4, 6, 20)
network1 <- add_edge(network1, 5, 4, 7)
network1 <- add_edge(network1, 5, 6, 4)

cat("Network edges:\n")
cat("1 -> 2 (16), 1 -> 3 (13)\n")
cat("2 -> 3 (10), 2 -> 4 (12)\n")
cat("3 -> 2 (4), 3 -> 5 (14)\n")
cat("4 -> 3 (9), 4 -> 6 (20)\n")
cat("5 -> 4 (7), 5 -> 6 (4)\n\n")

# Find maximum flow from vertex 1 to vertex 6
result1 <- dinic_max_flow(network1, source = 1, sink = 6)
print_max_flow(result1, network1)
print_flow_edges(network1)

# ========== Example 2: Simple Network ==========

cat("========== Example 2: Simple 4-Vertex Network ==========\n\n")

network2 <- create_flow_network(4)
network2 <- add_edge(network2, 1, 2, 10)
network2 <- add_edge(network2, 1, 3, 10)
network2 <- add_edge(network2, 2, 3, 2)
network2 <- add_edge(network2, 2, 4, 4)
network2 <- add_edge(network2, 3, 4, 9)

cat("Network edges:\n")
cat("1 -> 2 (10), 1 -> 3 (10)\n")
cat("2 -> 3 (2), 2 -> 4 (4)\n")
cat("3 -> 4 (9)\n\n")

result2 <- dinic_max_flow(network2, source = 1, sink = 4)
print_max_flow(result2, network2)
print_flow_edges(network2)

# ========== Example 3: Bipartite Matching ==========

cat("========== Example 3: Bipartite Matching (5 workers, 4 jobs) ==========\n\n")

# Create bipartite graph: source(1) -> workers(2-6) -> jobs(7-10) -> sink(11)
network3 <- create_flow_network(11)

# Source to workers (capacity 1 each)
for (i in 2:6) {
  network3 <- add_edge(network3, 1, i, 1)
}

# Workers to jobs (based on which jobs they can do)
network3 <- add_edge(network3, 2, 7, 1)   # Worker 1 can do Job 1
network3 <- add_edge(network3, 2, 8, 1)   # Worker 1 can do Job 2
network3 <- add_edge(network3, 3, 7, 1)   # Worker 2 can do Job 1
network3 <- add_edge(network3, 3, 9, 1)   # Worker 2 can do Job 3
network3 <- add_edge(network3, 4, 8, 1)   # Worker 3 can do Job 2
network3 <- add_edge(network3, 4, 10, 1)  # Worker 3 can do Job 4
network3 <- add_edge(network3, 5, 9, 1)   # Worker 4 can do Job 3
network3 <- add_edge(network3, 6, 10, 1)  # Worker 5 can do Job 4

# Jobs to sink (capacity 1 each)
for (i in 7:10) {
  network3 <- add_edge(network3, i, 11, 1)
}

cat("Bipartite matching problem:\n")
cat("Workers: 1-5, Jobs: 1-4\n")
cat("Maximum number of worker-job assignments:\n\n")

result3 <- dinic_max_flow(network3, source = 1, sink = 11)
cat(sprintf("Maximum Matching: %d\n\n", result3$max_flow))

# Show assignments
cat("Worker-Job Assignments:\n")
for (worker in 2:6) {
  for (job in 7:10) {
    if (network3$flow[worker, job] > 0) {
      cat(sprintf("Worker %d -> Job %d\n", worker - 1, job - 6))
    }
  }
}
cat("\n")

# ========== Example 4: Multi-source Multi-sink ==========

cat("========== Example 4: Multi-Source Multi-Sink Problem ==========\n\n")

# Convert to single source/sink by adding super source and super sink
network4 <- create_flow_network(8)

# Super source (1) to sources (2, 3)
network4 <- add_edge(network4, 1, 2, 10)
network4 <- add_edge(network4, 1, 3, 15)

# Internal edges
network4 <- add_edge(network4, 2, 4, 8)
network4 <- add_edge(network4, 2, 5, 6)
network4 <- add_edge(network4, 3, 4, 5)
network4 <- add_edge(network4, 3, 5, 12)
network4 <- add_edge(network4, 4, 6, 10)
network4 <- add_edge(network4, 5, 7, 9)

# Sinks (6, 7) to super sink (8)
network4 <- add_edge(network4, 6, 8, 15)
network4 <- add_edge(network4, 7, 8, 12)

cat("Multi-source multi-sink converted to single source/sink\n")
cat("Sources: 2, 3 | Sinks: 6, 7\n\n")

result4 <- dinic_max_flow(network4, source = 1, sink = 8)
print_max_flow(result4, network4)

# ========== Algorithm Properties ==========

cat("========== Algorithm Properties ==========\n\n")
cat("1. Dinic's algorithm guarantees finding maximum flow\n")
cat("2. Works on directed graphs with non-negative capacities\n")
cat("3. Faster than Ford-Fulkerson for dense graphs\n")
cat("4. Maximum flow equals minimum cut capacity (Max-Flow Min-Cut Theorem)\n")
cat("5. Each iteration increases distance to sink for at least one vertex\n")
cat("6. Number of iterations is O(V) for unit capacity networks\n\n")

cat("========== Comparison with Other Max Flow Algorithms ==========\n\n")
cat("Ford-Fulkerson:\n")
cat("  Time: O(E * max_flow) - can be slow for large flows\n")
cat("  Method: Find any augmenting path\n\n")
cat("Edmonds-Karp:\n")
cat("  Time: O(V * E^2) - specific Ford-Fulkerson with BFS\n")
cat("  Method: Shortest augmenting path\n\n")
cat("Dinic's Algorithm:\n")
cat("  Time: O(V^2 * E), O(E * sqrt(V)) for unit capacities\n")
cat("  Method: Blocking flows in level graphs\n\n")
cat("Push-Relabel:\n")
cat("  Time: O(V^2 * E) or O(V^3) depending on implementation\n")
cat("  Method: Preflow-push operations\n")
