# Kosaraju's Algorithm for Finding Strongly Connected Components
#
# Kosaraju's algorithm is used to find all strongly connected components (SCCs) in a directed graph.
# A strongly connected component is a maximal set of vertices such that there is a path from 
# each vertex to every other vertex in the component.
#
# Algorithm Steps:
# 1. Perform DFS on the original graph and store vertices in finishing order (stack)
# 2. Create transpose graph (reverse all edge directions)
# 3. Perform DFS on transpose graph in the order of decreasing finish times
#
# Time Complexity: O(V + E) where V is vertices and E is edges
# Space Complexity: O(V) for visited arrays and recursion stack
#
# Author: Contributor for TheAlgorithms/R
# Applications: Social network analysis, web crawling, circuit design verification

# Helper function: DFS to fill stack with finishing times
dfs_fill_order <- function(graph, vertex, visited, stack) {
  # Mark current vertex as visited
  visited[vertex] <- TRUE
  
  # Visit all adjacent vertices
  if (as.character(vertex) %in% names(graph)) {
    for (neighbor in graph[[as.character(vertex)]]) {
      if (!visited[neighbor]) {
        result <- dfs_fill_order(graph, neighbor, visited, stack)
        stack <- result$stack
        visited <- result$visited
      }
    }
  }
  
  # Push current vertex to stack (finishing time)
  stack <- c(stack, vertex)
  
  return(list(visited = visited, stack = stack))
}

# Helper function: DFS to collect vertices in current SCC
dfs_collect_scc <- function(transpose_graph, vertex, visited, current_scc) {
  # Mark current vertex as visited
  visited[vertex] <- TRUE
  current_scc <- c(current_scc, vertex)
  
  # Visit all adjacent vertices in transpose graph
  if (as.character(vertex) %in% names(transpose_graph)) {
    for (neighbor in transpose_graph[[as.character(vertex)]]) {
      if (!visited[neighbor]) {
        result <- dfs_collect_scc(transpose_graph, neighbor, visited, current_scc)
        visited <- result$visited
        current_scc <- result$current_scc
      }
    }
  }
  
  return(list(visited = visited, current_scc = current_scc))
}

# Function to create transpose graph (reverse all edges)
create_transpose_graph <- function(graph) {
  # Initialize empty transpose graph
  transpose_graph <- list()
  
  # Get all vertices
  all_vertices <- unique(c(names(graph), unlist(graph)))
  
  # Initialize empty adjacency lists for all vertices
  for (vertex in all_vertices) {
    transpose_graph[[as.character(vertex)]] <- c()
  }
  
  # Reverse all edges
  for (vertex in names(graph)) {
    for (neighbor in graph[[vertex]]) {
      # Add edge from neighbor to vertex (reverse direction)
      transpose_graph[[as.character(neighbor)]] <- 
        c(transpose_graph[[as.character(neighbor)]], as.numeric(vertex))
    }
  }
  
  # Remove empty adjacency lists
  transpose_graph <- transpose_graph[lengths(transpose_graph) > 0 | names(transpose_graph) %in% names(graph)]
  
  return(transpose_graph)
}

# Main Kosaraju's Algorithm function
kosaraju_scc <- function(graph) {
  #' Kosaraju's Algorithm for Strongly Connected Components
  #' 
  #' @param graph A named list representing adjacency list of directed graph
  #'              Format: list("1" = c(2, 3), "2" = c(3), "3" = c())
  #'              Keys are vertex names (as strings), values are vectors of adjacent vertices
  #' 
  #' @return A list containing:
  #'   - scc_list: List of strongly connected components (each is a vector of vertices)
  #'   - scc_count: Number of strongly connected components
  #'   - vertex_to_scc: Named vector mapping each vertex to its SCC number
  #'   - transpose_graph: The transpose graph used in algorithm
  
  # Input validation
  if (!is.list(graph)) {
    stop("Graph must be a list representing adjacency list")
  }
  
  if (length(graph) == 0) {
    return(list(scc_list = list(), scc_count = 0, vertex_to_scc = c(), transpose_graph = list()))
  }
  
  # Get all vertices in the graph
  all_vertices <- unique(c(names(graph), unlist(graph)))
  max_vertex <- max(all_vertices)
  
  # Initialize visited array for first DFS
  visited <- rep(FALSE, max_vertex)
  names(visited) <- 1:max_vertex
  stack <- c()
  
  # Step 1: Fill vertices in stack according to their finishing times
  cat("Step 1: Performing DFS to determine finishing order...\n")
  for (vertex in all_vertices) {
    if (!visited[vertex]) {
      result <- dfs_fill_order(graph, vertex, visited, stack)
      visited <- result$visited
      stack <- result$stack
    }
  }
  
  cat("Finishing order (stack):", rev(stack), "\n")
  
  # Step 2: Create transpose graph
  cat("Step 2: Creating transpose graph...\n")
  transpose_graph <- create_transpose_graph(graph)
  
  # Step 3: Perform DFS on transpose graph in order of decreasing finish times
  cat("Step 3: Finding SCCs in transpose graph...\n")
  visited <- rep(FALSE, max_vertex)
  names(visited) <- 1:max_vertex
  
  scc_list <- list()
  scc_count <- 0
  vertex_to_scc <- rep(NA, max_vertex)
  names(vertex_to_scc) <- 1:max_vertex
  
  # Process vertices in reverse finishing order
  for (vertex in rev(stack)) {
    if (!visited[vertex]) {
      scc_count <- scc_count + 1
      result <- dfs_collect_scc(transpose_graph, vertex, visited, c())
      visited <- result$visited
      current_scc <- sort(result$current_scc)
      
      scc_list[[scc_count]] <- current_scc
      
      # Map vertices to their SCC number
      for (v in current_scc) {
        vertex_to_scc[v] <- scc_count
      }
      
      cat("SCC", scc_count, ":", current_scc, "\n")
    }
  }
  
  # Filter vertex_to_scc to only include vertices that exist in graph
  vertex_to_scc <- vertex_to_scc[all_vertices]
  
  return(list(
    scc_list = scc_list,
    scc_count = scc_count,
    vertex_to_scc = vertex_to_scc,
    transpose_graph = transpose_graph
  ))
}

# Print function for SCC results
print_scc_results <- function(result) {
  cat("\n=== KOSARAJU'S ALGORITHM RESULTS ===\n")
  cat("Number of Strongly Connected Components:", result$scc_count, "\n\n")
  
  for (i in 1:result$scc_count) {
    cat("SCC", i, ":", result$scc_list[[i]], "\n")
  }
  
  cat("\nVertex to SCC mapping:\n")
  for (vertex in names(result$vertex_to_scc)) {
    if (!is.na(result$vertex_to_scc[vertex])) {
      cat("Vertex", vertex, "-> SCC", result$vertex_to_scc[vertex], "\n")
    }
  }
}

# Function to visualize graph structure (text-based)
print_graph <- function(graph, title = "Graph") {
  cat("\n=== ", title, " ===\n")
  if (length(graph) == 0) {
    cat("Empty graph\n")
    return()
  }
  
  for (vertex in names(graph)) {
    if (length(graph[[vertex]]) > 0) {
      cat("Vertex", vertex, "->", graph[[vertex]], "\n")
    } else {
      cat("Vertex", vertex, "-> (no outgoing edges)\n")
    }
  }
  
  # Also show vertices with no outgoing edges
  all_vertices <- unique(c(names(graph), unlist(graph)))
  vertices_with_no_outgoing <- setdiff(all_vertices, names(graph))
  for (vertex in vertices_with_no_outgoing) {
    cat("Vertex", vertex, "-> (no outgoing edges)\n")
  }
}

# ==============================================================================
# EXAMPLES AND TEST CASES
# ==============================================================================

run_kosaraju_examples <- function() {
  cat("=================================================================\n")
  cat("KOSARAJU'S ALGORITHM - STRONGLY CONNECTED COMPONENTS EXAMPLES\n")
  cat("=================================================================\n\n")
  
  # Example 1: Simple graph with 2 SCCs
  cat("EXAMPLE 1: Simple Directed Graph with 2 SCCs\n")
  cat("-----------------------------------------------------------------\n")
  
  # Graph: 1 -> 2 -> 3 -> 1 (SCC: {1,2,3}) and 4 -> 5, 5 -> 4 (SCC: {4,5})
  #        Also: 2 -> 4 (bridge between SCCs)
  graph1 <- list(
    "1" = c(2),
    "2" = c(3, 4),
    "3" = c(1),
    "4" = c(5),
    "5" = c(4)
  )
  
  print_graph(graph1, "Example 1 - Original Graph")
  result1 <- kosaraju_scc(graph1)
  print_scc_results(result1)
  
  cat("\n=================================================================\n")
  cat("EXAMPLE 2: Linear Chain (No Cycles)\n")
  cat("-----------------------------------------------------------------\n")
  
  # Graph: 1 -> 2 -> 3 -> 4 (Each vertex is its own SCC)
  graph2 <- list(
    "1" = c(2),
    "2" = c(3),
    "3" = c(4),
    "4" = c()
  )
  
  print_graph(graph2, "Example 2 - Linear Chain")
  result2 <- kosaraju_scc(graph2)
  print_scc_results(result2)
  
  cat("\n=================================================================\n")
  cat("EXAMPLE 3: Complex Graph with Multiple SCCs\n")
  cat("-----------------------------------------------------------------\n")
  
  # More complex graph with 3 SCCs
  # SCC 1: {1, 2, 3}  SCC 2: {4, 5, 6}  SCC 3: {7}
  graph3 <- list(
    "1" = c(2),
    "2" = c(3, 4),
    "3" = c(1),
    "4" = c(5),
    "5" = c(6),
    "6" = c(4, 7),
    "7" = c()
  )
  
  print_graph(graph3, "Example 3 - Complex Graph")
  result3 <- kosaraju_scc(graph3)
  print_scc_results(result3)
  
  cat("\n=================================================================\n")
  cat("EXAMPLE 4: Single Strongly Connected Component\n")
  cat("-----------------------------------------------------------------\n")
  
  # Complete cycle: 1 -> 2 -> 3 -> 4 -> 1
  graph4 <- list(
    "1" = c(2),
    "2" = c(3),
    "3" = c(4),
    "4" = c(1)
  )
  
  print_graph(graph4, "Example 4 - Single SCC")
  result4 <- kosaraju_scc(graph4)
  print_scc_results(result4)
  
  cat("\n=================================================================\n")
  cat("EXAMPLE 5: Disconnected Graph\n")
  cat("-----------------------------------------------------------------\n")
  
  # Two separate components: {1 -> 2 -> 1} and {3 -> 4 -> 3}
  graph5 <- list(
    "1" = c(2),
    "2" = c(1),
    "3" = c(4),
    "4" = c(3)
  )
  
  print_graph(graph5, "Example 5 - Disconnected Graph")
  result5 <- kosaraju_scc(graph5)
  print_scc_results(result5)
  
  cat("\n=================================================================\n")
  cat("PRACTICAL APPLICATION: Social Network Analysis\n")
  cat("-----------------------------------------------------------------\n")
  
  cat("In social networks, SCCs represent groups of people who can\n")
  cat("all reach each other through mutual connections. This is useful for:\n")
  cat("- Community detection\n")
  cat("- Information spread analysis\n") 
  cat("- Influence maximization\n")
  cat("- Network segmentation\n\n")
  
  # Example social network (simplified)
  social_network <- list(
    "Alice" = c("Bob"),
    "Bob" = c("Charlie", "David"),
    "Charlie" = c("Alice"),  # Forms cycle Alice->Bob->Charlie->Alice
    "David" = c("Eve"),
    "Eve" = c("David"),      # Forms cycle David->Eve->David
    "Frank" = c()            # Isolated node
  )
  
  cat("Social Network Example:\n")
  print_graph(social_network, "Social Network Graph")
  
  # Note: This will work but vertex names will be converted to numbers
  cat("Note: Algorithm works with numeric vertices. For named vertices,\n")
  cat("you would need to create a mapping between names and numbers.\n\n")
  
  cat("=================================================================\n")
  cat("END OF EXAMPLES\n")
  cat("=================================================================\n")
}

# Utility function to convert named graph to numeric
convert_named_to_numeric_graph <- function(named_graph) {
  # Get unique vertex names
  all_names <- unique(c(names(named_graph), unlist(named_graph)))
  
  # Create name to number mapping
  name_to_num <- setNames(seq_along(all_names), all_names)
  num_to_name <- setNames(all_names, seq_along(all_names))
  
  # Convert graph
  numeric_graph <- list()
  for (vertex_name in names(named_graph)) {
    vertex_num <- name_to_num[vertex_name]
    neighbors <- named_graph[[vertex_name]]
    numeric_neighbors <- name_to_num[neighbors]
    numeric_graph[[as.character(vertex_num)]] <- numeric_neighbors
  }
  
  return(list(
    graph = numeric_graph,
    name_to_num = name_to_num,
    num_to_name = num_to_name
  ))
}

# Examples are available but not run automatically to avoid side effects
# To run examples, execute: run_kosaraju_examples()
if (interactive()) {
  cat("Loading Kosaraju's Strongly Connected Components Algorithm...\n")
  cat("Run 'run_kosaraju_examples()' to see examples and test cases.\n")
}

# Uncomment the following line to run examples automatically:
# run_kosaraju_examples()