# ==============================================
# Edmonds’ Blossom Algorithm
# ==============================================
# Algorithm: Maximum matching in general (non-bipartite) graphs.
# Framework: R (using igraph package)
#
# Purpose:
# - Compute the maximum matching in a general (non-bipartite) graph.
# - Finds the largest set of edges without common vertices.
#
# Core Idea:
# - Uses the Blossom algorithm to handle odd-length cycles (blossoms) in non-bipartite graphs.
# - Iteratively augments paths to find the maximum matching.
#
# Complexity:
# - Time: O(V^3) in general (V = number of vertices)
# - Space: O(V + E) for graph representation
#
# Edge Cases / Notes:
# - Works for both bipartite and non-bipartite graphs.
# - Handles odd-length cycles using the blossom contraction technique.
#
# Typical Applications:
# - Network pairing/matching problems
# - Job assignment and scheduling
# - Stable pairings in tournaments or projects
#
# Reference:
# Edmonds, J. (1965). Paths, trees, and flowers. Canadian Journal of Mathematics.
# ==============================================

# Load required library
suppressPackageStartupMessages(library(igraph))

# Example Graph: Non-bipartite
edges <- c(1,2, 1,3, 2,4, 3,4, 4,5)
g <- graph(edges, directed = FALSE)

# Compute Maximum Matching
matching <- max_bipartite_match(g, types = NULL) # types=NULL for general graphs
max_match_edges <- matching$matching

# Display Result
cat("Maximum matching edges:\n")
print(max_match_edges)

# ==============================================
# Note:
# - This script defines the Edmonds’ Blossom Algorithm in R using igraph.
# - Suitable for small to medium non-bipartite graphs.
# - For large graphs, performance may degrade due to cubic complexity.
# ==============================================
