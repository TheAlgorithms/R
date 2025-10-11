# Bellman-Ford Shortest Path

The Bellman-Ford algorithm finds shortest paths from a single source vertex to all
other vertices in a weighted graph. It supports negative edge weights and detects
negative-weight cycles.

## Characteristics
- Time complexity: O(V * E)
- Space complexity: O(V)
- Handles negative weights
- Detects negative-weight cycles

## Usage
The repository implementation expects a graph as an adjacency list. Each vertex key
is a string of its numeric id, and its value is a list of edges. Each edge is a list
with fields `vertex` (integer) and `weight` (numeric).

Example graph entry:

```
"1" = list(list(vertex = 2, weight = 6), list(vertex = 3, weight = 5))
```

Call the algorithm with:

```
result <- bellman_ford_shortest_path(graph, source)

# Check for negative cycle
if (result$negative_cycle) {
  # Handle accordingly
}

# Reconstruct path
path_info <- get_bellman_ford_path(result, source, target)
```

## Notes
- The implementation is educational and prioritizes clarity. For production use,
  consider more efficient data structures and input validation.
