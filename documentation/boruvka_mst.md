# Boruvka's Minimum Spanning Tree (MST)

This document describes the Boruvka MST implementation located at `R/graph_algorithms/boruvka_mst.r`.

## Description

The implementation builds a Minimum Spanning Tree for an undirected weighted graph using Boruvka's method. The graph is represented by a list with `V` (number of vertices) and `edges` (a data.frame with columns `u`, `v`, `w`). Vertex indices are 1-based to match other algorithms in the repository.

## Usage

In an R session:

source('graph_algorithms/boruvka_mst.r')

From command line using Rscript:

Rscript -e "source('R/graph_algorithms/boruvka_mst.r')"

## Complexity

- Time complexity: Depends on implementation details; this simple version iterates until components merge.
- Space complexity: O(V + E)

## Notes

- This implementation prioritizes clarity and repository consistency. For large graphs, more optimized data structures and path compression in union-find should be used.
