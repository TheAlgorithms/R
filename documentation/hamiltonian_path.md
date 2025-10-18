# Hamiltonian Path (Backtracking)

This document describes the Hamiltonian Path backtracking implementation in `R/graph_algorithms/hamiltonian_path.r`.

## Description

The `hamiltonianPath` function searches for a Hamiltonian Path in an undirected graph represented by an adjacency matrix. It uses backtracking to attempt to build a path that visits every vertex exactly once.

## Usage

In an R session:

source('graph_algorithms/hamiltonian_path.r')

From command line using Rscript:

Rscript -e "source('R/graph_algorithms/hamiltonian_path.r')"

## Complexity

- Time complexity: O(n!) in the worst case (backtracking over permutations).
- Space complexity: O(n) for path storage and recursion.

## Notes

- The implementation assumes an undirected graph given as an adjacency matrix with 0/1 entries.
- For production use on larger graphs, consider heuristics or approximation algorithms; the problem is NP-complete.
