# Push-Relabel (Preflow-Push) Maximum Flow

This document describes the Push-Relabel implementation located at `R/graph_algorithms/push_relabel.r`.

## Description

The `push_relabel` function implements the Push-Relabel algorithm (also known as Preflow-Push) for computing maximum flow in a directed graph represented by a capacity matrix. It uses the highest-label selection rule by moving active vertices to the front of the list when relabeled.

## Usage

In an R session:

source('graph_algorithms/push_relabel.r')

From command line using Rscript:

Rscript -e "source('R/graph_algorithms/push_relabel.r')"

## Complexity

- Time complexity: O(V^3) for the generic implementation; improvements (gap relabeling, global relabel) reduce this significantly for practical graphs.
- Space complexity: O(V^2) for flow and capacity matrices.

## Notes

- The implementation uses 1-based indexing to be consistent with other algorithms in the repository.
- For large graphs consider adding optimizations such as gap relabeling or global relabeling.
