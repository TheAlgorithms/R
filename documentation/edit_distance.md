# Edit Distance

Levenshtein edit distance calculates the minimum number of single-character insertions, deletions, and substitutions required to transform one string into another.

``` r
source("dynamic_programming/edit_distance.r")

# Compute the edit distance
distance <- edit_distance("kitten", "sitting")
print(distance)

# Reconstruct the optimal sequence of operations
result <- edit_distance_with_path("kitten", "sitting")
print(result$distance)
print(result$operations)
```
