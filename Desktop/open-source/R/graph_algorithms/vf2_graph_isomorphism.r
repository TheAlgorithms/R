# VF2 Graph Isomorphism Algorithm
#
# Fully checks node and edge compatibility between two graphs.
# Returns TRUE if graphs are isomorphic, FALSE otherwise.
# If isomorphic, also returns one valid node mapping.

vf2_isomorphic <- function(G1, G2) {
  
  n1 <- max(as.numeric(names(G1)))
  n2 <- max(as.numeric(names(G2)))
  
  if (n1 != n2) return(list(isomorphic = FALSE, mapping = NULL))
  
  n <- n1
  mapping <- rep(NA, n)
  used <- rep(FALSE, n)
  
  # Check if current partial mapping is feasible
  is_feasible <- function(u, v) {
    # Check neighbors already mapped
    for (i in 1:n) {
      if (!is.na(mapping[i])) {
        if ((i %in% G1[[as.character(u)]]) != (mapping[i] %in% G2[[as.character(v)]])) {
          return(FALSE)
        }
      }
    }
    return(TRUE)
  }
  
  # Recursive backtracking
  match <- function(depth) {
    if (depth > n) return(TRUE)
    
    for (v in 1:n) {
      if (!used[v]) {
        if (is_feasible(depth, v)) {
          mapping[depth] <<- v
          used[v] <<- TRUE
          if (match(depth + 1)) return(TRUE)
          mapping[depth] <<- NA
          used[v] <<- FALSE
        }
      }
    }
    return(FALSE)
  }
  
  result <- match(1)
  return(list(isomorphic = result, mapping = if (result) mapping else NULL))
}

# Example usage
cat("=== VF2 Graph Isomorphism Algorithm ===\n")

G1 <- list(
  "1" = c(2, 3),
  "2" = c(1, 3),
  "3" = c(1, 2)
)

G2 <- list(
  "1" = c(2, 3),
  "2" = c(1, 3),
  "3" = c(1, 2)
)

res <- vf2_isomorphic(G1, G2)
cat("Graphs are isomorphic:", res$isomorphic, "\n")
if (res$isomorphic) cat("Mapping:", paste(res$mapping, collapse = " -> "), "\n")
