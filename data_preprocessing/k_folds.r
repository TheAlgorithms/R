# Create randomized k-fold cross-validation assignments.
#
# y: vector whose observations should be divided into folds
# k: number of folds
# isList: if TRUE, return observation indices grouped by fold;
#         if FALSE, return one fold ID per observation
# seed: random seed for reproducibility

get_k_folds <- function(
    y = c(),
    k = 10,
    isList = TRUE,
    seed = 123
) {
  n <- length(y)
  
  if (n == 0) {
    stop("y must contain at least one observation")
  }
  
  if (length(k) != 1 ||
      !is.numeric(k) ||
      is.na(k) ||
      k != as.integer(k)) {
    stop("k must be a positive integer")
  }
  
  if (k < 2 || k > n) {
    stop("k must be between 2 and length(y)")
  }
  
  if (length(isList) != 1 || !is.logical(isList) || is.na(isList)) {
    stop("isList must be TRUE or FALSE")
  }
  
  set.seed(seed)
  
  # Randomly assign every observation to a fold.
  # Fold sizes differ by at most one observation.
  fold_ids <- sample(rep(seq_len(k), length.out = n))
  
  if (isList) {
    return(lapply(seq_len(k), function(fold) {
      which(fold_ids == fold)
    }))
  }
  
  fold_ids
}