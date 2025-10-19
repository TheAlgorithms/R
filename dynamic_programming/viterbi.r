# ==============================================================
# Viterbi Algorithm — Hidden Markov Model (HMM) Decoding
# ==============================================================
#
# Description:
#   The Viterbi algorithm finds the most probable sequence of
#   hidden states (state path) that results in a given sequence of
#   observed events in a Hidden Markov Model.
#
# Time Complexity: O(N * T)
#   - N = number of hidden states
#   - T = length of observation sequence
#
# Space Complexity: O(N * T)
#
# Input:
#   states        - vector of hidden states
#   observations  - vector of observed symbols
#   start_prob    - named vector of initial probabilities (state → prob)
#   trans_prob    - matrix of transition probabilities (from_state → to_state)
#   emit_prob     - matrix of emission probabilities (state → observation)
#
# Output:
#   A list containing:
#       best_path     - most probable state sequence
#       best_prob      - probability of the best path
#
# Example usage provided at bottom of file.
# ==============================================================

viterbi <- function(states, observations, start_prob, trans_prob, emit_prob) {
  N <- length(states)
  T_len <- length(observations)
  
  # Initialize matrices
  V <- matrix(0, nrow = N, ncol = T_len)  # probability table
  path <- matrix(NA, nrow = N, ncol = T_len)  # backpointer table
  
  # Initialization step
  for (i in 1:N) {
    V[i, 1] <- start_prob[states[i]] * emit_prob[states[i], observations[1]]
    path[i, 1] <- 0
  }
  
  # Recursion step
  for (t in 2:T_len) {
    for (j in 1:N) {
      probs <- V[, t - 1] * trans_prob[, states[j]] * emit_prob[states[j], observations[t]]
      V[j, t] <- max(probs)
      path[j, t] <- which.max(probs)
    }
  }
  
  # Termination step
  best_last_state <- which.max(V[, T_len])
  best_prob <- V[best_last_state, T_len]
  
  # Backtrack the best path
  best_path <- rep(NA, T_len)
  best_path[T_len] <- best_last_state
  
  if (T_len > 1) {
    for (t in rev(seq_len(T_len - 1))) {
      best_path[t] <- path[best_path[t + 1], t + 1]
    }
      best_path[t] <- path[best_path[t + 1], t + 1]
    }
  }
  
  best_state_sequence <- states[best_path]
  
  return(list(
    best_path = best_state_sequence,
    best_prob = best_prob
  ))
}

# ==============================================================
# Example Usage and Test
if (interactive()) {
  cat("=== Viterbi Algorithm — Hidden Markov Model ===\n")

  # Example: Weather HMM
  # States: Rainy, Sunny
  # Observations: walk, shop, clean
  states <- c("Rainy", "Sunny")
  observations <- c("walk", "shop", "clean")

  # Start probabilities
  start_prob <- c(Rainy = 0.6, Sunny = 0.4)

  # Transition probabilities
  trans_prob <- matrix(c(
    0.7, 0.3,   # from Rainy to (Rainy, Sunny)
    0.4, 0.6    # from Sunny to (Rainy, Sunny)
  ), nrow = 2, byrow = TRUE)
  rownames(trans_prob) <- states
  colnames(trans_prob) <- states

  # Emission probabilities
  emit_prob <- matrix(c(
    0.1, 0.4, 0.5,  # Rainy emits (walk, shop, clean)
    0.6, 0.3, 0.1   # Sunny emits (walk, shop, clean)
  ), nrow = 2, byrow = TRUE)
  rownames(emit_prob) <- states
  colnames(emit_prob) <- observations

  # Observed sequence
  obs_seq <- c("walk", "shop", "clean")

  cat("Observation sequence:", paste(obs_seq, collapse = ", "), "\n")
  result <- viterbi(states, obs_seq, start_prob, trans_prob, emit_prob)

  cat("Most probable state sequence:\n")
  cat(paste(result$best_path, collapse = " -> "), "\n")
  cat("Probability of this sequence:", result$best_prob, "\n")
}
cat(paste(result$best_path, collapse = " -> "), "\n")
cat("Probability of this sequence:", result$best_prob, "\n")
