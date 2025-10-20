find_winner <- function(n, k) {

  #' @description Finds the winner in the Josephus problem
  #' @param n The number of people in the initial circle
  #' @param k The count of each step
  #' @usage find_winner(n, k)
  #' @details In computer science and mathematics, the Josephus problem
  #' (or Josephus permutation) is a theoretical problem related to a certain
  #' counting-out game. Such games are used to pick out a person from a group.
  #' @references https://en.wikipedia.org/wiki/Josephus_problem

  if (k > n) stop("Size of the group must be greater than step")

  winner <- 0

  for (i in 1:n) {
     winner <- (winner + k) %% i
  }

  return(winner + 1)
}

result <- find_winner(11, 2)
print(result)  # expected 7

result <- find_winner(5, 2)
print(result)  # expected 3