#' Computes modular exponentiation using fast binary exponentiation.
#'
#' @param base Numeric or integer base.
#' @param exp Non-negative integer exponent.
#' @param mod Optional positive integer modulus. If `NULL`, computes
#'   \eqn{base^{exp}} without modulus (may overflow for large values).
#'
#' @return If `mod` is provided, returns an integer in \[0, mod - 1\] equal to
#'   \eqn{(base^{exp}) \bmod mod}. If `mod` is `NULL`, returns \eqn{base^{exp}}.
#'
#' @details
#' Implements **binary (fast) exponentiation** running in \eqn{O(\log exp)} time
#' and \eqn{O(1)} extra space.
#' - When `mod` is provided, intermediate values are reduced modulo `mod` to
#'   avoid overflow and keep numbers bounded.
#' - Negative bases are handled correctly in modular mode by normalizing
#'   \code{base <- (base \%\% mod + mod) \%\% mod}.
#' - Negative exponents are **not supported** (would require modular inverse).
#'
#' @examples
#' modular_exponentiation(2, 10, 1000)   # 24
#' modular_exponentiation(3, 0, 7)       # 1
#' modular_exponentiation(5, 3)          # 125 (no modulus)
#' modular_exponentiation(-2, 5, 13)     # 6  because (-2)^5 = -32 ≡ 6 (mod 13)
#'
#' @seealso \code{\link[base]{%%}} for modulus operator.
#'
#' @export
modular_exponentiation <- function(base, exp, mod = NULL) {
  # validate exponent
  if (length(exp) != 1 || is.na(exp) || exp < 0 || exp != as.integer(exp)) {
    stop("`exp` must be a single non-negative integer.")
  }
  exp <- as.integer(exp)

  # no modulus: compute power with fast exponentiation (may overflow for large numbers)
  if (is.null(mod)) {
    result <- 1
    b <- base
    e <- exp
    while (e > 0) {
      if (e %% 2L == 1L) result <- result * b
      b <- b * b
      e <- e %/% 2L
    }
    return(result)
  }

  # validate modulus
  if (length(mod) != 1 || is.na(mod) || mod <= 0 || mod != as.integer(mod)) {
    stop("`mod` must be a single positive integer when provided.")
  }
  mod <- as.integer(mod)

  # normalize base into [0, mod-1]
  b <- ((base %% mod) + mod) %% mod
  result <- 1L
  e <- exp

  while (e > 0L) {
    if (e %% 2L == 1L) {
      result <- (result * b) %% mod
    }
    b <- (b * b) %% mod
    e <- e %/% 2L
  }
  result
}

