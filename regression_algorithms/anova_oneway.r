#' One-way ANOVA (from scratch)
#'
#' @description
#' Computes a one-way Analysis of Variance (ANOVA) to test whether k group means
#' are equal. Implements sums of squares, F statistic, and p-value without using \code{aov()}.
#'
#' @param x Numeric vector of observations.
#' @param g Factor or character vector of group labels (same length as \code{x}).
#'
#' @return A list with:
#' \item{df_between}{Degrees of freedom between groups (k - 1).}
#' \item{df_within}{Degrees of freedom within groups (n - k).}
#' \item{ss_between}{Between-group sum of squares.}
#' \item{ss_within}{Within-group sum of squares.}
#' \item{ms_between}{Between-group mean square.}
#' \item{ms_within}{Within-group mean square.}
#' \item{F}{F-statistic.}
#' \item{p_value}{Right-tail p-value from F distribution.}
#'
#' @details
#' One-way ANOVA partitions total variance into between-group and within-group components:
#' \deqn{SS_T = SS_B + SS_W.}
#' The test statistic \eqn{F = MS_B / MS_W} follows an F distribution under H0 (equal means).
#'
#' @examples
#' set.seed(0)
#' x <- c(rnorm(20, 0, 1), rnorm(22, 0.2, 1), rnorm(18, -0.1, 1))
#' g <- factor(rep(c("A", "B", "C"), times = c(20, 22, 18)))
#' res <- anova_oneway(x, g)
#' res$F; res$p_value
#'
#' @export
anova_oneway <- function(x, g) {
  if (!is.numeric(x)) stop("`x` must be numeric.")
  if (length(x) != length(g)) stop("`x` and `g` must have the same length.")
  if (!is.factor(g)) g <- factor(g)

  n <- length(x)
  k <- nlevels(g)
  if (k < 2L) stop("Need at least 2 groups.")
  if (n <= k) stop("Total observations must exceed number of groups.")

  grand_mean <- mean(x)

  # group stats
  group_means <- tapply(x, g, mean)
  group_sizes <- tapply(x, g, length)

  # SS_between = sum_{groups} n_j * (mean_j - grand_mean)^2
  ss_between <- sum(group_sizes * (group_means - grand_mean)^2)

  # SS_within = sum over groups of sum (x_ij - mean_j)^2
  ss_within <- sum(unlist(tapply(x, g, function(v) sum((v - mean(v))^2))))

  df_between <- k - 1L
  df_within  <- n - k

  ms_between <- ss_between / df_between
  ms_within  <- ss_within  / df_within

  F_stat <- ms_between / ms_within
  p_val  <- stats::pf(F_stat, df_between, df_within, lower.tail = FALSE)

  list(
    df_between = df_between,
    df_within  = df_within,
    ss_between = ss_between,
    ss_within  = ss_within,
    ms_between = ms_between,
    ms_within  = ms_within,
    F = F_stat,
    p_value = p_val
  )
}

# --- Simple self-test (uncomment to run locally) ---
# set.seed(1)
# x <- c(rnorm(15, 0, 1), rnorm(15, 0.5, 1), rnorm(15, 1, 1))
# g <- factor(rep(c("A","B","C"), each = 15))
# res <- anova_oneway(x, g)
# stopifnot(res$F > 0, res$p_value >= 0, res$p_value <= 1)
