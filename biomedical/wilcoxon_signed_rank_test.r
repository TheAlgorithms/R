# Wilcoxon Signed-Rank Test for Biomedical Data Analysis
# Author: Contributor
# Description: Implementation of Wilcoxon signed-rank test for paired samples
# Common applications: Before/after treatment comparisons, paired clinical measurements

wilcoxon_signed_rank_test <- function(x, y = NULL, paired = TRUE, alternative = "two.sided", mu = 0) {
  #' Wilcoxon Signed-Rank Test
  #' 
  #' Performs the Wilcoxon signed-rank test on paired samples or single sample against a median
  #' 
  #' @param x numeric vector of data values (first group or single sample)
  #' @param y numeric vector of data values (second group for paired test), default NULL
  #' @param paired logical, whether the test is for paired samples, default TRUE
  #' @param alternative character, alternative hypothesis ("two.sided", "less", "greater")
  #' @param mu numeric, hypothesized median for single sample test, default 0
  #' 
  #' @return list containing:
  #'   - statistic: test statistic (W)
  #'   - p_value: p-value of the test
  #'   - W_plus: sum of positive ranks
  #'   - W_minus: sum of negative ranks
  #'   - n: effective sample size (after removing zeros)
  #'   - method: description of test performed
  #'   - alternative: alternative hypothesis
  #'   - data_summary: summary of input data
  
  # Input validation
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }
  
  if (!is.null(y) && !is.numeric(y)) {
    stop("y must be a numeric vector or NULL")
  }
  
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop("alternative must be 'two.sided', 'less', or 'greater'")
  }
  
  # Remove NA values early
  if (is.null(y)) {
    # One-sample test against median mu
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      stop("No non-missing observations in x")
    }
    differences <- x - mu
    test_type <- "One-sample Wilcoxon signed-rank test"
  } else {
    # Paired samples test - remove NA pairs
    if (length(x) != length(y)) {
      stop("x and y must have the same length for paired test")
    }
    idx <- complete.cases(x, y)
    if (sum(idx) == 0) {
      stop("No complete cases found")
    }
    x <- x[idx]
    y <- y[idx]
    differences <- x - y
    test_type <- "Paired samples Wilcoxon signed-rank test"
  }
  
  # Remove zeros (ties at the hypothesized median)
  non_zero_diff <- differences[differences != 0]
  n <- length(non_zero_diff)
  
  if (n == 0) {
    stop("No non-zero differences found. All values are tied at the hypothesized median.")
  }
  
  # Calculate ranks of absolute differences
  abs_diff <- abs(non_zero_diff)
  ranks <- rank(abs_diff, ties.method = "average")
  
  # Sum of ranks for positive and negative differences
  W_plus <- sum(ranks[non_zero_diff > 0])
  W_minus <- sum(ranks[non_zero_diff < 0])
  
  # Test statistic selection based on alternative hypothesis
  # For one-sided tests, always use W_plus as the test statistic
  if (alternative == "greater" || alternative == "less") {
    W <- W_plus
  } else {
    # For two-sided, use the smaller of the two (traditional approach)
    W <- min(W_plus, W_minus)
  }
  
  # Calculate p-value using normal approximation for large samples (n >= 10)
  if (n >= 10) {
    # Mean of W_plus under null hypothesis
    mu_W <- n * (n + 1) / 4
    
    # Variance with tie correction
    tie_counts <- table(abs_diff)
    tie_correction <- sum(tie_counts^3 - tie_counts)
    var_W <- (n * (n + 1) * (2 * n + 1) - tie_correction) / 24
    sigma_W <- sqrt(var_W)
    
    # Apply continuity correction and compute p-values
    if (alternative == "two.sided") {
      z <- (abs(W - mu_W) - 0.5) / sigma_W
      p_value <- 2 * pnorm(z, lower.tail = FALSE)
    } else if (alternative == "less") {
      # For 'less': test if W_plus is unusually small
      z <- (W_plus + 0.5 - mu_W) / sigma_W
      p_value <- pnorm(z, lower.tail = TRUE)
    } else { # greater
      # For 'greater': test if W_plus is unusually large
      z <- (W_plus - 0.5 - mu_W) / sigma_W
      p_value <- pnorm(z, lower.tail = FALSE)
    }
  } else {
    # For small samples, exact p-values would require lookup tables
    p_value <- NA
    warning("Sample size is small (n < 10). P-value calculation requires exact tables.")
  }
  
  # Prepare data summary
  if (is.null(y)) {
    data_summary <- list(
      sample_size = length(x),
      median_x = median(x),
      mean_x = mean(x),
      hypothesized_median = mu
    )
  } else {
    data_summary <- list(
      sample_size = length(x),
      median_x = median(x),
      mean_x = mean(x),
      median_y = median(y),
      mean_y = mean(y),
      median_difference = median(differences),
      mean_difference = mean(differences)
    )
  }
  
  # Return results
  result <- list(
    statistic = W,
    p_value = p_value,
    W_plus = W_plus,
    W_minus = W_minus,
    n = n,
    method = test_type,
    alternative = alternative,
    data_summary = data_summary
  )
  
  class(result) <- "wilcoxon_test"
  return(result)
}

# Print method for wilcoxon_test objects
print.wilcoxon_test <- function(x, ...) {
  cat("\n", x$method, "\n")
  cat("Data summary:\n")
  if (!is.null(x$data_summary$median_y)) {
    cat("  Group 1 (x): median =", round(x$data_summary$median_x, 3), 
        ", mean =", round(x$data_summary$mean_x, 3), "\n")
    cat("  Group 2 (y): median =", round(x$data_summary$median_y, 3), 
        ", mean =", round(x$data_summary$mean_y, 3), "\n")
    cat("  Differences: median =", round(x$data_summary$median_difference, 3), 
        ", mean =", round(x$data_summary$mean_difference, 3), "\n")
  } else {
    cat("  Sample: median =", round(x$data_summary$median_x, 3), 
        ", mean =", round(x$data_summary$mean_x, 3), "\n")
    cat("  Hypothesized median =", x$data_summary$hypothesized_median, "\n")
  }
  
  cat("\nTest results:\n")
  cat("  W+ (sum of positive ranks) =", x$W_plus, "\n")
  cat("  W- (sum of negative ranks) =", x$W_minus, "\n")
  cat("  Test statistic W =", x$statistic, "\n")
  cat("  Effective sample size =", x$n, "\n")
  cat("  Alternative hypothesis:", x$alternative, "\n")
  
  if (!is.na(x$p_value)) {
    cat("  P-value =", round(x$p_value, 6), "\n")
    
    # Interpretation
    if (x$p_value < 0.001) {
      significance <- "highly significant (p < 0.001)"
    } else if (x$p_value < 0.01) {
      significance <- "very significant (p < 0.01)"
    } else if (x$p_value < 0.05) {
      significance <- "significant (p < 0.05)"
    } else {
      significance <- "not significant (p >= 0.05)"
    }
    cat("  Result:", significance, "\n")
  } else {
    cat("  P-value: Not calculated (small sample size)\n")
  }
}

# ==============================================================================
# EXAMPLE WITH BIOMEDICAL DUMMY DATA
# ==============================================================================

run_biomedical_examples <- function() {
  cat("=================================================================\n")
  cat("WILCOXON SIGNED-RANK TEST - BIOMEDICAL EXAMPLES\n")
  cat("=================================================================\n\n")
  
  # Example 1: Blood pressure before and after treatment
  cat("EXAMPLE 1: Blood Pressure Analysis (Before vs After Treatment)\n")
  cat("-----------------------------------------------------------------\n")
  
  # Dummy data: Systolic blood pressure (mmHg)
  set.seed(123)  # For reproducible results
  before_treatment <- c(145, 150, 138, 155, 142, 148, 152, 140, 146, 149,
                       143, 151, 147, 153, 141, 144, 156, 139, 145, 150)
  
  after_treatment <- c(138, 142, 135, 148, 136, 140, 145, 133, 139, 143,
                      137, 144, 141, 146, 134, 138, 149, 132, 140, 144)
  
  cat("Before treatment (n=20):", before_treatment, "\n")
  cat("After treatment (n=20):", after_treatment, "\n\n")
  
  # Perform the test
  result1 <- wilcoxon_signed_rank_test(before_treatment, after_treatment, 
                                      alternative = "greater")
  print(result1)
  
  cat("\nClinical Interpretation:\n")
  cat("This test examines whether the treatment significantly reduces blood pressure.\n")
  cat("H0: The treatment has no effect (median difference = 0)\n")
  cat("H1: The treatment reduces blood pressure (before > after)\n\n")
  
  # Example 2: Pain scores before and after medication
  cat("\n=================================================================\n")
  cat("EXAMPLE 2: Pain Score Analysis (Before vs After Medication)\n")
  cat("-----------------------------------------------------------------\n")
  
  # Dummy data: Pain scores on a scale of 0-10
  set.seed(456)
  pain_before <- c(8, 7, 9, 6, 8, 7, 9, 8, 7, 6, 8, 9, 7, 8, 6, 9, 7, 8, 6, 7)
  pain_after <- c(4, 3, 5, 2, 4, 3, 5, 4, 3, 2, 4, 5, 3, 4, 2, 5, 3, 4, 2, 3)
  
  cat("Pain before medication (0-10 scale):", pain_before, "\n")
  cat("Pain after medication (0-10 scale):", pain_after, "\n\n")
  
  result2 <- wilcoxon_signed_rank_test(pain_before, pain_after, 
                                      alternative = "greater")
  print(result2)
  
  cat("\nClinical Interpretation:\n")
  cat("This test evaluates the effectiveness of pain medication.\n")
  cat("H0: The medication has no effect on pain levels\n")
  cat("H1: The medication reduces pain levels (before > after)\n\n")
  
  # Example 3: Weight loss study
  cat("\n=================================================================\n")
  cat("EXAMPLE 3: Weight Loss Study (Before vs After Diet Program)\n")
  cat("-----------------------------------------------------------------\n")
  
  # Dummy data: Weight in kg
  set.seed(789)
  weight_before <- c(85.2, 92.1, 78.5, 88.9, 95.3, 81.7, 86.4, 90.8, 83.2, 87.6,
                    91.3, 84.1, 89.7, 82.5, 94.2, 80.9, 88.1, 86.7, 93.4, 85.8)
  
  weight_after <- c(82.1, 89.3, 76.2, 85.7, 91.8, 79.4, 83.9, 87.5, 80.8, 84.3,
                   88.1, 81.6, 86.4, 80.1, 90.7, 78.5, 85.2, 83.9, 89.8, 82.9)
  
  cat("Weight before program (kg):", round(weight_before, 1), "\n")
  cat("Weight after program (kg):", round(weight_after, 1), "\n\n")
  
  result3 <- wilcoxon_signed_rank_test(weight_before, weight_after, 
                                      alternative = "greater")
  print(result3)
  
  cat("\nClinical Interpretation:\n")
  cat("This test assesses the effectiveness of a weight loss program.\n")
  cat("H0: The program has no effect on weight\n")
  cat("H1: The program reduces weight (before > after)\n\n")
  
  # Example 4: One-sample test - cholesterol levels vs normal
  cat("\n=================================================================\n")
  cat("EXAMPLE 4: Cholesterol Levels vs Normal Range (One-sample test)\n")
  cat("-----------------------------------------------------------------\n")
  
  # Dummy data: Total cholesterol levels (mg/dL)
  # Normal level is considered to be 200 mg/dL
  set.seed(321)
  cholesterol_levels <- c(215, 230, 195, 220, 240, 185, 225, 210, 235, 205,
                         245, 190, 228, 218, 232, 198, 242, 208, 226, 214)
  
  normal_cholesterol <- 200  # mg/dL
  
  cat("Cholesterol levels (mg/dL):", cholesterol_levels, "\n")
  cat("Normal level (reference):", normal_cholesterol, "mg/dL\n\n")
  
  result4 <- wilcoxon_signed_rank_test(cholesterol_levels, mu = normal_cholesterol, 
                                      alternative = "greater")
  print(result4)
  
  cat("\nClinical Interpretation:\n")
  cat("This test determines if the population has elevated cholesterol levels.\n")
  cat("H0: Median cholesterol level = 200 mg/dL (normal)\n")
  cat("H1: Median cholesterol level > 200 mg/dL (elevated)\n\n")
  
  cat("=================================================================\n")
  cat("END OF EXAMPLES\n")
  cat("=================================================================\n")
}

# Examples are available but not run automatically to avoid side effects
# To run examples, execute: run_biomedical_examples()
if (interactive()) {
  cat("Loading Wilcoxon Signed-Rank Test implementation...\n")
  cat("Run 'run_biomedical_examples()' to see biomedical examples.\n")
}

# Uncomment the following line to run examples automatically:
# run_biomedical_examples()