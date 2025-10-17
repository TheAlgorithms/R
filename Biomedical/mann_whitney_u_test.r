# Mann-Whitney U Test (Wilcoxon Rank-Sum Test) for Biomedical Data Analysis
# Author: Contributor
# Description: Implementation of Mann-Whitney U test for comparing two independent groups
# Common applications: Comparing treatment vs control groups, comparing different populations

mann_whitney_u_test <- function(x, y, alternative = "two.sided", exact = FALSE) {
  #' Mann-Whitney U Test (Wilcoxon Rank-Sum Test)
  #' 
  #' Performs the Mann-Whitney U test to compare two independent samples
  #' 
  #' @param x numeric vector of data values for group 1
  #' @param y numeric vector of data values for group 2
  #' @param alternative character, alternative hypothesis ("two.sided", "less", "greater")
  #' @param exact logical, whether to compute exact p-values for small samples
  #' 
  #' @return list containing:
  #'   - statistic: test statistic (U)
  #'   - p_value: p-value of the test
  #'   - U1: U statistic for group 1
  #'   - U2: U statistic for group 2
  #'   - W1: sum of ranks for group 1
  #'   - W2: sum of ranks for group 2
  #'   - n1: sample size of group 1
  #'   - n2: sample size of group 2
  #'   - method: description of test performed
  #'   - alternative: alternative hypothesis
  #'   - data_summary: summary of input data
  
  # Input validation
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric vectors")
  }
  
  if (length(x) == 0 || length(y) == 0) {
    stop("Both groups must contain at least one observation")
  }
  
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop("alternative must be 'two.sided', 'less', or 'greater'")
  }
  
  # Remove missing values
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  n1 <- length(x)
  n2 <- length(y)
  
  if (n1 == 0 || n2 == 0) {
    stop("Groups cannot be empty after removing missing values")
  }
  
  # Combine data and calculate ranks
  combined <- c(x, y)
  N <- n1 + n2
  ranks <- rank(combined, ties.method = "average")
  
  # Sum of ranks for each group
  W1 <- sum(ranks[1:n1])                    # Sum of ranks for group 1
  W2 <- sum(ranks[(n1 + 1):N])            # Sum of ranks for group 2
  
  # Calculate U statistics
  U1 <- W1 - n1 * (n1 + 1) / 2
  U2 <- W2 - n2 * (n2 + 1) / 2
  
  # Verify: U1 + U2 should equal n1 * n2
  if (abs(U1 + U2 - n1 * n2) > 1e-10) {
    warning("U statistics calculation may have numerical errors")
  }
  
  # Test statistic (traditionally the smaller U, but depends on alternative)
  if (alternative == "greater") {
    # Testing if group 1 > group 2, so we want U1
    U_stat <- U1
  } else if (alternative == "less") {
    # Testing if group 1 < group 2, so we want U1 (smaller values)
    U_stat <- U1
  } else {
    # Two-sided: use the smaller U for traditional reporting
    U_stat <- min(U1, U2)
  }
  
  # Calculate p-value
  if (n1 >= 8 && n2 >= 8 && !exact) {
    # Normal approximation for large samples
    mu_U <- n1 * n2 / 2
    
    # Check for ties and adjust variance if necessary
    ties <- table(combined)
    tie_correction <- sum(ties^3 - ties) / (N * (N - 1))
    
    var_U <- n1 * n2 * (N + 1) / 12 - n1 * n2 * tie_correction / (12 * (N - 1))
    sigma_U <- sqrt(var_U)
    
    # Apply continuity correction
    if (alternative == "two.sided") {
      z <- (abs(U1 - mu_U) - 0.5) / sigma_U
      p_value <- 2 * pnorm(z, lower.tail = FALSE)
    } else if (alternative == "less") {
      z <- (U1 + 0.5 - mu_U) / sigma_U
      p_value <- pnorm(z)
    } else { # greater
      z <- (U1 - 0.5 - mu_U) / sigma_U
      p_value <- pnorm(z, lower.tail = FALSE)
    }
    
    method <- "Mann-Whitney U test with normal approximation"
    
  } else {
    # For small samples or when exact is requested
    if (exact && n1 <= 20 && n2 <= 20) {
      # Note: Exact calculation would require generating all possible rank combinations
      # This is computationally intensive and typically done using lookup tables
      p_value <- NA
      method <- "Mann-Whitney U test (exact method - requires lookup tables)"
      warning("Exact p-value calculation requires specialized tables. Using NA.")
    } else {
      p_value <- NA
      method <- "Mann-Whitney U test"
      warning("Sample sizes are small. Consider using exact tables for p-value calculation.")
    }
  }
  
  # Prepare data summary
  data_summary <- list(
    n1 = n1,
    n2 = n2,
    median_x = median(x),
    mean_x = mean(x),
    sd_x = sd(x),
    median_y = median(y),
    mean_y = mean(y),
    sd_y = sd(y),
    combined_median = median(combined),
    combined_mean = mean(combined)
  )
  
  # Return results
  result <- list(
    statistic = U_stat,
    p_value = p_value,
    U1 = U1,
    U2 = U2,
    W1 = W1,
    W2 = W2,
    n1 = n1,
    n2 = n2,
    method = method,
    alternative = alternative,
    data_summary = data_summary
  )
  
  class(result) <- "mann_whitney_test"
  return(result)
}

# Print method for mann_whitney_test objects
print.mann_whitney_test <- function(x, ...) {
  cat("\n", x$method, "\n")
  cat("Data summary:\n")
  cat("  Group 1 (x): n =", x$n1, ", median =", round(x$data_summary$median_x, 3), 
      ", mean =", round(x$data_summary$mean_x, 3), ", SD =", round(x$data_summary$sd_x, 3), "\n")
  cat("  Group 2 (y): n =", x$n2, ", median =", round(x$data_summary$median_y, 3), 
      ", mean =", round(x$data_summary$mean_y, 3), ", SD =", round(x$data_summary$sd_y, 3), "\n")
  
  cat("\nTest results:\n")
  cat("  W1 (sum of ranks, group 1) =", x$W1, "\n")
  cat("  W2 (sum of ranks, group 2) =", x$W2, "\n")
  cat("  U1 (U statistic, group 1) =", x$U1, "\n")
  cat("  U2 (U statistic, group 2) =", x$U2, "\n")
  cat("  Test statistic U =", x$statistic, "\n")
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
    cat("  P-value: Not calculated (exact method required)\n")
  }
  
  # Effect size estimate
  cat("  Effect size estimate (r) =", round(abs(x$U1 - x$n1 * x$n2 / 2) / sqrt(x$n1 * x$n2 * (x$n1 + x$n2 + 1) / 12), 3), "\n")
}

# ==============================================================================
# EXAMPLES WITH BIOMEDICAL DUMMY DATA
# ==============================================================================

run_biomedical_examples <- function() {
  cat("=================================================================\n")
  cat("MANN-WHITNEY U TEST - BIOMEDICAL EXAMPLES\n")
  cat("=================================================================\n\n")
  
  # Example 1: Treatment vs Control group - Drug efficacy
  cat("EXAMPLE 1: Drug Efficacy Study (Treatment vs Control)\n")
  cat("-----------------------------------------------------------------\n")
  
  # Dummy data: Improvement scores (0-100)
  set.seed(123)
  treatment_group <- c(78, 85, 92, 73, 88, 91, 76, 83, 89, 95, 
                      80, 87, 94, 71, 86, 93, 79, 84, 90, 77)
  
  control_group <- c(65, 58, 71, 62, 69, 54, 67, 60, 73, 56,
                    63, 70, 59, 66, 52, 68, 61, 74, 57, 64)
  
  cat("Treatment group scores (n=20):", treatment_group, "\n")
  cat("Control group scores (n=20):", control_group, "\n\n")
  
  result1 <- mann_whitney_u_test(treatment_group, control_group, 
                                alternative = "greater")
  print(result1)
  
  cat("\nClinical Interpretation:\n")
  cat("This test compares the effectiveness of a new drug vs placebo.\n")
  cat("H0: No difference between treatment and control groups\n")
  cat("H1: Treatment group shows greater improvement than control\n\n")
  
  # Example 2: Male vs Female biomarker levels
  cat("\n=================================================================\n")
  cat("EXAMPLE 2: Biomarker Analysis by Gender (Male vs Female)\n")
  cat("-----------------------------------------------------------------\n")
  
  # Dummy data: Protein biomarker levels (ng/mL)
  set.seed(456)
  male_levels <- c(12.3, 15.7, 11.8, 14.2, 16.1, 13.5, 12.9, 15.3, 14.8, 13.1,
                  11.6, 16.4, 12.7, 14.9, 13.8, 15.2, 12.1, 14.6, 13.3, 15.9)
  
  female_levels <- c(9.8, 10.5, 8.9, 11.2, 9.3, 10.8, 8.7, 10.1, 9.6, 11.5,
                    8.4, 10.9, 9.1, 10.3, 8.8, 11.1, 9.4, 10.6, 8.6, 10.2)
  
  cat("Male biomarker levels (ng/mL):", round(male_levels, 1), "\n")
  cat("Female biomarker levels (ng/mL):", round(female_levels, 1), "\n\n")
  
  result2 <- mann_whitney_u_test(male_levels, female_levels, 
                                alternative = "two.sided")
  print(result2)
  
  cat("\nClinical Interpretation:\n")
  cat("This test examines gender differences in biomarker expression.\n")
  cat("H0: No difference in biomarker levels between males and females\n")
  cat("H1: There is a difference in biomarker levels between genders\n\n")
  
  # Example 3: Disease severity - Stage I vs Stage II
  cat("\n=================================================================\n")
  cat("EXAMPLE 3: Disease Severity (Stage I vs Stage II)\n")
  cat("-----------------------------------------------------------------\n")
  
  # Dummy data: Disease severity scores
  set.seed(789)
  stage_1 <- c(18, 22, 15, 20, 25, 17, 19, 23, 16, 21, 24, 14, 18, 22, 19)
  stage_2 <- c(35, 42, 38, 29, 45, 33, 40, 37, 31, 44, 36, 39, 32, 43, 34, 41, 28, 46, 30)
  
  cat("Stage I severity scores (n=15):", stage_1, "\n")
  cat("Stage II severity scores (n=19):", stage_2, "\n\n")
  
  result3 <- mann_whitney_u_test(stage_1, stage_2, alternative = "less")
  print(result3)
  
  cat("\nClinical Interpretation:\n")
  cat("This test compares disease severity between different stages.\n")
  cat("H0: No difference in severity between Stage I and Stage II\n")
  cat("H1: Stage I has lower severity scores than Stage II\n\n")
  
  # Example 4: Age groups - Young vs Elderly immune response
  cat("\n=================================================================\n")
  cat("EXAMPLE 4: Immune Response by Age Group (Young vs Elderly)\n")
  cat("-----------------------------------------------------------------\n")
  
  # Dummy data: Antibody titers (IU/mL)
  set.seed(321)
  young_adults <- c(450, 520, 380, 490, 560, 420, 480, 510, 440, 500,
                   470, 530, 410, 485, 525, 395, 475, 515, 435, 495)
  
  elderly <- c(280, 320, 250, 310, 290, 270, 300, 260, 330, 285,
              275, 315, 255, 305, 295, 265, 325, 245, 290, 280, 310, 270)
  
  cat("Young adults antibody titers (IU/mL):", young_adults, "\n")
  cat("Elderly antibody titers (IU/mL):", elderly, "\n\n")
  
  result4 <- mann_whitney_u_test(young_adults, elderly, alternative = "greater")
  print(result4)
  
  cat("\nClinical Interpretation:\n")
  cat("This test compares immune response between age groups.\n")
  cat("H0: No difference in antibody titers between young and elderly\n")
  cat("H1: Young adults have higher antibody titers than elderly\n\n")
  
  # Example 5: Pre-diabetic vs Diabetic glucose levels
  cat("\n=================================================================\n")
  cat("EXAMPLE 5: Glucose Levels (Pre-diabetic vs Diabetic)\n")
  cat("-----------------------------------------------------------------\n")
  
  # Dummy data: Fasting glucose levels (mg/dL)
  set.seed(654)
  prediabetic <- c(108, 115, 102, 118, 125, 110, 112, 120, 105, 117,
                  122, 106, 114, 119, 103, 116, 123, 107, 113, 121)
  
  diabetic <- c(145, 165, 138, 172, 156, 149, 183, 142, 168, 151,
               178, 135, 161, 154, 175, 140, 164, 158, 181, 147, 170, 153)
  
  cat("Pre-diabetic glucose levels (mg/dL):", prediabetic, "\n")
  cat("Diabetic glucose levels (mg/dL):", diabetic, "\n\n")
  
  result5 <- mann_whitney_u_test(prediabetic, diabetic, alternative = "less")
  print(result5)
  
  cat("\nClinical Interpretation:\n")
  cat("This test compares glucose levels between pre-diabetic and diabetic patients.\n")
  cat("H0: No difference in glucose levels between groups\n")
  cat("H1: Pre-diabetic patients have lower glucose levels than diabetic patients\n\n")
  
  cat("=================================================================\n")
  cat("END OF EXAMPLES\n")
  cat("=================================================================\n")
}

# Helper function to compare with built-in R function (if available)
compare_with_r_builtin <- function() {
  cat("\n=================================================================\n")
  cat("COMPARISON WITH R's BUILT-IN wilcox.test() FUNCTION\n")
  cat("=================================================================\n\n")
  
  # Generate sample data
  set.seed(123)
  group1 <- rnorm(15, mean = 50, sd = 10)
  group2 <- rnorm(18, mean = 45, sd = 12)
  
  cat("Comparison using sample data:\n")
  cat("Group 1:", round(group1, 2), "\n")
  cat("Group 2:", round(group2, 2), "\n\n")
  
  # Our implementation
  cat("Our implementation:\n")
  our_result <- mann_whitney_u_test(group1, group2, alternative = "two.sided")
  print(our_result)
  
  # R's built-in function
  cat("\nR's built-in wilcox.test():\n")
  r_result <- wilcox.test(group1, group2, alternative = "two.sided", exact = FALSE)
  print(r_result)
  
  cat("\nNote: Small differences may occur due to different tie-handling methods\n")
  cat("and continuity corrections, but results should be very similar.\n")
}

# Run the examples when the script is executed
if (interactive()) {
  cat("Loading Mann-Whitney U Test implementation...\n")
  cat("Run 'run_biomedical_examples()' to see biomedical examples.\n")
  cat("Run 'compare_with_r_builtin()' to compare with R's wilcox.test().\n")
} else {
  run_biomedical_examples()
}