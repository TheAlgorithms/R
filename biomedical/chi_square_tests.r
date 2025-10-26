# Chi-Square Tests for Biomedical Data Analysis
#
# This implementation provides comprehensive chi-square testing capabilities:
# 1. Chi-square goodness of fit test (compare observed vs expected frequencies)
# 2. Chi-square test of independence (test association between categorical variables)
# 3. Chi-square test for homogeneity (compare distributions across groups)
# 4. McNemar's test (for paired categorical data)
#
# Applications in biomedical research:
# - Clinical trials: testing treatment response patterns
# - Epidemiology: testing associations between risk factors and diseases
# - Genetics: testing Hardy-Weinberg equilibrium, allele frequencies
# - Public health: analyzing categorical health outcomes
# - Quality control: testing distribution patterns in lab results
#
# Time Complexity: O(n) for data processing, O(r*c) for contingency tables
# Space Complexity: O(r*c) for storing contingency tables

# Helper function for creating professional contingency table visualizations
create_contingency_plot <- function(observed, expected = NULL, main_title = "Contingency Table Analysis") {
  # Set up color palette
  colors <- c("#E8F4F8", "#B8E0D2", "#95A5A6", "#D5A6BD", "#F7DC6F", "#F8C471")
  
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # 1. Observed frequencies heatmap
  if (is.matrix(observed)) {
    image(1:ncol(observed), 1:nrow(observed), t(observed[nrow(observed):1, ]), 
          col = heat.colors(20), main = "Observed Frequencies", 
          xlab = "Column Variable", ylab = "Row Variable", axes = FALSE)
    axis(1, at = 1:ncol(observed), labels = colnames(observed))
    axis(2, at = 1:nrow(observed), labels = rev(rownames(observed)))
    
    # Add text values
    for (i in 1:nrow(observed)) {
      for (j in 1:ncol(observed)) {
        text(j, nrow(observed) - i + 1, observed[i, j], cex = 1.2, font = 2)
      }
    }
  }
  
  # 2. Expected frequencies heatmap (if provided)
  if (!is.null(expected) && is.matrix(expected)) {
    image(1:ncol(expected), 1:nrow(expected), t(expected[nrow(expected):1, ]), 
          col = terrain.colors(20), main = "Expected Frequencies", 
          xlab = "Column Variable", ylab = "Row Variable", axes = FALSE)
    axis(1, at = 1:ncol(expected), labels = colnames(expected))
    axis(2, at = 1:nrow(expected), labels = rev(rownames(expected)))
    
    # Add text values
    for (i in 1:nrow(expected)) {
      for (j in 1:ncol(expected)) {
        text(j, nrow(expected) - i + 1, round(expected[i, j], 1), cex = 1.2, font = 2)
      }
    }
  }
  
  # 3. Residuals plot (if expected provided)
  if (!is.null(expected)) {
    residuals <- (observed - expected) / sqrt(expected)
    image(1:ncol(residuals), 1:nrow(residuals), t(residuals[nrow(residuals):1, ]), 
          col = cm.colors(20), main = "Standardized Residuals", 
          xlab = "Column Variable", ylab = "Row Variable", axes = FALSE)
    axis(1, at = 1:ncol(residuals), labels = colnames(residuals))
    axis(2, at = 1:nrow(residuals), labels = rev(rownames(residuals)))
    
    # Add text values with color coding
    for (i in 1:nrow(residuals)) {
      for (j in 1:ncol(residuals)) {
        color <- ifelse(abs(residuals[i, j]) > 2, "red", "black")
        text(j, nrow(residuals) - i + 1, round(residuals[i, j], 2), 
             cex = 1.1, font = 2, col = color)
      }
    }
  }
}

# Chi-square goodness of fit test
chi_square_goodness_of_fit <- function(observed, expected = NULL, labels = NULL, 
                                       plot = TRUE, plot_title = "Goodness of Fit Test") {
  #' Chi-Square Goodness of Fit Test
  #' 
  #' Tests whether observed frequencies differ significantly from expected frequencies
  #' 
  #' @param observed numeric vector of observed frequencies
  #' @param expected numeric vector of expected frequencies (or NULL for equal proportions)
  #' @param labels character vector of category labels
  #' @param plot logical: whether to create visualization
  #' @param plot_title character: title for the plot
  #' @return list with test results
  
  # Input validation
  if (!is.numeric(observed) || any(observed < 0)) {
    stop("observed must be a numeric vector with non-negative values")
  }
  
  if (any(observed != floor(observed))) {
    warning("observed frequencies should be integers (counts)")
  }
  
  k <- length(observed)  # number of categories
  
  if (is.null(expected)) {
    # Equal expected frequencies
    total <- sum(observed)
    expected <- rep(total / k, k)
  } else {
    if (length(expected) != k) {
      stop("observed and expected must have the same length")
    }
    if (any(expected <= 0)) {
      stop("expected frequencies must be positive")
    }
    # Scale expected to match total observed
    expected <- expected * sum(observed) / sum(expected)
  }
  
  # Check minimum expected frequency rule
  min_expected <- min(expected)
  if (min_expected < 5) {
    warning(paste("Minimum expected frequency is", round(min_expected, 2), 
                 "< 5. Results may be unreliable."))
  }
  
  # Calculate chi-square statistic
  chi_square <- sum((observed - expected)^2 / expected)
  df <- k - 1
  p_value <- pchisq(chi_square, df, lower.tail = FALSE)
  
  # Calculate residuals
  residuals <- (observed - expected) / sqrt(expected)
  standardized_residuals <- residuals / sqrt(1 - 1/k)
  
  # Prepare labels
  if (is.null(labels)) {
    labels <- paste("Category", 1:k)
  }
  
  # Create visualization
  if (plot) {
    par(mfrow = c(2, 2), mar = c(5, 4, 3, 2))
    
    # 1. Bar plot of observed vs expected
    barplot_data <- rbind(observed, expected)
    rownames(barplot_data) <- c("Observed", "Expected")
    colnames(barplot_data) <- labels
    
    barplot(barplot_data, beside = TRUE, col = c("lightblue", "lightcoral"),
            main = paste(plot_title, "\nObserved vs Expected"), 
            ylab = "Frequency", legend = TRUE, las = 2)
    
    # 2. Residuals plot
    barplot(residuals, names.arg = labels, col = ifelse(abs(residuals) > 2, "red", "lightgreen"),
            main = "Standardized Residuals", ylab = "Residual", las = 2)
    abline(h = c(-2, 2), col = "red", lty = 2)
    abline(h = 0, col = "black", lty = 1)
    
    # 3. Pie chart of observed frequencies
    pie(observed, labels = paste(labels, "\n(", observed, ")", sep = ""), 
        col = rainbow(k), main = "Observed Distribution")
    
    # 4. Test summary
    plot(1, 1, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
         main = "Test Summary", xlab = "", ylab = "", axes = FALSE)
    
    text(5, 9.5, "Chi-Square Goodness of Fit", cex = 1.2, font = 2)
    text(5, 8.7, paste("Chi-square statistic:", round(chi_square, 4)), cex = 1)
    text(5, 8.1, paste("Degrees of freedom:", df), cex = 1)
    text(5, 7.5, paste("p-value:", round(p_value, 6)), cex = 1)
    text(5, 6.9, paste("Sample size:", sum(observed)), cex = 1)
    
    # Significance
    sig_level <- ifelse(p_value < 0.001, "***", 
                       ifelse(p_value < 0.01, "**", 
                             ifelse(p_value < 0.05, "*", "ns")))
    text(5, 6.1, paste("Significance:", sig_level), cex = 1, 
         col = ifelse(sig_level == "ns", "red", "darkgreen"))
    
    if (p_value < 0.05) {
      text(5, 5.3, "Result: Reject H0", cex = 1, col = "darkgreen", font = 2)
      text(5, 4.7, "Observed ≠ Expected", cex = 1, col = "darkgreen")
    } else {
      text(5, 5.3, "Result: Fail to reject H0", cex = 1, col = "red", font = 2)
      text(5, 4.7, "Observed = Expected", cex = 1, col = "red")
    }
    
    # Effect size (Cramer's V for goodness of fit)
    cramers_v <- sqrt(chi_square / sum(observed))
    text(5, 3.9, paste("Cramer's V:", round(cramers_v, 4)), cex = 1)
    
    par(mfrow = c(1, 1))
  }
  
  # Return results
  result <- list(
    statistic = chi_square,
    p_value = p_value,
    degrees_of_freedom = df,
    observed = observed,
    expected = expected,
    residuals = residuals,
    standardized_residuals = standardized_residuals,
    cramers_v = sqrt(chi_square / sum(observed)),
    method = "Chi-square goodness of fit test",
    categories = labels,
    sample_size = sum(observed)
  )
  
  class(result) <- "biomedical_chisq"
  return(result)
}

# Chi-square test of independence
chi_square_independence <- function(x, y = NULL, plot = TRUE, 
                                   plot_title = "Test of Independence") {
  #' Chi-Square Test of Independence
  #' 
  #' Tests whether two categorical variables are independent
  #' 
  #' @param x either a contingency table (matrix) or a factor/vector for first variable
  #' @param y factor/vector for second variable (if x is not a matrix)
  #' @param plot logical: whether to create visualization
  #' @param plot_title character: title for the plot
  #' @return list with test results
  
  # Handle input formats
  if (is.matrix(x) || is.table(x)) {
    # x is already a contingency table
    observed <- as.matrix(x)
    if (is.null(rownames(observed))) rownames(observed) <- paste("Row", 1:nrow(observed))
    if (is.null(colnames(observed))) colnames(observed) <- paste("Col", 1:ncol(observed))
  } else {
    # Create contingency table from vectors
    if (is.null(y)) stop("If x is not a matrix, y must be provided")
    if (length(x) != length(y)) stop("x and y must have the same length")
    
    # Remove missing values
    complete_cases <- complete.cases(x, y)
    x <- x[complete_cases]
    y <- y[complete_cases]
    
    observed <- table(x, y)
    observed <- as.matrix(observed)
  }
  
  # Input validation
  if (any(observed < 0)) stop("All frequencies must be non-negative")
  if (sum(observed) == 0) stop("Total frequency cannot be zero")
  
  # Calculate expected frequencies
  row_totals <- rowSums(observed)
  col_totals <- colSums(observed)
  total <- sum(observed)
  
  expected <- outer(row_totals, col_totals) / total
  
  # Check minimum expected frequency rule
  min_expected <- min(expected)
  prop_low_expected <- mean(expected < 5)
  
  if (min_expected < 1) {
    stop("Some expected frequencies < 1. Test is not appropriate.")
  }
  
  if (prop_low_expected > 0.2) {
    warning(paste(round(prop_low_expected * 100, 1), 
                 "% of expected frequencies are < 5. Consider combining categories."))
  }
  
  # Calculate chi-square statistic
  chi_square <- sum((observed - expected)^2 / expected)
  df <- (nrow(observed) - 1) * (ncol(observed) - 1)
  p_value <- pchisq(chi_square, df, lower.tail = FALSE)
  
  # Calculate effect sizes
  n <- sum(observed)
  cramers_v <- sqrt(chi_square / (n * (min(nrow(observed), ncol(observed)) - 1)))
  phi_coefficient <- ifelse(nrow(observed) == 2 && ncol(observed) == 2, 
                           sqrt(chi_square / n), NA)
  
  # Calculate standardized residuals
  residuals <- (observed - expected) / sqrt(expected)
  adj_residuals <- residuals / sqrt((1 - row_totals/total) %*% t(1 - col_totals/total))
  
  # Create visualization
  if (plot) {
    create_contingency_plot(observed, expected, plot_title)
    
    # Additional summary plot
    plot(1, 1, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
         main = "Independence Test Summary", xlab = "", ylab = "", axes = FALSE)
    
    text(5, 9.5, "Chi-Square Test of Independence", cex = 1.2, font = 2)
    text(5, 8.7, paste("Chi-square statistic:", round(chi_square, 4)), cex = 1)
    text(5, 8.1, paste("Degrees of freedom:", df), cex = 1)
    text(5, 7.5, paste("p-value:", round(p_value, 6)), cex = 1)
    text(5, 6.9, paste("Sample size:", n), cex = 1)
    text(5, 6.3, paste("Cramer's V:", round(cramers_v, 4)), cex = 1)
    
    if (!is.na(phi_coefficient)) {
      text(5, 5.7, paste("Phi coefficient:", round(phi_coefficient, 4)), cex = 1)
    }
    
    # Significance
    sig_level <- ifelse(p_value < 0.001, "***", 
                       ifelse(p_value < 0.01, "**", 
                             ifelse(p_value < 0.05, "*", "ns")))
    text(5, 4.9, paste("Significance:", sig_level), cex = 1, 
         col = ifelse(sig_level == "ns", "red", "darkgreen"))
    
    if (p_value < 0.05) {
      text(5, 4.1, "Result: Variables are dependent", cex = 1, col = "darkgreen", font = 2)
    } else {
      text(5, 4.1, "Result: Variables are independent", cex = 1, col = "red", font = 2)
    }
    
    # Effect size interpretation
    effect_magnitude <- if (cramers_v < 0.1) "negligible"
                       else if (cramers_v < 0.3) "small"
                       else if (cramers_v < 0.5) "medium"
                       else "large"
    text(5, 3.3, paste("Effect size:", effect_magnitude), cex = 1)
  }
  
  # Return results
  result <- list(
    statistic = chi_square,
    p_value = p_value,
    degrees_of_freedom = df,
    observed = observed,
    expected = expected,
    residuals = residuals,
    adjusted_residuals = adj_residuals,
    cramers_v = cramers_v,
    phi_coefficient = phi_coefficient,
    method = "Chi-square test of independence",
    sample_size = n,
    row_totals = row_totals,
    col_totals = col_totals
  )
  
  class(result) <- "biomedical_chisq"
  return(result)
}

# McNemar's test for paired categorical data
mcnemar_test <- function(x, y = NULL, correct = TRUE, plot = TRUE, 
                        plot_title = "McNemar's Test") {
  #' McNemar's Test for Paired Categorical Data
  #' 
  #' Tests for changes in paired categorical data (e.g., before/after treatment)
  #' 
  #' @param x either a 2x2 contingency table or a factor/vector for condition 1
  #' @param y factor/vector for condition 2 (if x is not a matrix)
  #' @param correct logical: apply continuity correction
  #' @param plot logical: whether to create visualization
  #' @param plot_title character: title for the plot
  #' @return list with test results
  
  # Handle input formats
  if (is.matrix(x) || is.table(x)) {
    if (nrow(x) != 2 || ncol(x) != 2) {
      stop("For McNemar's test, contingency table must be 2x2")
    }
    contingency_table <- as.matrix(x)
  } else {
    if (is.null(y)) stop("If x is not a matrix, y must be provided")
    if (length(x) != length(y)) stop("x and y must have the same length")
    
    # Remove missing values
    complete_cases <- complete.cases(x, y)
    x <- x[complete_cases]
    y <- y[complete_cases]
    
    contingency_table <- table(x, y)
    if (nrow(contingency_table) != 2 || ncol(contingency_table) != 2) {
      stop("Both variables must have exactly 2 levels for McNemar's test")
    }
    contingency_table <- as.matrix(contingency_table)
  }
  
  # Extract discordant pairs
  b <- contingency_table[1, 2]  # changed from negative to positive
  c <- contingency_table[2, 1]  # changed from positive to negative
  
  # McNemar's test statistic
  if (correct && (b + c) > 0) {
    # With continuity correction
    chi_square <- (abs(b - c) - 1)^2 / (b + c)
  } else {
    # Without continuity correction
    chi_square <- (b - c)^2 / (b + c)
  }
  
  df <- 1
  p_value <- pchisq(chi_square, df, lower.tail = FALSE)
  
  # Calculate concordant pairs
  concordant <- contingency_table[1,1] + contingency_table[2,2]
  
  # Effect size (odds ratio for discordant pairs)
  if (c > 0) {
    odds_ratio <- b / c
    log_or_se <- sqrt(1/b + 1/c)
    or_ci_lower <- exp(log(odds_ratio) - 1.96 * log_or_se)
    or_ci_upper <- exp(log(odds_ratio) + 1.96 * log_or_se)
  } else {
    odds_ratio <- Inf
    or_ci_lower <- NA
    or_ci_upper <- NA
  }
  
  # Create visualization
  if (plot) {
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
    
    # 1. Contingency table heatmap
    image(1:2, 1:2, t(contingency_table[2:1, ]), col = heat.colors(10),
          main = "2x2 Contingency Table", xlab = "After", ylab = "Before", axes = FALSE)
    axis(1, at = 1:2, labels = colnames(contingency_table))
    axis(2, at = 1:2, labels = rev(rownames(contingency_table)))
    
    # Add cell values
    for (i in 1:2) {
      for (j in 1:2) {
        text(j, 3-i, contingency_table[i, j], cex = 2, font = 2)
      }
    }
    
    # 2. Concordant vs Discordant pairs
    discordant <- b + c
    
    barplot(c(concordant, discordant), names.arg = c("Concordant", "Discordant"),
            col = c("lightgreen", "lightcoral"), main = "Concordant vs Discordant Pairs",
            ylab = "Count")
    
    # 3. Change direction
    if (discordant > 0) {
      change_data <- c(b, c)
      names(change_data) <- c("- to +", "+ to -")
      barplot(change_data, col = c("lightblue", "orange"), 
              main = "Direction of Change", ylab = "Count")
    }
    
    # 4. Test summary
    plot(1, 1, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
         main = "McNemar Test Summary", xlab = "", ylab = "", axes = FALSE)
    
    text(5, 9.5, "McNemar's Test Results", cex = 1.2, font = 2)
    text(5, 8.7, paste("Chi-square statistic:", round(chi_square, 4)), cex = 1)
    text(5, 8.1, paste("Degrees of freedom:", df), cex = 1)
    text(5, 7.5, paste("p-value:", round(p_value, 6)), cex = 1)
    text(5, 6.9, paste("Continuity correction:", correct), cex = 1)
    text(5, 6.3, paste("Discordant pairs:", discordant), cex = 1)
    
    if (is.finite(odds_ratio)) {
      text(5, 5.7, paste("Odds ratio:", round(odds_ratio, 4)), cex = 1)
    }
    
    # Significance
    sig_level <- ifelse(p_value < 0.001, "***", 
                       ifelse(p_value < 0.01, "**", 
                             ifelse(p_value < 0.05, "*", "ns")))
    text(5, 4.9, paste("Significance:", sig_level), cex = 1, 
         col = ifelse(sig_level == "ns", "red", "darkgreen"))
    
    if (p_value < 0.05) {
      text(5, 4.1, "Result: Significant change", cex = 1, col = "darkgreen", font = 2)
    } else {
      text(5, 4.1, "Result: No significant change", cex = 1, col = "red", font = 2)
    }
    
    par(mfrow = c(1, 1))
  }
  
  # Return results
  result <- list(
    statistic = chi_square,
    p_value = p_value,
    degrees_of_freedom = df,
    contingency_table = contingency_table,
    discordant_pairs = c(b = b, c = c),
    concordant_pairs = concordant,
    odds_ratio = odds_ratio,
    odds_ratio_ci = c(or_ci_lower, or_ci_upper),
    continuity_correction = correct,
    method = "McNemar's test for paired data",
    sample_size = sum(contingency_table)
  )
  
  class(result) <- "biomedical_chisq"
  return(result)
}

# Print method for chi-square test results
print.biomedical_chisq <- function(x, ...) {
  cat("\n", x$method, "\n")
  cat(rep("=", nchar(x$method) + 2), "\n", sep = "")
  
  if (grepl("goodness of fit", x$method)) {
    cat("Categories:", length(x$observed), "\n")
    cat("Sample size:", x$sample_size, "\n")
    cat("Chi-square statistic:", x$statistic, "\n")
    cat("Degrees of freedom:", x$degrees_of_freedom, "\n")
    cat("p-value:", x$p_value, "\n")
    cat("Cramer's V:", x$cramers_v, "\n")
    
  } else if (grepl("independence", x$method)) {
    cat("Contingency table dimensions:", nrow(x$observed), "x", ncol(x$observed), "\n")
    cat("Sample size:", x$sample_size, "\n")
    cat("Chi-square statistic:", x$statistic, "\n")
    cat("Degrees of freedom:", x$degrees_of_freedom, "\n")
    cat("p-value:", x$p_value, "\n")
    cat("Cramer's V:", x$cramers_v, "\n")
    if (!is.na(x$phi_coefficient)) {
      cat("Phi coefficient:", x$phi_coefficient, "\n")
    }
    
  } else if (grepl("McNemar", x$method)) {
    cat("Sample size:", x$sample_size, "\n")
    cat("Discordant pairs:", sum(x$discordant_pairs), "\n")
    cat("Chi-square statistic:", x$statistic, "\n")
    cat("Degrees of freedom:", x$degrees_of_freedom, "\n")
    cat("p-value:", x$p_value, "\n")
    cat("Continuity correction:", x$continuity_correction, "\n")
    if (is.finite(x$odds_ratio)) {
      cat("Odds ratio:", x$odds_ratio, "\n")
    }
  }
  
  # Interpretation
  if (x$p_value < 0.05) {
    cat("\nConclusion: Reject the null hypothesis (significant result)\n")
  } else {
    cat("\nConclusion: Fail to reject the null hypothesis (not significant)\n")
  }
}

# Comprehensive demonstration function
demonstrate_chi_square_tests <- function() {
  cat("=== Chi-Square Tests for Biomedical Data Analysis ===\n\n")
  
  set.seed(123)
  
  # Example 1: Goodness of fit test (Blood type distribution)
  cat("1. GOODNESS OF FIT TEST: Blood Type Distribution\n")
  cat("Research Question: Does the observed blood type distribution match expected population frequencies?\n")
  cat("H0: Observed frequencies = Expected frequencies\n")
  cat("H1: Observed frequencies ≠ Expected frequencies\n\n")
  
  # Blood type data
  observed_blood_types <- c(45, 42, 10, 3)  # A, B, AB, O
  expected_proportions <- c(0.42, 0.37, 0.09, 0.12)  # Population frequencies
  blood_type_labels <- c("Type A", "Type B", "Type AB", "Type O")
  
  cat("Observed blood types in sample (n=100):\n")
  for (i in 1:length(observed_blood_types)) {
    cat(paste(blood_type_labels[i], ":", observed_blood_types[i], "\n"))
  }
  cat("\nExpected population proportions:\n")
  for (i in 1:length(expected_proportions)) {
    cat(paste(blood_type_labels[i], ":", expected_proportions[i], "\n"))
  }
  
  goodness_result <- chi_square_goodness_of_fit(
    observed_blood_types, 
    expected_proportions, 
    blood_type_labels,
    plot_title = "Blood Type Distribution Analysis"
  )
  print(goodness_result)
  
  cat("\n", rep("=", 60), "\n\n")
  
  # Example 2: Test of independence (Treatment response by gender)
  cat("2. INDEPENDENCE TEST: Treatment Response by Gender\n")
  cat("Research Question: Is treatment response independent of patient gender?\n")
  cat("H0: Treatment response is independent of gender\n")
  cat("H1: Treatment response depends on gender\n\n")
  
  # Create contingency table
  treatment_response <- matrix(c(
    25, 15,  # Male: Success, Failure
    30, 10   # Female: Success, Failure
  ), nrow = 2, byrow = TRUE,
  dimnames = list(Gender = c("Male", "Female"), 
                  Response = c("Success", "Failure")))
  
  cat("Contingency Table:\n")
  print(treatment_response)
  cat("\n")
  
  independence_result <- chi_square_independence(
    treatment_response,
    plot_title = "Treatment Response by Gender"
  )
  print(independence_result)
  
  cat("\n", rep("=", 60), "\n\n")
  
  # Example 3: McNemar's test (Before/after treatment)
  cat("3. McNEMAR'S TEST: Before/After Treatment Status\n")
  cat("Research Question: Did treatment significantly change patient status?\n")
  cat("H0: No change in treatment status (marginal frequencies equal)\n")
  cat("H1: Significant change in treatment status\n\n")
  
  # Paired before/after data
  before_after <- matrix(c(
    10, 5,   # Before Positive: After Positive, After Negative
    15, 20   # Before Negative: After Positive, After Negative
  ), nrow = 2, byrow = TRUE,
  dimnames = list(Before = c("Positive", "Negative"),
                  After = c("Positive", "Negative")))
  
  cat("Before/After Contingency Table:\n")
  print(before_after)
  cat("\n")
  
  mcnemar_result <- mcnemar_test(
    before_after,
    plot_title = "Before/After Treatment Analysis"
  )
  print(mcnemar_result)
  
  cat("\n", rep("=", 60), "\n\n")
  
  # Example 4: Large contingency table (Drug side effects by age group)
  cat("4. LARGE CONTINGENCY TABLE: Drug Side Effects by Age Group\n")
  cat("Research Question: Do drug side effects vary by age group?\n\n")
  
  side_effects <- matrix(c(
    12, 8, 15, 5,   # Age 18-30: None, Mild, Moderate, Severe
    18, 12, 20, 10, # Age 31-50: None, Mild, Moderate, Severe
    25, 20, 25, 15, # Age 51-70: None, Mild, Moderate, Severe
    15, 18, 30, 22  # Age 71+: None, Mild, Moderate, Severe
  ), nrow = 4, byrow = TRUE,
  dimnames = list(Age_Group = c("18-30", "31-50", "51-70", "71+"),
                  Side_Effect = c("None", "Mild", "Moderate", "Severe")))
  
  cat("Side Effects by Age Group:\n")
  print(side_effects)
  cat("\n")
  
  large_table_result <- chi_square_independence(
    side_effects,
    plot_title = "Drug Side Effects by Age Group"
  )
  print(large_table_result)
  
  cat("\n", rep("=", 60), "\n\n")
  
  # Example 5: Power analysis and sample size considerations
  cat("5. POWER ANALYSIS FOR CHI-SQUARE TESTS\n")
  cat("Demonstrating the relationship between effect size, sample size, and statistical power\n\n")
  
  # Simulate power analysis for different effect sizes
  sample_sizes <- c(50, 100, 200, 500)
  effect_sizes <- c(0.1, 0.3, 0.5)  # Small, medium, large (Cramer's V)
  
  power_results <- data.frame(
    sample_size = rep(sample_sizes, each = length(effect_sizes)),
    effect_size = rep(effect_sizes, length(sample_sizes)),
    power = NA
  )
  
  for (i in 1:nrow(power_results)) {
    n <- power_results$sample_size[i]
    effect <- power_results$effect_size[i]
    
    # Simulate chi-square tests with known effect size
    power <- mean(replicate(1000, {
      # Create 2x2 table with specified effect size
      p1 <- 0.5
      p2 <- p1 + effect
      if (p2 > 1) p2 <- 1 - effect
      
      x1 <- rbinom(n/2, 1, p1)
      x2 <- rbinom(n/2, 1, p2)
      group <- rep(c("Group1", "Group2"), each = n/2)
      outcome <- c(x1, x2)
      
      test_table <- table(group, outcome)
      if (any(dim(test_table) != 2)) return(FALSE)
      
      result <- chi_square_independence(test_table, plot = FALSE)
      result$p_value < 0.05
    }))
    
    power_results$power[i] <- power
  }
  
  cat("Power Analysis Results (1000 simulations each):\n")
  power_matrix <- matrix(power_results$power, nrow = length(sample_sizes), 
                        dimnames = list(paste("n =", sample_sizes), 
                                       paste("Effect =", effect_sizes)))
  print(round(power_matrix, 3))
  
  cat("\nInterpretations:\n")
  cat("- Cramer's V = 0.1: Small effect size, requires large samples\n")
  cat("- Cramer's V = 0.3: Medium effect size, moderate samples adequate\n")
  cat("- Cramer's V = 0.5: Large effect size, detectable with smaller samples\n")
  cat("- Aim for power ≥ 0.80 (80%) in study design\n\n")
  
  cat("Clinical Guidelines for Chi-Square Tests:\n")
  cat("- Ensure expected frequencies ≥ 5 in at least 80% of cells\n")
  cat("- No expected frequency should be < 1\n")
  cat("- Consider Fisher's exact test for small samples\n")
  cat("- Report effect sizes (Cramer's V, phi coefficient) alongside p-values\n")
  cat("- Consider clinical significance of associations\n")
}

# Run demonstration if script is executed directly
if (sys.nframe() == 0) {
  demonstrate_chi_square_tests()
}