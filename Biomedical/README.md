# R for Biomedical Data Analysis

This directory contains R implementations of essential statistical tests commonly used in biomedical research and clinical studies. These non-parametric tests are fundamental tools for biomedical students and researchers working with medical data.

## 📋 Contents

- [`wilcoxon_signed_rank_test.r`](wilcoxon_signed_rank_test.r) - Wilcoxon Signed-Rank Test for paired samples
- [`mann_whitney_u_test.r`](mann_whitney_u_test.r) - Mann-Whitney U Test for independent samples
- [`README.md`](README.md) - This documentation file

## 🏥 Why These Tests Are Essential for Biomedical Students

### 1. **Real-World Medical Data Challenges**
Medical data rarely follows normal distributions due to:
- **Skewed distributions**: Lab values, reaction times, survival data
- **Outliers**: Extreme measurements that are medically significant
- **Small sample sizes**: Pilot studies, rare diseases, expensive procedures
- **Ordinal scales**: Pain scores, quality of life indices, severity ratings

### 2. **Robust and Assumption-Free**
Non-parametric tests like Wilcoxon and Mann-Whitney:
- **No normality assumption**: Work with any distribution shape
- **Outlier resistant**: Not affected by extreme values
- **Rank-based**: Focus on relative ordering rather than exact values
- **Clinically meaningful**: Often more relevant for medical decisions

### 3. **Common Applications in Medicine**
- **Clinical trials**: Comparing treatment effectiveness
- **Before/after studies**: Evaluating intervention outcomes
- **Biomarker research**: Comparing levels between groups
- **Quality of life studies**: Analyzing patient-reported outcomes
- **Diagnostic accuracy**: Comparing test performance

## 📊 Statistical Tests Overview

### Wilcoxon Signed-Rank Test
**Purpose**: Compare paired samples or test single sample against a median

**When to use**:
- Before/after treatment comparisons
- Matched pairs (e.g., twins, matched controls)
- Repeated measurements on same subjects
- Testing if median differs from a specific value

**Medical Examples**:
- Blood pressure before vs after medication
- Pain scores before vs after treatment
- Weight loss in diet studies
- Biomarker changes over time

**Assumptions**:
- Paired observations or single sample
- Data can be ranked
- Differences are symmetrically distributed around the median

### Mann-Whitney U Test (Wilcoxon Rank-Sum Test)
**Purpose**: Compare two independent groups

**When to use**:
- Comparing treatment vs control groups
- Comparing different populations
- When groups are independent (not paired)
- Alternative to two-sample t-test

**Medical Examples**:
- Drug efficacy: treatment vs placebo
- Gender differences in biomarkers
- Disease severity between stages
- Age-related immune responses
- Comparing diagnostic methods

**Assumptions**:
- Two independent samples
- Data can be ranked
- Similar distribution shapes (for location comparison)

## 🚀 Quick Start Guide

### Installation
No additional packages required! Just R base installation.

```r
# Clone or download the files to your R working directory
# Source the functions
source("wilcoxon_signed_rank_test.r")
source("mann_whitney_u_test.r")
```

### Basic Usage

#### Wilcoxon Signed-Rank Test
```r
# Example: Blood pressure before and after treatment
before <- c(145, 150, 138, 155, 142, 148, 152, 140)
after <- c(138, 142, 135, 148, 136, 140, 145, 133)

# Perform the test
result <- wilcoxon_signed_rank_test(before, after, alternative = "greater")
print(result)
```

#### Mann-Whitney U Test
```r
# Example: Comparing treatment vs control groups
treatment <- c(78, 85, 92, 73, 88, 91, 76, 83)
control <- c(65, 58, 71, 62, 69, 54, 67, 60)

# Perform the test
result <- mann_whitney_u_test(treatment, control, alternative = "greater")
print(result)
```

## 🔬 Detailed Examples with Biomedical Data

### Running the Examples
Each R file contains comprehensive examples with dummy biomedical data:

```r
# Run Wilcoxon examples
source("wilcoxon_signed_rank_test.r")
run_biomedical_examples()

# Run Mann-Whitney examples
source("mann_whitney_u_test.r")
run_biomedical_examples()
```

## 📈 Understanding the Results

### Key Output Elements

#### For Wilcoxon Signed-Rank Test:
- **W+**: Sum of ranks for positive differences
- **W-**: Sum of ranks for negative differences  
- **Test statistic W**: Usually min(W+, W-)
- **P-value**: Probability of observing the result by chance
- **Effect size**: Magnitude of the difference

#### For Mann-Whitney U Test:
- **U1, U2**: U statistics for each group
- **W1, W2**: Sum of ranks for each group
- **Test statistic U**: Depends on alternative hypothesis
- **P-value**: Probability of observing the result by chance
- **Effect size estimate**: Magnitude of group difference

### Interpreting P-values in Medical Context
- **p < 0.001**: Highly significant - very strong evidence
- **p < 0.01**: Very significant - strong evidence  
- **p < 0.05**: Significant - moderate evidence
- **p ≥ 0.05**: Not significant - insufficient evidence

**Important**: Always consider clinical significance alongside statistical significance!

## 🎯 Choosing the Right Test

| Scenario | Test | Example |
|----------|------|---------|
| Same subjects, before/after | Wilcoxon Signed-Rank | Pre/post treatment blood pressure |
| Paired subjects | Wilcoxon Signed-Rank | Twins, matched controls |
| Single sample vs reference | Wilcoxon Signed-Rank | Patient cholesterol vs normal (200) |
| Two independent groups | Mann-Whitney U | Treatment vs control groups |
| Gender/age comparisons | Mann-Whitney U | Male vs female biomarker levels |
| Disease stage comparison | Mann-Whitney U | Stage I vs Stage II severity |

## 🧮 Statistical Theory (Simplified)

### Wilcoxon Signed-Rank Test
1. **Calculate differences** between paired observations
2. **Rank the absolute differences** (ignore zeros)
3. **Sum ranks** for positive and negative differences separately
4. **Test statistic** is typically the smaller sum
5. **Compare** to expected distribution under null hypothesis

### Mann-Whitney U Test
1. **Combine all observations** from both groups
2. **Rank all values** from smallest to largest
3. **Sum ranks** for each group separately
4. **Calculate U statistics** using rank sums
5. **Compare** to expected distribution under null hypothesis

## 🔧 Advanced Features

### Alternative Hypotheses
- **"two.sided"**: Groups/conditions are different (default)
- **"greater"**: First group/condition is greater than second
- **"less"**: First group/condition is less than second

### Handling Ties
Both implementations use average ranks for tied values, which is the standard approach.

### Effect Size
- **Wilcoxon**: Based on Z-score and sample size
- **Mann-Whitney**: Based on U statistic relative to maximum possible

## 📚 Further Reading

### Recommended Resources for Biomedical Students
1. **"Biostatistics: A Foundation for Analysis in the Health Sciences"** - Wayne Daniel
2. **"Statistical Methods in Medical Research"** - Armitage, Berry & Matthews  
3. **"Nonparametric Statistical Methods"** - Hollander, Wolfe & Chicken
4. **"Medical Statistics from Scratch"** - David Bowers

### Online Resources
- [Laerd Statistics](https://statistics.laerd.com/) - Excellent step-by-step guides
- [StatsDirect](https://www.statsdirect.com/) - Comprehensive statistical reference
- [BMJ Statistics Notes](https://www.bmj.com/about-bmj/resources-readers/publications/statistics-square-one) - Medical statistics primer

## ⚠️ Important Considerations

### When NOT to Use These Tests
- **Large samples with normal data**: t-tests might be more powerful
- **Survival data**: Use specialized survival analysis methods
- **Repeated measures**: Consider mixed-effects models
- **Multiple comparisons**: Adjust p-values appropriately

### Common Pitfalls
1. **Multiple testing**: Correct for multiple comparisons
2. **Effect size**: Don't ignore practical significance
3. **Sample size**: Very small samples need exact methods
4. **Assumptions**: Ensure data can be meaningfully ranked

### Data Quality Checks
- Check for outliers and data entry errors
- Verify assumptions are met
- Consider the clinical context
- Validate results with domain experts
