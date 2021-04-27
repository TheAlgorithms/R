# File:    R01_4_1_summary().R
# Course:  R01: R: An introduction
# Chapter: 4: Basic statistics
# Section: 1: summary()
# Author:  Barton Poulson, datalab.cc, @bartonpoulson
# Date:    2016-06-02

# INSTALL AND LOAD PACKAGES ################################

library(datasets)  # Load/unload base packages manually

# LOAD DATA ################################################

head(iris)

# SUMMARY() ################################################

summary(iris$Species)       # Categorical variable
summary(iris$Sepal.Length)  # Quantitative variable
summary(iris)               # Entire data frame

# CLEAN UP #################################################

# Clear packages
detach("package:datasets", unload = TRUE)   # For base

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
