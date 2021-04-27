# File:    R01_2_2_RStudio.R
# Course:  R01: R: An introduction
# Chapter: 2: Setting up
# Section: 2: RStudio
# Author:  Barton Poulson, datalab.cc, @bartonpoulson
# Date:    2016-04-04

# LOAD DATA ################################################

library(datasets)  # Load built-in datasets

# SUMMARIZE DATA ###########################################

head(iris)         # Show the first six lines of iris data
summary(iris)      # Summary statistics for iris data
plot(iris)         # Scatterplot matrix for iris data

# CLEAN UP #################################################

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
