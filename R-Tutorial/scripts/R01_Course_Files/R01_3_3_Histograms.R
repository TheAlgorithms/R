# File:    R01_3_3_Histograms.R
# Course:  R01: R: An introduction
# Chapter: 3: Basic graphics
# Section: 3: Histograms
# Author:  Barton Poulson, datalab.cc, @bartonpoulson
# Date:    2016-05-24

# LOAD PACKAGES ############################################

library(datasets)

# LOAD DATA ################################################

?iris
head(iris)

# BASIC HISTOGRAMS #########################################

hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)

# HISTOGRAM BY GROUP #######################################

# Put graphs in 3 rows and 1 column
par(mfrow = c(3, 1))

# Histograms for each species using options
hist(iris$Petal.Width [iris$Species == "setosa"],
  xlim = c(0, 3),
  breaks = 9,
  main = "Petal Width for Setosa",
  xlab = "",
  col = "red")

hist(iris$Petal.Width [iris$Species == "versicolor"],
  xlim = c(0, 3),
  breaks = 9,
  main = "Petal Width for Versicolor",
  xlab = "",
  col = "purple")

hist(iris$Petal.Width [iris$Species == "virginica"],
  xlim = c(0, 3),
  breaks = 9,
  main = "Petal Width for Virginica",
  xlab = "",
  col = "blue")

# Restore graphic parameter
par(mfrow=c(1, 1))

# CLEAN UP #################################################

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
