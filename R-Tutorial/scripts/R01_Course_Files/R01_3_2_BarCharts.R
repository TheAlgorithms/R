# File:    R01_3_2_BarCharts.R
# Course:  R01: R: An introduction
# Chapter: 3: Basic graphics
# Section: 2: Bar charts
# Author:  Barton Poulson, datalab.cc, @bartonpoulson
# Date:    2016-05-24

# LOAD DATASETS PACKAGES ###################################

library(datasets)

# LOAD DATA ################################################

?mtcars
head(mtcars)

# BAR CHARTS ###############################################

barplot(mtcars$cyl)             # Doesn't work

# Need a table with frequencies for each category
cylinders <- table(mtcars$cyl)  # Create table
barplot(cylinders)              # Bar chart
plot(cylinders)                 # Default X-Y plot (lines)

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
