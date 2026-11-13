# Create dataset
data <- data.frame(
  Score = c(75, 80, 85, 70, 72, 78,
            82, 88, 90, 76, 79, 84,
            68, 72, 75, 65, 70, 73),
  Method = factor(rep(c("A", "B", "C"), each = 6)),
  Gender = factor(rep(c("Male", "Female"), times = 9))
)

# Display dataset
print(data)

# Perform Two-Way ANOVA
model <- aov(Score ~ Method * Gender, data = data)

# Display ANOVA table
summary(model)

# Interaction plot
interaction.plot(data$Method, data$Gender, data$Score,
                 col = c("red", "blue"),
                 lty = 1,
                 type = "b",
                 xlab = "Teaching Method",
                 ylab = "Average Score",
                 trace.label = "Gender")


#using prebuild data in R

# Load built-in dataset
data("ToothGrowth")

# Display dataset
print(ToothGrowth)

# Convert factors
ToothGrowth$supp <- as.factor(ToothGrowth$supp)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# Perform Two-Way ANOVA
model <- aov(len ~ supp * dose, data = ToothGrowth)

# Display ANOVA table
summary(model)

# Interaction plot
interaction.plot(
  ToothGrowth$dose,
  ToothGrowth$supp,
  ToothGrowth$len,
  col = c("red", "blue"),
  type = "b",
  xlab = "Dose",
  ylab = "Tooth Length",
  trace.label = "Supplement"
)