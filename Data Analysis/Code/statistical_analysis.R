#--------------------start-------------------------------
# Get current working directory
getwd()

# Install and load necessary packages
#install.packages("PMCMRplus")
#install.packages("coin")
library(PMCMRplus)
library(coin)  # Load the package containing the perm.relation function
library(dplyr) # for data processing

#----------------read dataset--------------------------
data <- read.csv("/data_for_analysis.csv")
summary(data)

# Check for duplicate values ​​in your data
cat("Lipids1 Number of duplicate values:", sum(duplicated(data$lipids1)), "\n")
cat("Lipids2 Number of duplicate values:", sum(duplicated(data$lipids2)), "\n")

# testing for normality of distribution
shapiro.test(data$lipids1)
shapiro.test(data$lipids2)

# Visualize distribution
par(mfrow=c(1,2))
hist(data$lipids1, main = "Histogram of Lipids1")  
qqnorm(data$lipids1)
qqline(data$lipids1)

# Spearman's correlation test
spearman_result <- cor.test(data$lipids1, data$lipids2, method="spearman", exact = FALSE)
print(spearman_result)

# data.frame for result
results <- data.frame(
  variable = character(),
  spearman_corr = numeric(),
  s_p_value = numeric(),
  stringsAsFactors = FALSE
)

# variables for analysis
target_vars <- c("lipids2", "lipids3", "lipids4")

# main 
for (var in target_vars) {
  # Use spearman_test of coin package to perform permutation test
  perm_spearman <- spearman_test(
    data$lipids1 ~ data[[var]],
    distribution = approximate(B = 10000)  # Perform 10,000 permutations
  )
  
  # add result
  results <- rbind(results, data.frame(
    variable = var,
    spearman_corr = cor(data$lipids1, data[[var]], method = "spearman"),
    s_p_value = pvalue(perm_spearman)
  ))
}

# output result
print(results)

#------visualization of significant results of correlation analysis---------
data <- data[order(data$lipids1),]

# Draw a scatter plot and add a regression line
plot(data$lipids1, data$lipids2, 
     main = "Relationship between Lipids1 and Lipids2",
     xlab = "Lipids1",
     ylab = "Lipids2",
     pch = 16,
     col = rgb(0,0,1,0.5))

# Add smooth curve
lines(data$lipids1, predict(lm(lipids2 ~ lipids1, data = data)), col = "red", lwd = 2)

# Add low ess smooth curve
lines(data$lipids1, loess(lipids2 ~ lipids1, data = data)$fitted, col = "blue", lwd = 2, lty = 2)


#_____________regression analysis________________ 
df <- data
df <- df[order(df$lipids1),]

#linear regression
model_linear <- lm(lipids1 ~ lipids2, data = df)
summary(model_linear)

#second degree polynomial
model_2 <- lm(lipids1 ~ poly(lipids2, 2, raw = TRUE), data = df)
summary(model_2)

#third degree polynomial
model_3 <- lm(lipids1 ~ poly(lipids2, 3, raw = TRUE), data = df)
summary(model_3)

#exponential dependence
model_exp <- lm(lipids1 ~ exp(lipids2), data = df)
summary(model_exp)

# log dependence
model_log <- lm(lipids1 ~ log(lipids2), data = df)
summary(model_log)

#comparison of models
#table of result
rezult <- data.frame(
  model = c("model_linear", "model_2", "model_3", "model_exp", "model_log"),
  BIC_value = c(BIC(model_linear), BIC(model_2), BIC(model_3), BIC(model_exp), BIC(model_log)),
  R_squared = c(summary(model_linear)$r.squared, 
                summary(model_2)$r.squared, 
                summary(model_3)$r.squared, 
                summary(model_exp)$r.squared, 
                summary(model_log)$r.squared)
)

rezult <- rezult[order(rezult$BIC_value),]
print(rezult)

# __________building graphs______________
# Plot prediction curves for all models
par(mfrow=c(2,3))

# Scatter plot of raw data
plot(df$lipids2, df$lipids1, 
     main = "Original Data", 
     pch = 16, 
     col = rgb(0,0,1,0.5))

# Linear Model
plot(df$lipids2, df$lipids1, 
     main = "Linear Model", 
     pch = 16, 
     col = rgb(0,0,1,0.5))
lines(df$lipids2, predict(model_linear), col = "red", lwd = 2)

# subpolynomial model
plot(df$lipids2, df$lipids1, 
     main = "二Quadratic Polynomial", 
     pch = 16, 
     col = rgb(0,0,1,0.5))
lines(df$lipids2, predict(model_2), col = "blue", lwd = 2)

# subpolynomial model
plot(df$lipids2, df$lipids1, 
     main = "三Cubic Polynomial", 
     pch = 16, 
     col = rgb(0,0,1,0.5))
lines(df$lipids2, predict(model_3), col = "green", lwd = 2)

# exponential model
plot(df$lipids2, df$lipids1, 
     main = "Exponential Model", 
     pch = 16, 
     col = rgb(0,0,1,0.5))
lines(df$lipids2, predict(model_exp), col = "purple", lwd = 2)

# Logarithmic Model
plot(df$lipids2, df$lipids1, 
     main = "Logarithmic Model", 
     pch = 16, 
     col = rgb(0,0,1,0.5))
lines(df$lipids2, predict(model_log), col = "orange", lwd = 2)

