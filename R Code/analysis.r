# Load necessary libraries stats for logistic regression and pROC for ROC curve analysis
library(stats)
library(pROC)

# Please here provide path to your data file
sleep_data <- read.csv("C:/Users/metro/iCloudDrive/FUAS/IDA/Report/R Code/sleep_data.csv") 

# Convert Sleep_Quality to a binary variable
sleep_data$Sleep_Quality <- ifelse(sleep_data$Sleep_Quality == "Good", 1, 0)

# Fit logistic regression model to predict Sleep_Quality based on Screen_Time
model <- glm(Sleep_Quality ~ Screen_Time, data = sleep_data, family = binomial)

# Print model summary
model_summary <- summary(model)
print(model_summary)

# Calculate and print odds ratios
odds_ratios <- exp(coef(model))
print(odds_ratios)

# Calculate and print confidence intervals
conf_intervals <- exp(confint(model))
print(conf_intervals)

# Calculate and print AIC value
aic_value <- AIC(model)
print(paste("AIC:", aic_value))

# Plot and save ROC curve
roc_curve <- roc(sleep_data$Sleep_Quality, fitted(model))
png("chart.png")
plot(1 - roc_curve$specificities, roc_curve$sensitivities, type = "l", main = "ROC Curve for Logistic Regression Model",
     xlab = "1 - Specificity", ylab = "Sensitivity", xlim = c(0, 1), ylim = c(0, 1))
dev.off()

# Calculate and print AUC
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Calculate and print sensitivity and specificity
sensitivity <- roc_curve$sensitivities[which.max(roc_curve$sensitivities + roc_curve$specificities - 1)]
specificity <- roc_curve$specificities[which.max(roc_curve$sensitivities + roc_curve$specificities - 1)]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Plot logistic regression results
png("logistic_regression_plot.png")
plot(sleep_data$Screen_Time, sleep_data$Sleep_Quality, main = "Logistic Regression of Sleep Quality on Screen Time",
     xlab = "Screen Time (hours)", ylab = "Probability of Good Sleep Quality", xlim = c(0, max(sleep_data$Screen_Time)), ylim = c(0, 1))
curve(predict(model, data.frame(Screen_Time = x), type = "response"), add = TRUE)
dev.off()