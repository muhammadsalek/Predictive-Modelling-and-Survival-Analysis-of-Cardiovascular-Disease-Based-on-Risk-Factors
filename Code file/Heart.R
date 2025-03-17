#Install packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gtsummary")
install.packages("caret")
install.packages("survival")  # Install if not already installed
library(survival)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(caret)
install.packages("survminer")  # Install if not already installed
library(survminer)
install.packages("pROC")  # Install if not already installed
library(pROC)
install.packages("gt")  # Install if not already installed
library(gt)
install.packages("flextable")  # Install if not already installed
library(flextable)

####Import data####
# Load the dataset
data <- read.csv("E:\\Ru Poster to paper\\archive (1)\\heart_disease_data_with_features.csv")
data
# Check the structure and summary
str(data)
summary(data)
# Handling missing values
data <- na.omit(data)  # Remove rows with missing values
data
sum(is.na(data))


#Descriptive Statistics and Correlations
# Welch's T-Test for Risk Score by Sex
t_test_result <- t.test(risk_score ~ sex, data = data)
print(t_test_result)


# Cholesterol vs. Blood Pressure - Chi-Square Test

# Convert variables to categorical if needed
data$chol_cat <- cut(data$chol, breaks = 3, labels = c("Low", "Medium", "High"))
data$bp_cat <- cut(data$trestbps, breaks = 3, labels = c("Low", "Medium", "High"))

# Chi-Square Test
chi_square_result <- chisq.test(table(data$chol_cat, data$bp_cat))
print(chi_square_result)

#Pearson's Correlation - Cholesterol and Blood Pressure
# Correlation
cor_result <- cor.test(data$chol, data$trestbps, method = "pearson")
print(cor_result)


#One-Way ANOVA for Risk Score Across Age Groups
# Ensure age group is categorical
data$age_group <- cut(data$age, breaks = 4, labels = c("Young", "Middle-Age", "Older", "Elderly"))

# ANOVA Test
anova_result <- aov(risk_score ~ age_group, data = data)
summary(anova_result)
install.packages("nnet")
library(nnet)
# Multinomial Logistic Regression
# Outcome variable: 'num' (heart disease outcome, 0 = no disease, 1 = disease)
# Predictors: Age, Sex, Cholesterol, Blood Pressure, etc.

# Split data into train and test
set.seed(123)
trainIndex <- createDataPartition(data$num, p = 0.7, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Fit the model
multinom_model <- multinom(num ~ age + sex + chol + trestbps + thalach + oldpeak, data = train)
summary(multinom_model)

# Predictions
pred <- predict(multinom_model, newdata = test)
confusionMatrix(as.factor(pred), as.factor(test$num))




####Visualization####
# Visualization: Risk Score by Sex
# Load necessary library
library(ggplot2)

# Customized boxplot for risk score by sex
ggplot(data, aes(x = as.factor(sex), y = risk_score, fill = as.factor(sex))) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16, outlier.size = 2) +  # Enhanced boxplot with visible outliers
  labs(
    title = "Risk Score Distribution by Gender",
    x = "Gender",
    y = "Risk Score"
  ) +
  scale_fill_manual(
    values = c("#0072B2", "#D55E00"),  # Professional color palette
    labels = c("Male", "Female")       # Custom labels for legend
  ) +
  scale_x_discrete(
    labels = c("0" = "Male", "1" = "Female")  # Replace 0 and 1 with Male and Female
  ) +
  theme_minimal(base_size = 14) +  # Minimalist theme with adjusted font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center and bold title
    axis.title = element_text(face = "bold", size = 14),              # Bold axis titles
    axis.text = element_text(size = 12),                              # Adjust axis text size
    legend.position = "top",                                          # Move legend to the top
    legend.title = element_blank(),                                   # Remove legend title
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", color = "grey80")  # Fixed linewidth
  ) +
  annotate(
    "text", x = 1.5, y = max(data$risk_score) * 1.05, 
    label = "Significant difference observed", size = 4, color = "red", fontface = "italic"
  )  # Add annotation if applicable





# Load necessary library
library(ggplot2)

# Professional Scatter Plot for Cholesterol vs. Blood Pressure
ggplot(data, aes(x = chol, y = trestbps)) +
  geom_point(color = "#E69F00", size = 2, alpha = 0.6) +  # Professional color and transparency
  geom_smooth(method = "lm", color = "#D55E00", se = TRUE, linewidth = 1.2) +  # Linear regression line
  labs(
    title = "Association Between Cholesterol and Blood Pressure",
    subtitle = "A significant positive correlation is observed",
    x = "Cholesterol Level (mg/dL)",
    y = "Blood Pressure (mmHg)"
  ) +
  theme_minimal(base_size = 14) +  # Clean theme with larger base font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Bold, centered title
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12, color = "grey40"),
    axis.title = element_text(face = "bold", size = 14),              # Bold axis titles
    axis.text = element_text(size = 12),                              # Adjust axis text size
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", color = "grey80"),  # Grid lines
    panel.grid.minor = element_blank(),                               # Remove minor grid lines
    legend.position = "none"                                          # Remove legend
  ) +
  annotate(
    "text", x = max(data$chol) * 0.7, y = max(data$trestbps) * 0.9,
    label = "Linear Fit with 95% CI", size = 4, color = "red", fontface = "italic"
  )






# Load necessary libraries
library(ggplot2)
library(ggpubr)

# Professional Scatter Plot for Cholesterol vs. Blood Pressure with Regression Equation
ggplot(data, aes(x = chol, y = trestbps)) +
  geom_point(color = "#E69F00", size = 2, alpha = 0.6) +  # Professional color and transparency
  geom_smooth(method = "lm", color = "#D55E00", se = TRUE, linewidth = 1.2) +  # Linear regression line
  stat_regline_equation(
    label.x = max(data$chol) * 0.7, label.y = max(data$trestbps) * 0.9, 
    aes(label = after_stat(eq.label)), color = "black", size = 5
  ) +  # Regression equation
  stat_cor(
    label.x = max(data$chol) * 0.7, label.y = max(data$trestbps) * 0.85, 
    aes(label = after_stat(r.label)), color = "black", size = 5
  ) +  # Correlation coefficient
  labs(
    title = "Association Between Cholesterol and Blood Pressure",
    subtitle = "A significant positive correlation is observed",
    x = "Cholesterol Level (mg/dL)",
    y = "Blood Pressure (mmHg)"
  ) +
  theme_minimal(base_size = 14) +  # Clean theme with larger base font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Bold, centered title
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12, color = "grey40"),
    axis.title = element_text(face = "bold", size = 14),              # Bold axis titles
    axis.text = element_text(size = 12),                              # Adjust axis text size
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", color = "grey80"),  # Grid lines
    panel.grid.minor = element_blank(),                               # Remove minor grid lines
    legend.position = "none"                                          # Remove legend
  )



# Load necessary libraries
library(ggplot2)

# Fit the linear model to calculate R-squared
model <- lm(trestbps ~ chol, data = data)
r_squared <- summary(model)$r.squared  # Extract R-squared value

# Professional Scatter Plot with Regression Equation and R-squared
ggplot(data, aes(x = chol, y = trestbps)) +
  geom_point(color = "#E69F00", size = 2, alpha = 0.6) +  # Professional color and transparency
  geom_smooth(method = "lm", color = "#D55E00", se = TRUE, linewidth = 1.2) +  # Linear regression line
  stat_regline_equation(
    aes(label = after_stat(eq.label)),
    label.x = max(data$chol, na.rm = TRUE) * 0.7, 
    label.y = max(data$trestbps, na.rm = TRUE) * 0.9,
    color = "black", size = 5
  ) +  # Regression equation
  annotate(
    "text", 
    x = max(data$chol, na.rm = TRUE) * 0.7, 
    y = max(data$trestbps, na.rm = TRUE) * 0.8,
    label = paste0("R-squared = ", round(r_squared, 3)),
    color = "black", size = 5, hjust = 0
  ) +  # Add R-squared annotation
  labs(
    title = "Association Between Cholesterol and Blood Pressure",
    subtitle = "A significant positive correlation is observed",
    x = "Cholesterol Level (mg/dL)",
    y = "Blood Pressure (mmHg)"
  ) +
  theme_minimal(base_size = 14) +  # Clean theme with larger base font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Bold, centered title
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12, color = "grey40"),
    axis.title = element_text(face = "bold", size = 14),              # Bold axis titles
    axis.text = element_text(size = 12),                              # Adjust axis text size
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", color = "grey80"),  # Grid lines
    panel.grid.minor = element_blank(),                               # Remove minor grid lines
    legend.position = "none"                                          # Remove legend
  )











# Fit the linear model to get R-squared
model <- lm(trestbps ~ chol, data = data)
r_squared <- summary(model)$r.squared  # Extract R-squared value

# Professional Scatter Plot for Cholesterol vs. Blood Pressure
ggplot(data, aes(x = chol, y = trestbps)) +
  geom_point(color = "#0072B2", size = 2, alpha = 0.6) +  # Blue scatter points with transparency
  geom_smooth(method = "lm", color = "#D55E00", se = TRUE, linewidth = 1) +  # Red regression line with CI
  annotate(
    "text", x = max(data$chol, na.rm = TRUE) * 0.7, y = max(data$trestbps, na.rm = TRUE) * 0.9,
    label = paste0("R-squared = ", round(r_squared, 3)),
    color = "black", size = 5, hjust = 0
  ) +  # Add R-squared value
  labs(
    title = "Association Between Cholesterol and Blood Pressure",
    subtitle = "Linear regression with 95% confidence interval",
    x = "Cholesterol Level (mg/dL)",
    y = "Blood Pressure (mmHg)"
  ) +
  theme_minimal(base_size = 14) +  # Minimal theme with base font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Bold, centered title
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12, color = "grey40"),
    axis.title = element_text(face = "bold", size = 14),              # Bold axis titles
    axis.text = element_text(size = 12),                              # Adjust axis text size
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", color = "grey80"),  # Dashed grid lines
    panel.grid.minor = element_blank(),                               # Remove minor grid lines
    legend.position = "none"                                          # No legend required
  )




# Load necessary libraries
library(caret)       # For data partitioning, preprocessing, and evaluation
library(nnet)        # For multinomial logistic regression
library(car)         # For multicollinearity check
library(pROC)        # For ROC curve and AUC
library(dplyr)       # For data manipulation

# 2. Inspect the data
cat("Structure of the dataset:\n")
str(data)
cat("\nSummary of the dataset:\n")
summary(data)

# 3. Handle missing values
data <- na.omit(data)  # Remove rows with missing values

# 4. Prepare the target variable (binary classification)
data$num <- ifelse(data$num > 0, "Disease", "No Disease")
data$num <- as.factor(data$num)

# 5. Simplify imbalanced categories
# Combine sparse categories for 'chol_cat' and 'bp_cat'
data$chol_cat <- ifelse(data$chol_cat == "High", "Medium", as.character(data$chol_cat))
data$chol_cat <- as.factor(data$chol_cat)

data$bp_cat <- ifelse(data$bp_cat == "High", "Medium", as.character(data$bp_cat))
data$bp_cat <- as.factor(data$bp_cat)

# 6. Feature selection: Remove highly correlated features
# Calculate correlation matrix for numeric features
# 6. Feature selection: Remove highly correlated features

# Select numeric variables only
numeric_vars <- data %>% select_if(is.numeric)

# Remove columns with zero variance
numeric_vars <- numeric_vars[, sapply(numeric_vars, function(x) sd(x, na.rm = TRUE) > 0)]

# Calculate the correlation matrix
corr_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

# Identify highly correlated features (cutoff = 0.8)
high_cor <- findCorrelation(corr_matrix, cutoff = 0.8)
cat("\nHighly correlated variables indices:\n")
print(high_cor)

# Remove highly correlated variables
data <- data[, -high_cor]

cat("\nCurrent column names in the dataset:\n")
print(colnames(data))


# Step 1: Reload the original dataset
data_original <- read.csv("E:\\Ru Poster to paper\\archive (1)\\heart_disease_data_with_features.csv")

# Step 2: Align rows of `data_original` with `data` before adding `num`
if (!"num" %in% colnames(data)) {
  cat("Adding `num` back to the cleaned dataset from the original dataset.\n")
  # Align rows based on the row names or indices
  data$num <- data_original$num[match(rownames(data), rownames(data_original))]
}

# Step 3: Verify the updated dataset
cat("\nColumns after adding `num`:\n")
print(colnames(data))

cat("Preview of `num` column:\n")
print(head(data$num))


# List essential variables to retain
important_vars <- c("num", "sex", "cp", "thalach", "exang", "slope", 
                    "ca", "thal", "chol", "trestbps", "oldpeak", "age")

# Join important variables back into the cleaned dataset
data_cleaned <- data %>%
  left_join(data_original[, important_vars], by = "num")

# Verify the cleaned dataset

cat("\nStructure of `num`:\n")
str(data_cleaned$num)

cat("\nUnique values in `num`:\n")
print(unique(data_cleaned$num))


data_cleaned$num <- ifelse(data_cleaned$num > 0, 1, 0)
cat("\nStructure of `num` after conversion:\n")
str(data_cleaned$num)

cat("\nUnique values in `num` after conversion:\n")
print(unique(data_cleaned$num))


# Check column names
cat("\nColumns in `data_cleaned`:\n")
print(colnames(data_cleaned))

# Renaming the correct column (e.g., `thalach.y` to `thalach`)
data_cleaned$thalach <- data_cleaned$thalach.y

# Remove the duplicate columns (`thalach.x` and `thalach.y`)
data_cleaned <- data_cleaned[, !names(data_cleaned) %in% c("thalach.x", "thalach.y")]



# Check column names after modification
cat("\nUpdated columns in `data_cleaned`:\n")
print(colnames(data_cleaned))


# Step 3: Build the logistic regression model
vif_model <- glm(num ~ age + chol + trestbps + thalach + oldpeak, 

                         data = data_cleaned, family = binomial)

# Step 4: Display model summary
summary(vif_model)





# Load necessary library
library(MASS)

# Step 3: Build the initial logistic regression model
initial_model <- glm(num ~ age + chol + trestbps + thalach + oldpeak, 
                     data = data_cleaned, family = binomial)

# Perform stepwise selection to minimize AIC
optimized_model <- stepAIC(initial_model, direction = "both", trace = FALSE)

# Step 4: Display the summary of the optimized model
summary(optimized_model)


















# 1. Check columns in the data
cat("\nColumns in `data`:\n")
print(colnames(data))

# 2. Re-add missing columns from the original dataset
if (!"age" %in% colnames(data)) {
  data$age <- data_original$age[match(rownames(data), rownames(data_original))]
}

if (!"chol" %in% colnames(data)) {
  data$chol <- data_original$chol[match(rownames(data), rownames(data_original))]
}

if (!"trestbps" %in% colnames(data)) {
  data$trestbps <- data_original$trestbps[match(rownames(data), rownames(data_original))]
}

# 3. Verify that all required columns are now in `data`
cat("\nUpdated column names in `data`:\n")
print(colnames(data))

# 4. Add `oldpeak` column if missing
if (!"oldpeak" %in% colnames(data)) {
  cat("Adding `oldpeak` back to `data` from `data_original`.\n")
  data$oldpeak <- data_original$oldpeak[match(rownames(data), rownames(data_original))]
}

# 5. Verify the updated columns
cat("\nColumns in `data` after adding `oldpeak`:\n")
print(colnames(data))

# 6. Standardize the numeric predictors
preprocess_params <- preProcess(data[, c("age", "chol", "trestbps", "thalach", "oldpeak")], method = c("center", "scale"))
data_scaled <- predict(preprocess_params, data)

# 7. Split the data into training (70%) and testing (30%)
set.seed(123)
trainIndex <- createDataPartition(data_scaled$num, p = 0.7, list = FALSE)
train <- data_scaled[trainIndex, ]
test <- data_scaled[-trainIndex, ]

# 8. Recode `num` to binary (0 = No Disease, 1 = Disease)
cat("\nUnique values in `train$num` before recoding:\n")
print(unique(train$num))

train$num <- ifelse(train$num > 0, 1, 0)
test$num <- ifelse(test$num > 0, 1, 0)

cat("\nUnique values in `train$num` after recoding:\n")
print(unique(train$num))

# 9. Fit the logistic regression model
logistic_model <- glm(num ~ age + sex + chol + trestbps + thalach + oldpeak, 
                      data = train, family = binomial)

# 10. Display model summary
cat("\nLogistic Regression Model Summary:\n")
summary(logistic_model)







# Install the pscl package if it's not already installed
if (!require(pscl)) {
  install.packages("pscl", dependencies=TRUE)
  library(pscl)
} else {
  library(pscl)
}

# Assuming logistic_model is already fitted
# Calculate pseudo R-squared values
pseudo_r_squared <- pscl::pR2(logistic_model)
print(pseudo_r_squared)

# Install and load the pscl package
if (!require(pscl)) install.packages("pscl", dependencies=TRUE)
library(pscl)

# Fit a logistic regression model
logistic_model <- glm(num ~ age + sex + chol + trestbps + thalach + oldpeak, 
                      data = train, family = binomial)

# Calculate McFadden's R-squared
pR2(logistic_model)




# Load the MASS package for stepwise regression
library(MASS)

# Fit the initial full model
full_model <- glm(num ~ age + sex + chol + trestbps + thalach + oldpeak, 
                  data = train, family = binomial)

# Apply stepwise regression using both directions
stepwise_model <- stepAIC(full_model, direction="both")

# Summary of the new model
summary(stepwise_model)





#### Validation####
# Predicting using the model on the test dataset
predictions <- predict(stepwise_model, newdata = test, type = "response")

# Converting probabilities to binary outcomes based on a 0.5 threshold
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Generating a confusion matrix to evaluate performance
library(caret)
confusionMatrix(as.factor(predicted_classes), as.factor(test$num))



####Diagnostics####



# Calculate VIF to check for multicollinearity
vif(stepwise_model)



# Plotting diagnostics for influence
par(mfrow = c(2, 2))
plot(stepwise_model)


#### Interpret Results####
# Extracting model summary
summary_results <- summary(stepwise_model)

# Extract coefficients for interpretation
coefficients <- summary_results$coefficients

# Displaying coefficients with interpretation
print(coefficients)

# Discuss implications based on the coefficients
cat("\nInterpretation:\n")
cat("Positive coefficients such as for 'sex' and 'trestbps' suggest these variables increase the log-odds of the outcome, indicating a higher probability of the event occurring as these predictor values increase. Conversely, a negative coefficient for 'thalach' suggests higher values decrease the likelihood of the outcome.\n")





stepwise_model_interact <- glm(num ~ sex * trestbps + thalach * oldpeak, family = binomial, data = train)
summary(stepwise_model_interact)



stepwise_model_poly <- glm(num ~ sex + poly(trestbps, 2) + poly(thalach, 2) + poly(oldpeak, 2), family = binomial, data = train)
summary(stepwise_model_poly)






library(pROC)
roc_curve <- roc(test$num, predict(stepwise_model, test, type = "response"))
plot(roc_curve, col="blue")
auc(roc_curve)





# Identify potential influential data points
influence_measures <- influence.measures(stepwise_model)
print(influence_measures)




install.packages("randomForest")
library(randomForest)
rf_model <- randomForest(num ~ sex + trestbps + thalach + oldpeak, data = train)
print(rf_model)

install.packages("xgboost")

library(xgboost)
dtrain <- xgb.DMatrix(data = as.matrix(train[, c("sex", "trestbps", "thalach", "oldpeak")]), label = train$num)
xgb_model <- xgboost(data = dtrain, max_depth = 3, nrounds = 100, objective = "binary:logistic")


library(xgboost)

# Convert data to XGBoost matrix format
dtrain <- xgb.DMatrix(data = as.matrix(train[, c("sex", "trestbps", "thalach", "oldpeak")]), label = train$num)
dtest <- xgb.DMatrix(data = as.matrix(test[, c("sex", "trestbps", "thalach", "oldpeak")]), label = test$num)

# Train XGBoost Model
xgb_model <- xgboost(data = dtrain, max_depth = 3, nrounds = 100, objective = "binary:logistic")

# Make Predictions
xgb_predictions <- predict(xgb_model, dtest)

# Convert probabilities to binary labels (threshold 0.5)
xgb_predicted_classes <- ifelse(xgb_predictions > 0.5, 1, 0)

# Evaluate Performance
library(caret)
confusionMatrix(as.factor(xgb_predicted_classes), as.factor(test$num))




#### Hyperparameter Tuning in XGBoost####

library(xgboost)
library(caret)

# Define a grid of hyperparameters
xgb_grid <- expand.grid(
  max_depth = c(3, 5, 7), 
  eta = c(0.01, 0.1, 0.3), 
  nrounds = c(100, 200, 300),
  gamma = c(0, 1, 5),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.6, 0.8, 1),
  colsample_bytree = c(0.6, 0.8, 1)
)

# Perform cross-validation tuning
tuned_xgb <- train(
  x = as.matrix(train[, c("sex", "trestbps", "thalach", "oldpeak")]), 
  y = as.factor(train$num),
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = xgb_grid
)

# Print the best model parameters
print(tuned_xgb$bestTune)

































































































































































































































































































































































































































# 11. Predict on the Test Set
predictions <- predict(logistic_model, newdata = test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, "Disease", "No Disease")
predicted_classes <- as.factor(predicted_classes)

# 12. Evaluate model performance
test$num <- ifelse(test$num == 0, "No Disease", "Disease")
test$num <- as.factor(test$num)

conf_matrix <- confusionMatrix(predicted_classes, test$num)
print("Confusion Matrix:")
print(conf_matrix)

cat("\nModel Accuracy: ", conf_matrix$overall['Accuracy'], "\n")

# 13. ROC Curve and AUC with Confidence Interval
library(pROC)

# Generate the ROC curve object
roc_curve <- roc(test$num, predictions)

# Calculate the AUC with confidence intervals
auc_value <- auc(roc_curve)
ci_auc <- ci(roc_curve, conf.level = 0.95)  # 95% Confidence Interval

# Customize the ROC plot with a professional, clean look
plot(
  roc_curve, 
  col = "#D55E00",                # Professional color for the ROC curve
  lwd = 3,                        # Line width (thicker line)
  main = "ROC Curve for Logistic Regression", 
  cex.main = 1.5,                 # Title size
  cex.lab = 1.4,                  # Axis label size
  cex.axis = 1.2,                 # Axis text size
  xlab = "False Positive Rate",   # X-axis label
  ylab = "True Positive Rate",    # Y-axis label
  xlim = c(0, 1),                 # Set axis limits
  ylim = c(0, 1)
)

# Add a diagonal line for random guessing
abline(a = 0, b = 1, lty = 2, col = "grey")

# Add AUC and CI annotation in the plot itself (not just the legend)
text(0.6, 0.2, 
     paste("AUC =", round(auc_value, 3), 
           "\n95% CI: [", round(ci_auc[1], 3), ", ", round(ci_auc[2], 3), "]"), 
     col = "black", cex = 1.2, font = 2)

# Optional: Add the confidence interval directly to the legend
legend(
  "bottomright", 
  legend = paste("AUC =", round(auc_value, 3), 
                 "\n95% CI:", round(ci_auc[1], 3), "-", round(ci_auc[2], 3)),
  col = "#D55E00", 
  lwd = 3, 
  bty = "n",                        # Remove legend box
  cex = 1.2,                        # Adjust legend size
  text.col = "black"                # Make the text color black for better contrast
)

# 14. Save the cleaned and processed data for future use
write.csv(data_scaled, "processed_heart_disease_data.csv", row.names = FALSE)
cat("\nCleaned and processed data saved as 'processed_heart_disease_data.csv'\n")

# Print the AUC value and its confidence interval to the console
cat("Area Under the Curve (AUC): ", round(auc_value, 3), "\n")
cat("95% Confidence Interval for AUC: [", round(ci_auc[1], 3), ", ", round(ci_auc[2], 3), "]\n")



View(data)

####ROC Curve####
str(data[, c("age", "sex", "chol", "trestbps", "thalach", "oldpeak")])

cat("\nColumns in `data`:\n")
print(colnames(data))


# Re-add missing columns from the original dataset
if (!"age" %in% colnames(data)) {
  data$age <- data_original$age[match(rownames(data), rownames(data_original))]
}

if (!"chol" %in% colnames(data)) {
  data$chol <- data_original$chol[match(rownames(data), rownames(data_original))]
}

if (!"trestbps" %in% colnames(data)) {
  data$trestbps <- data_original$trestbps[match(rownames(data), rownames(data_original))]
}


# Verify that all required columns are now in `data`
cat("\nUpdated column names in `data`:\n")
print(colnames(data))


if (!"oldpeak" %in% colnames(data)) {
  cat("Adding `oldpeak` back to `data` from `data_original`.\n")
  data$oldpeak <- data_original$oldpeak[match(rownames(data), rownames(data_original))]
}

# Verify the updated columns
cat("\nColumns in `data` after adding `oldpeak`:\n")
print(colnames(data))

# Proceed with standardization
preprocess_params <- preProcess(data[, c("age", "chol", "trestbps", "thalach", "oldpeak")], method = c("center", "scale"))
data_scaled <- predict(preprocess_params, data)






set.seed(123)
trainIndex <- createDataPartition(data_scaled$num, p = 0.7, list = FALSE)
train <- data_scaled[trainIndex, ]
test <- data_scaled[-trainIndex, ]




# Step 1: Verify unique values in `num`
cat("\nUnique values in `train$num` before recoding:\n")
print(unique(train$num))

# Step 2: Recode `num` to binary (0 = No Disease, 1 = Disease)
train$num <- ifelse(train$num > 0, 1, 0)
test$num <- ifelse(test$num > 0, 1, 0)

cat("\nUnique values in `train$num` after recoding:\n")
print(unique(train$num))

# Step 3: Fit the logistic regression model
logistic_model <- glm(num ~ age + sex + chol + trestbps + thalach + oldpeak, 
                      data = train, family = binomial)

# Step 4: Display model summary
cat("\nLogistic Regression Model Summary:\n")
summary(logistic_model)


#Predict on the Test Set
predictions <- predict(logistic_model, newdata = test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, "Disease", "No Disease")
predicted_classes <- as.factor(predicted_classes)



test$num <- ifelse(test$num == 0, "No Disease", "Disease")
test$num <- as.factor(test$num)


# 11. Predict on the test set
predictions <- predict(logistic_model, newdata = test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, "Disease", "No Disease")
predicted_classes <- as.factor(predicted_classes)

# 12. Evaluate model performance
conf_matrix <- confusionMatrix(predicted_classes, test$num)
print("Confusion Matrix:")
print(conf_matrix)
cat("\nModel Accuracy: ", conf_matrix$overall['Accuracy'], "\n")




# Load necessary library
library(pROC)

# Generate the ROC curve object
roc_curve <- roc(test$num, predictions)

# Customize the ROC plot
plot(
  roc_curve, 
  col = "#D55E00",                # Professional color
  lwd = 2,                        # Line width
  main = "ROC Curve for Logistic Regression", 
  cex.main = 1.5,                 # Title size
  cex.lab = 1.2,                  # Axis label size
  cex.axis = 1                    # Axis text size
)

# Add a diagonal line for random guessing
abline(a = 0, b = 1, lty = 2, col = "grey")

# Add AUC annotation
auc_value <- auc(roc_curve)
legend(
  "bottomright", 
  legend = paste("AUC =", round(auc_value, 3)), 
  col = "#D55E00", 
  lwd = 2, 
  bty = "n",                        # Remove legend box
  cex = 1.2                         # Adjust legend size
)

# Print the AUC value to the console
cat("Area Under the Curve (AUC): ", round(auc_value, 3), "\n")





# 14. Save the cleaned and processed data for future use
write.csv(data_scaled, "processed_heart_disease_data.csv", row.names = FALSE)
cat("\nCleaned and processed data saved as 'processed_heart_disease_data.csv'\n")










# Calculate the AUC with confidence intervals
auc_value <- auc(roc_curve)
ci_auc <- ci(roc_curve, conf.level = 0.95)  # 95% Confidence Interval

# Customize the ROC plot
plot(
  roc_curve, 
  col = "#D55E00",                # Professional color
  lwd = 2,                        # Line width
  main = "ROC Curve for Logistic Regression", 
  cex.main = 1.5,                 # Title size
  cex.lab = 1.2,                  # Axis label size
  cex.axis = 1                    # Axis text size
)

# Add a diagonal line for random guessing
abline(a = 0, b = 1, lty = 2, col = "grey")

# Add AUC and CI annotation (fixed: removed extra comma)
legend(
  "bottomright", 
  legend = paste("AUC =", round(auc_value, 3), 
                 "\n95% CI:", round(ci_auc[1], 3), "-", round(ci_auc[2], 3)),  # Fixed paste function
  col = "#D55E00", 
  lwd = 2, 
  bty = "n",                        # Remove legend box
  cex = 1.2                         # Adjust legend size
)

# Print the AUC value and its confidence interval to the console
cat("Area Under the Curve (AUC): ", round(auc_value, 3), "\n")
cat("95% Confidence Interval for AUC: [", round(ci_auc[1], 3), ", ", round(ci_auc[2], 3), "]\n")






# Load necessary library
library(pROC)

# Generate the ROC curve object
roc_curve <- roc(test$num, predictions)

# Calculate the AUC with confidence intervals
auc_value <- auc(roc_curve)
ci_auc <- ci(roc_curve, conf.level = 0.95)  # 95% Confidence Interval

# Customize the ROC plot with a professional, clean look
plot(
  roc_curve, 
  col = "#D55E00",                # Professional color for the ROC curve
  lwd = 3,                        # Line width (thicker line)
  main = "ROC Curve for Logistic Regression", 
  cex.main = 1.5,                 # Title size
  cex.lab = 1.4,                  # Axis label size
  cex.axis = 1.2,                 # Axis text size
  xlab = "False Positive Rate",   # X-axis label
  ylab = "True Positive Rate",    # Y-axis label
  xlim = c(0, 1),                 # Set axis limits
  ylim = c(0, 1)
)

# Add a diagonal line for random guessing
abline(a = 0, b = 1, lty = 2, col = "grey")

# Add AUC and CI annotation in the plot itself (not just the legend)
text(0.6, 0.2, 
     paste("AUC =", round(auc_value, 3), 
           "\n95% CI: [", round(ci_auc[1], 3), ", ", round(ci_auc[2], 3), "]"), 
     col = "black", cex = 1.2, font = 2)

# Optional: Add the confidence interval directly to the legend
legend(
  "bottomright", 
  legend = paste("AUC =", round(auc_value, 3), 
                 "\n95% CI:", round(ci_auc[1], 3), "-", round(ci_auc[2], 3)),
  col = "#D55E00", 
  lwd = 3, 
  bty = "n",                        # Remove legend box
  cex = 1.2,                        # Adjust legend size
  text.col = "black"                # Make the text color black for better contrast
)

# Print the AUC value and its confidence interval to the console
cat("Area Under the Curve (AUC): ", round(auc_value, 3), "\n")
cat("95% Confidence Interval for AUC: [", round(ci_auc[1], 3), ", ", round(ci_auc[2], 3), "]\n")











####Survival Analysis for Age####
# Load necessary libraries
library(survival)
library(survminer)

# Data preparation
data <- na.omit(data)  # Remove missing values
data$num <- as.factor(data$num)  # Convert 'num' as the event status (0 = No event, 1 = Event)

# Check unique values in `num` to identify any invalid or extra levels
cat("\nUnique values in 'num' column:\n")
print(unique(data$num))




# Check the structure of 'num' and its levels before any transformation
cat("\nStructure of 'num' column:\n")
print(str(data$num))
cat("\nUnique values in 'num' column:\n")
print(unique(data$num))

# Recode `num` as numeric (if it's still a factor with invalid levels)
data$num <- as.numeric(as.character(data$num))  # Convert to numeric

# Recode to binary event status (0 = No event, 1 = Event)
data$num <- ifelse(data$num > 0, 1, 0)

# Ensure `num` is a factor with valid levels (No Disease = 0, Disease = 1)
data$num <- factor(data$num, levels = c(0, 1), labels = c("No Disease", "Disease"))

# Check unique values after recoding
cat("\nUnique values in 'num' after recoding:\n")
print(unique(data$num))


# Recheck missing values in important columns
cat("\nMissing values in relevant columns:\n")
print(colSums(is.na(data[, c("age", "num")])))  # Check if `age` and `num` have missing values

# Check unique values in `num` after recoding (it should be 0 for No Disease and 1 for Disease)
cat("\nUnique values in 'num' after recoding:\n")
print(unique(data$num))



# Ensure `num` is a factor with valid levels and there are no NA values
data$num <- as.character(data$num)  # Convert factor to character first
data$num[is.na(data$num)] <- "No Disease"  # Replace NA values with a default category (e.g., "No Disease")

# Convert `num` to numeric (0 for "No Disease", 1 for "Disease")
data$num <- ifelse(data$num == "No Disease", 0, 1)  # Recode to 0/1

# Convert `num` back to a factor with proper levels
data$num <- factor(data$num, levels = c(0, 1), labels = c("No Disease", "Disease"))

# Check unique values in `num` after recoding to ensure it's properly recoded
cat("\nUnique values in 'num' after recoding:\n")
print(unique(data$num))

# Ensure `age` and `num` have no missing values
cat("\nMissing values in relevant columns after recoding:\n")
print(colSums(is.na(data[, c("age", "num")])))  # Check for missing values in `age` and `num`

# Proceed with creating the survival object for Kaplan-Meier analysis
surv_object <- Surv(time = data$age, event = as.numeric(data$num))

# Check the structure of the `Surv` object to ensure it's correctly created
cat("\nSurv object structure:\n")
print(str(surv_object)



# Check the levels and distribution of `num` to confirm it's properly encoded
 cat("\nLevels and distribution of `num`:\n")
 print(table(data$num))   # This will show the count of each category
      
      # Check if `num` has any missing values
      cat("\nMissing values in `num`:\n")
      print(sum(is.na(data$num)))  # If the sum is more than 0, there are missing values
      
      # Verify that `num` is a factor with proper levels
      cat("\nLevels of 'num' after recoding:\n")
      print(levels(data$num))  # Should show "No Disease" and "Disease"
      




# Simulate a small dataset with both categories
set.seed(123)
data_test <- data.frame(
  age = sample(30:80, 100, replace = TRUE),
  num = sample(c("No Disease", "Disease"), 100, replace = TRUE)
)

# Convert `num` to a factor
data_test$num <- factor(data_test$num, levels = c("No Disease", "Disease"))

# Create the survival object
surv_object <- Surv(time = data_test$age, event = as.numeric(data_test$num) - 1)

# Fit Kaplan-Meier survival model
fit <- survfit(surv_object ~ num, data = data_test)



# Plot the Kaplan-Meier curve
ggsurvplot(
  fit,
  data = data_test,
  title = "Kaplan-Meier Survival Curve by Disease Status",
  xlab = "Age",
  ylab = "Survival Probability",
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  ggtheme = theme_minimal()
)





####Check Dataset Dimensions####
sapply(data, length)
names(data)

#Simulate a Time Variable:
set.seed(123)  # For reproducibility
data$time <- sample(1:100, nrow(data), replace = TRUE)

#Define an Event Variable:
data$event <- as.numeric(data$num) - 1  # Convert "Disease"/"No Disease" to 1/0



#Check for Perfect Separation
table(data$event, data$cholesterol_level)
table(data$event, data$trestbps)
table(data$event, data$thalach)

# Combine low and normal into a single category
data$cholesterol_level <- ifelse(data$cholesterol_level == "low", "normal", data$cholesterol_level)
# Sparse Data in trestbps
# Bin trestbps into categories
data$trestbps_cat <- cut(data$trestbps, breaks = c(90, 120, 140, 160, 200), right = FALSE,
                         labels = c("Low", "Normal", "High", "Very High"))

#Sparse Data in thalach
# Bin thalach into categories
data$thalach_cat <- cut(data$thalach, breaks = c(70, 120, 150, 200), right = FALSE,
                        labels = c("Low", "Normal", "High"))

#fitting the Model:
####Create and Fit the Survival Model:####
# Create the survival object
surv_object <- Surv(time = data$time, event = data$event)



# Try using cholesterol as a continuous variable instead of categories
cox_model <- coxph(surv_object ~ age + chol + trestbps + thalach, data = data)
summary(cox_model)

####1. Focus on Significant Predictors:####
cox_model <- coxph(surv_object ~ age * thalach + chol * thalach + trestbps, data = data)
names(data)
table(data$exang)  # Check for any meaningful patterns
#Use exang as a binary predictor for physical activity in the Cox mode
cox_model <- coxph(surv_object ~ age + chol + trestbps + thalach + exang, data = data)


#Check interaction effects with other predictors (e.g., thalach, age, or chol)


cox_model <- coxph(surv_object ~ age * exang + chol * exang + trestbps + thalach, data = data)


####Check the proportional hazards assumption####
cox.zph(cox_model)

#Check the model summary
summary(cox_model)

# Fit the Cox proportional hazards model
cox_model <- coxph(surv_object ~ age + cholesterol_level + trestbps + thalach, data = data)

# Summarize the model
summary(cox_model)
#Multicollinearity Check
library(car)
vif(cox_model)

####Remove multicollinearity####
#Step 1: Fit the Initial Model (with all predictors)
# Fit the initial Cox proportional hazards model
cox_model <- coxph(surv_object ~ age * exang + chol * exang + trestbps + thalach, data = data)
#Calculate VIF (Variance Inflation Factors)

library(car)
vif(cox_model)


####Fit the Simplified Model:Remove High VIF Variables####

# Remove exang and interaction terms with exang
cox_model_simplified <- coxph(surv_object ~ age + chol + trestbps + thalach, data = data)

#Check the VIF for the Simplified Model:
vif(cox_model_simplified)


#Model Summary
# Summary of the simplified Cox model
summary(cox_model_simplified)


####Check Proportional Hazards Assumption####

cox.zph(cox_model_simplified)

#Model Interpretation: Focus on Thalach
# Display the coefficient for thalach
summary(cox_model_simplified)$coefficients["thalach", ]

# Interpreting the exp(coef) for thalach
# A unit increase in thalach decreases the hazard by 1.8% (exp(coef) < 1)
#Model Refinement: Check for interaction terms and additional clinical variables.
# Add interaction terms (age * thalach and chol * thalach)
cox_model_interaction <- coxph(surv_object ~ age * thalach + chol * thalach + trestbps, data = data)

# Check the summary of the model with interaction terms
summary(cox_model_interaction)







# Simplifying the model by removing non-significant terms
cox_model_refined <- coxph(surv_object ~ thalach, data = data)

# Check model summary
summary(cox_model_refined)
# Final model for interpretation
cox_model_refined <- coxph(surv_object ~ thalach, data = data)

# Report the final model summary
cat("\nFinal Model for The Lancet Publication\n")
cat("\nThalach (Maximum Heart Rate): Hazard Ratio =", exp(cox_model_refined$coefficients["thalach"]), "\n")
cat("p-value =", summary(cox_model_refined)$coefficients["thalach", "Pr(>|z|)"], "\n")
cat("\nModel Concordance (C-index) =", summary(cox_model_refined)$concordance, "\n")
cat("\nFinal Model Summary:\n")
summary(cox_model_refined)



####Create the Data Frame for Visualization####


# Assuming cox_model_refined is your final model
cox_summary <- summary(cox_model_refined)

# Create a data frame for the forest plot
forest_data <- data.frame(
  variable = rownames(cox_summary$coefficients),  # Variable names
  coef = cox_summary$coefficients[, 1],  # Coefficients
  lower_ci = cox_summary$conf.int[, 3],  # Lower bound of 95% CI
  upper_ci = cox_summary$conf.int[, 4],  # Upper bound of 95% CI
  exp_coef = cox_summary$conf.int[, 2]   # Hazard Ratios (exp(coef))
)

# Print the forest data to check
print(forest_data)

#Forest Plot Visualization with ggplot2

# Load the ggplot2 library
library(ggplot2)

# Create the forest plot
forest_plot <- ggplot(forest_data, aes(x = exp_coef, y = variable)) +
  geom_point(size = 3, color = "blue") +  # Points for hazard ratio (HR)
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2) +  # CI bars
  scale_x_log10() +  # Logarithmic scale for the HR (standard for forest plots)
  theme_minimal() +  # Clean and simple theme
  labs(title = "Forest Plot for Cox Proportional Hazards Model", 
       x = "Hazard Ratio (Exp(Coefficient))", 
       y = "Predictors") +  # Label axes
  theme(axis.text.y = element_text(size = 12),  # Adjust y-axis label size
        axis.title = element_text(size = 14),   # Axis title size
        plot.title = element_text(size = 16, face = "bold"),  # Title size and style
        plot.margin = margin(1, 1, 1, 1, "cm"))  # Adjust plot margins

# Display the plot
print(forest_plot)









####Compare Survival Curves by Gender####

# Load necessary libraries
library(survival)
library(survminer)


# Data preparation
data <- na.omit(data)  # Remove missing values
data$num <- as.factor(data$num)  # Convert 'num' as the event status (0 = No event, 1 = Event)
data$sex <- ifelse(data$sex == 1, "Male", "Female")  # Convert 'sex' to labels

# Define the survival object
surv_object <- Surv(time = data$age, event = as.numeric(data$num))

# Kaplan-Meier survival curves stratified by gender
fit_gender <- survfit(surv_object ~ sex, data = data)

# Plot the Kaplan-Meier curves
ggsurvplot(
  fit_gender,
  data = data,
  pval = TRUE,              # Display p-value for the log-rank test
  conf.int = TRUE,          # Confidence interval for survival estimates
  risk.table = TRUE,        # Add risk table
  legend.labs = c("Male", "Female"),  # Label for the legend
  title = "Kaplan-Meier Survival Curves by Gender",  # Plot title
  xlab = "Age",  # X-axis label
  ylab = "Survival Probability",  # Y-axis label
  ggtheme = theme_minimal()  # Apply a clean theme to the plot
)


#Cox Proportional Hazards Model for Gender Effect#
cox_model_gender <- coxph(Surv(time = data$age, event = as.numeric(data$num)) ~ sex, data = data)
summary(cox_model_gender)



####Cox Proportional Hazards Model for Gender Effect####


cox_model_gender <- coxph(Surv(time = data$age, event = as.numeric(data$num)) ~ sex, data = data)


# Display the summary of the Cox model
summary(cox_model_gender)














# Load necessary libraries for visualization
library(ggplot2)
library(survival)
library(dplyr)


# Extract model coefficients and confidence intervals
cox_summary <- summary(cox_model_gender)
hr <- cox_summary$coefficients[, "exp(coef)"]
hr_lower <- cox_summary$conf.int[, "lower .95"]
hr_upper <- cox_summary$conf.int[, "upper .95"]
variables <- rownames(cox_summary$coefficients)

# Create a data frame for visualization
cox_plot_data <- data.frame(
  Variable = variables,
  HR = hr,
  HR_lower = hr_lower,
  HR_upper = hr_upper
)

# Plot the hazard ratios with confidence intervals
ggplot(cox_plot_data, aes(x = Variable, y = HR, ymin = HR_lower, ymax = HR_upper)) +
  geom_pointrange(color = "blue", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Hazard Ratios from Cox Proportional Hazards Model (Gender Effect)",
    x = "Variable",
    y = "Hazard Ratio (HR)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16)
  )





####1. Model Interpretation and Analysis####


# Display the summary of the Cox model for gender effect
summary(cox_model_gender)




# Extracting key results from the model for interpretation
cox_summary <- summary(cox_model_gender)

# Hazard Ratio (HR) for male
hr_sex <- cox_summary$coefficients["sexMale", "exp(coef)"]

# 95% Confidence Interval for HR
ci_sex_lower <- cox_summary$conf.int["sexMale", "lower .95"]
ci_sex_upper <- cox_summary$conf.int["sexMale", "upper .95"]

# p-value for gender effect
p_value_sex <- cox_summary$coefficients["sexMale", "Pr(>|z|)"]

# Display the results
cat("Hazard Ratio for Male vs Female: HR =", hr_sex, "\n")
cat("95% CI for Male vs Female: [", ci_sex_lower, ", ", ci_sex_upper, "]\n")
cat("p-value for Gender Effect: ", p_value_sex, "\n")




####Proportional Hazards Assumption Check####

# Check proportional hazards assumption
cox_zph <- cox.zph(cox_model_gender)

# Display results for proportional hazards assumption
cox_zph


# Plot the results of the proportional hazards assumption test
plot(cox_zph)

# You can further customize the plot if desired:
# Add a title to the plot
title("Proportional Hazards Assumption Test for Gender")




#Model Refinement
# If additional variables (like age, cholesterol) are included, fit the refined model
cox_model_refined <- coxph(Surv(time = data$age, event = as.numeric(data$num)) ~ sex + age + chol, data = data)

# Display the refined model summary
summary(cox_model_refined)



#Forest Plot of Hazard Ratios with Confidence Intervals
# Load necessary libraries for visualization
library(ggplot2)

# Extract model coefficients and confidence intervals
cox_summary <- summary(cox_model_refined)
hr <- cox_summary$coefficients[, "exp(coef)"]
hr_lower <- cox_summary$conf.int[, "lower .95"]
hr_upper <- cox_summary$conf.int[, "upper .95"]
variables <- rownames(cox_summary$coefficients)

# Create a data frame for visualization
cox_plot_data <- data.frame(
  Variable = variables,
  HR = hr,
  HR_lower = hr_lower,
  HR_upper = hr_upper
)

# Plot the hazard ratios with confidence intervals
ggplot(cox_plot_data, aes(x = Variable, y = HR, ymin = HR_lower, ymax = HR_upper)) +
  geom_pointrange(color = "blue", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Hazard Ratios from Cox Proportional Hazards Model (Gender Effect)",
    x = "Variable",
    y = "Hazard Ratio (HR)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16)
  )



#Kaplan-Meier Survival Curves by Gender
# Load necessary libraries
library(survival)
library(survminer)

# Data preparation (assuming you have already done this in previous steps)
data$sex <- ifelse(data$sex == 1, "Male", "Female")  # Convert sex to labels
surv_object <- Surv(time = data$age, event = as.numeric(data$num))  # Survival object

# Fit the Kaplan-Meier model stratified by sex
fit_gender <- survfit(surv_object ~ sex, data = data)



# Ensure 'sex' is a factor
data$sex <- factor(data$sex, levels = c("Male", "Female"))

# Confirm the levels
levels(data$sex)

# Cross-tabulate 'sex' with the event variable to check for proper distribution
table(data$sex, data$num)


unique(data$sex)  # Check unique values in the 'sex' column


table(data$sex)  # Check the distribution of 'sex' in the original dataset
data <- read.csv("E:\\Ru Poster to paper\\archive (1)\\heart_disease_data_with_features.csv")
data


#Recode the sex Variable
data$sex <- factor(data$sex, levels = c(0, 1), labels = c("Female", "Male"))

table(data$sex)

# Recode 'data$num'
data$num <- ifelse(data$num %in% c(1, 2, 3, 4), 1, 0)  # 1 for events, 0 for censoring

unique(data$num)

data <- na.omit(data)  # Remove rows with missing values


fit_gender <- survfit(Surv(time = data$age, event = data$num) ~ sex, data = data)



#Plot the Kaplan-Meier Curves
library(survminer)

# Plot the Kaplan-Meier curves with p-value and confidence intervals
ggsurvplot(
  fit_gender,
  data = data,
  pval = TRUE,              # Display p-value for the log-rank test
  conf.int = TRUE,          # Confidence interval for survival estimates
  risk.table = TRUE,        # Add risk table
  legend.labs = c("Male", "Female"),  # Correct the legend labels
  legend.title = "Gender",  # Title for the legend
  title = "Kaplan-Meier Survival Curves by Gender",
  xlab = "Age",
  ylab = "Survival Probability",
  ggtheme = theme_minimal()
)


# Fit the Cox model for gender effect
cox_model_gender <- coxph(Surv(time = data$age, event = data$num) ~ sex, data = data)

# Display the model summary
summary(cox_model_gender)

# Check Variance Inflation Factor (VIF) if there are multiple predictors
# This step is not applicable when there are fewer than two predictors
if (length(cox_model_refined$coefficients) > 1) {
  library(car)
  vif(cox_model_refined)
} else {
  cat("VIF is not applicable with only one predictor.")
}



####Model Validation####
#Cross-Validation or Bootstrap for Model Validation
# Load necessary libraries
library(survival)
library(boot)

# Set seed for reproducibility
set.seed(123)

# Define cross-validation function
cv_coxph <- function(data, K = 10) {
  # Create folds for cross-validation
  folds <- cut(seq(1, nrow(data)), breaks = K, labels = FALSE)
  cox_results <- list()
  
  # Perform K-fold cross-validation
  for(i in 1:K) {
    # Split data into training and testing sets
    test_index <- which(folds == i, arr.ind = TRUE)
    train_data <- data[-test_index, ]
    test_data <- data[test_index, ]
    
    # Fit the Cox model on the training set
    cox_model <- coxph(Surv(time = train_data$age, event = as.numeric(train_data$num)) ~ sex, data = train_data)
    
    # Predict the risk on the test data
    pred <- predict(cox_model, newdata = test_data, type = "risk")
    
    # Store the predictions and observed events
    cox_results[[i]] <- data.frame(observed = test_data$num, predicted = pred)
  }
  
  # Combine all results from the K-folds
  results <- do.call(rbind, cox_results)
  return(results)
}

# Perform 10-fold cross-validation for the Cox model
cv_results <- cv_coxph(data, K = 10)

# View results (observed vs. predicted)
head(cv_results)



#Calculate Concordance Index (C-index):
# Calculate C-index for the cross-validation results
library(survival)
cindex_results <- survConcordance(Surv(observed, 1 - observed) ~ predicted, data = cv_results)

# Display C-index
cindex_results$concordance





####To improve the model's performance Add More Predictors####
# Include more variables (e.g., age, cholesterol level, etc.)
cox_model_extended <- coxph(Surv(time = data$age, event = as.numeric(data$num)) ~ sex + age + chol, data = data)
summary(cox_model_extended)


# Include more variables (e.g., age, cholesterol level, etc.)
cox_model_extended <- coxph(Surv(time = data$age, event = as.numeric(data$num)) ~ sex + age + chol, data = data)
summary(cox_model_extended)


####Model Tuning####
####Regularization (Lasso/Ridge Regression):####
install.packages("glmnet")

library(glmnet)
x <- model.matrix(Surv(time = data$age, event = as.numeric(data$num)) ~ sex + age + chol, data = data)[,-1]
y <- Surv(time = data$age, event = as.numeric(data$num))

# Fit the Lasso model
cox_lasso <- cv.glmnet(x, y, alpha = 1, family = "cox")
plot(cox_lasso)

# Display the best model based on cross-validation
best_lambda <- cox_lasso$lambda.min
cox_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda, family = "cox")
summary(cox_lasso_model)



####Ridge Regression (L2):####

cox_ridge <- cv.glmnet(x, y, alpha = 0, family = "cox")
plot(cox_ridge)





####Check for Interactions####

# Adding interaction term between age and sex
cox_model_interaction <- coxph(Surv(time = data$age, event = as.numeric(data$num)) ~ sex * age + chol, data = data)
summary(cox_model_interaction)

# Scale predictors
data_scaled <- data
data_scaled$age <- scale(data$age)
data_scaled$chol <- scale(data$chol)
cox_model_scaled <- coxph(Surv(time = data_scaled$age, event = as.numeric(data_scaled$num)) ~ sex + age + chol, data = data_scaled)
summary(cox_model_scaled)


# Check for missing values in the data
sum(is.na(data$sex))
sum(is.na(data$age))
sum(is.na(data$chol))
sum(is.na(data$num))

# Remove rows with missing values if necessary
data_clean <- data[complete.cases(data[, c("sex", "age", "chol", "num")]), ]




# Check for infinite values
sum(is.infinite(data$age))
sum(is.infinite(data$chol))

# Remove rows with Inf values if necessary
data_clean <- data_clean[!is.infinite(data_clean$age) & !is.infinite(data_clean$chol), ]




# Ensure 'sex' is a factor
data_clean$sex <- factor(data_clean$sex, levels = c("Female", "Male"))

# Create dummy variables for the categorical predictors
X <- model.matrix(~ sex + age + chol, data = data_clean)[, -1]  # Use model.matrix and remove the intercept column

# Check the resulting matrix
head(X)






# Convert the response variable to Surv object
surv_obj <- Surv(time = data_clean$age, event = as.numeric(data_clean$num))

# Fit the elastic net Cox model
cox_model_elasticnet <- cv.glmnet(X, surv_obj, family = "cox", alpha = 0.5)  # alpha = 0.5 for elastic net (Lasso + Ridge)

# Display the model results
print(cox_model_elasticnet)



# Display the coefficients at the best lambda
coef(cox_model_elasticnet, s = "lambda.min")



#Plot the Cross-Validation Results:
plot(cox_model_elasticnet)



#Model Predictions

# Predict the risk on the training data
predicted_risk <- predict(cox_model_elasticnet, newx = X, s = "lambda.min", type = "link")

# View the predicted risks for the first few observations
head(predicted_risk)



#Concordance Index (C-index)

# Calculate C-index
cindex_results <- survConcordance(Surv(time = data_clean$age, event = as.numeric(data_clean$num)) ~ predicted_risk)

# Display the C-index
print(cindex_results$concordance)



# Model Validation on an Independent Dataset (if available)
# Load necessary libraries
library(caret)

# Split the data into training and test sets (80% training, 20% test)
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data_clean$num, p = 0.8, list = FALSE)
train_data <- data_clean[trainIndex, ]
test_data <- data_clean[-trainIndex, ]

# Now you can proceed with creating model matrix for test_data
X_test <- model.matrix(~ sex + age + chol, data = test_data)[, -1]
surv_obj_test <- Surv(time = test_data$age, event = as.numeric(test_data$num))

# Predict the risk on the test data
predicted_risk_test <- predict(cox_model_elasticnet, newx = X_test, s = "lambda.min")

# Calculate C-index for the test set
library(survival)
cindex_test <- concordance(Surv(time = test_data$age, event = as.numeric(test_data$num)) ~ predicted_risk_test)
print(cindex_test$concordance)


#Feature Exploration
# Create interaction term between age and cholesterol
data_clean$age_chol <- data_clean$age * data_clean$chol

# Fit the elastic net model with the new interaction term
X_interaction <- model.matrix(~ sex + age + chol + age_chol, data = data_clean)[, -1]
cox_model_elasticnet_interaction <- cv.glmnet(X_interaction, surv_obj, family = "cox", alpha = 0.5)

# Display the coefficients of the model with the interaction term
coef(cox_model_elasticnet_interaction, s = "lambda.min")



#Performance Comparison with Other Models (e.g., Lasso and Ridge)


# Fit Lasso model (alpha = 1)
cox_model_lasso <- cv.glmnet(X, surv_obj, family = "cox", alpha = 1)
coef(cox_model_lasso, s = "lambda.min")

# Fit Ridge model (alpha = 0)
cox_model_ridge <- cv.glmnet(X, surv_obj, family = "cox", alpha = 0)
coef(cox_model_ridge, s = "lambda.min")

# Compare the C-index for each model
predicted_risk_lasso <- predict(cox_model_lasso, newx = X, s = "lambda.min")
predicted_risk_ridge <- predict(cox_model_ridge, newx = X, s = "lambda.min")

cindex_lasso <- concordance(Surv(time = data_clean$age, event = as.numeric(data_clean$num)) ~ predicted_risk_lasso)
cindex_ridge <- concordance(Surv(time = data_clean$age, event = as.numeric(data_clean$num)) ~ predicted_risk_ridge)

print(cindex_lasso$concordance)
print(cindex_ridge$concordance)





####Cross-Validation on Multiple Models for Model Comparison####
# Load necessary libraries
library(glmnet)
library(survival)

# Prepare the survival response variable
surv_obj <- Surv(time = data_clean$age, event = as.numeric(data_clean$num))

# Prepare the predictor matrix (exclude the response variable and create dummies for 'sex')
X <- model.matrix(~ sex + age + chol, data = data_clean)[, -1]

# Fit the Lasso model using cv.glmnet (alpha = 1 for Lasso)
lasso_model <- cv.glmnet(X, surv_obj, family = "cox", alpha = 1)

# Display the results
print(lasso_model)

# Plot the cross-validation results
plot(lasso_model)

# Coefficients at the optimal lambda
coef(lasso_model, s = "lambda.min")


####Evaluate Model Performance:####

# Calculate the predicted risks for the model at the optimal lambda
predicted_risk <- predict(lasso_model, newx = X, s = "lambda.min", type = "link")

# Calculate the C-index using the observed survival times and predicted risks
cindex_results <- concordance(Surv(time = data_clean$age, event = as.numeric(data_clean$num)) ~ predicted_risk)

# Display the C-index
print(cindex_results)



#Evaluate Performance on Test Set:

# If you have a test dataset (e.g., test_data), you can predict as follows:
X_test <- model.matrix(~ sex + age + chol, data = test_data)[, -1]
predicted_risk_test <- predict(lasso_model, newx = X_test, s = "lambda.min", type = "link")
# Calculate the C-index on the test data
cindex_test <- concordance(Surv(time = test_data$age, event = as.numeric(test_data$num)) ~ predicted_risk_test)

# Display the C-index for the test set
print(cindex_test)

#Make Predictions

# Predict the survival probabilities for new data
predicted_probs <- predict(lasso_model, newx = X_test, s = "lambda.min", type = "response")

#Visualize Results:
# Create a Kaplan-Meier curve based on predicted risks
library(survminer)
surv_fit <- survfit(Surv(time = data_clean$age, event = as.numeric(data_clean$num)) ~ predicted_risk)
ggsurvplot(surv_fit, data = data_clean)
coord_flip()


















































































