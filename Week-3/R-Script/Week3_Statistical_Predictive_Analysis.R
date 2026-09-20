# ============================================================
# WEEK 3 - STATISTICAL ANALYSIS & PREDICTIVE MODELING
# Virtual R Data Analyst Internship
# Dataset: Telco Customer Churn
# ============================================================

# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

library(tidyverse)
library(janitor)
library(skimr)
library(caret)
library(pROC)
library(broom)
library(car)

# ============================================================
# 2. PATH SETUP
# ============================================================

base_path <- "D:/Virtual R Data Analyst Intern/Week-3"

dataset_path <- file.path(
  base_path,
  "Dataset",
  "Telco_Customer_Churn_Week3.csv"
)

screenshot_path <- file.path(
  base_path,
  "Screenshots"
)

report_path <- file.path(
  base_path,
  "Report"
)

dir.create(screenshot_path, showWarnings = FALSE)
dir.create(report_path, showWarnings = FALSE)

# ============================================================
# 3. LOAD DATA
# ============================================================

data <- read.csv(
  dataset_path,
  stringsAsFactors = FALSE
)

# Clean column names
data <- clean_names(data)

cat("\n============================================\n")
cat("DATASET OVERVIEW\n")
cat("============================================\n")

print(dim(data))
print(names(data))
str(data)

# ============================================================
# 4. DATA CLEANING
# ============================================================

# Convert TotalCharges to numeric
data$total_charges <- as.numeric(data$total_charges)

# Replace missing TotalCharges for zero-tenure customers
data$total_charges[is.na(data$total_charges)] <- 0

# Convert categorical variables to factors

data$gender <- factor(data$gender)

data$senior_citizen <- factor(
  data$senior_citizen,
  levels = c(0, 1)
)

data$partner <- factor(data$partner)

data$dependents <- factor(data$dependents)

data$phone_service <- factor(data$phone_service)

data$multiple_lines <- factor(data$multiple_lines)

data$internet_service <- factor(data$internet_service)

data$online_security <- factor(data$online_security)

data$online_backup <- factor(data$online_backup)

data$device_protection <- factor(data$device_protection)

data$tech_support <- factor(data$tech_support)

data$streaming_tv <- factor(data$streaming_tv)

data$streaming_movies <- factor(data$streaming_movies)

data$contract <- factor(
  data$contract,
  levels = c(
    "Month-to-month",
    "One year",
    "Two year"
  )
)

data$paperless_billing <- factor(
  data$paperless_billing
)

data$payment_method <- factor(
  data$payment_method
)

data$churn <- factor(
  data$churn,
  levels = c("No", "Yes")
)

# Numeric target version
data$churn_numeric <- ifelse(
  data$churn == "Yes",
  1,
  0
)

# ============================================================
# 5. SAVE CLEANED DATASET
# ============================================================

write.csv(
  data,
  file.path(
    base_path,
    "Dataset",
    "Telco_Customer_Churn_Week3_Cleaned.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 6. SUMMARY STATISTICS
# ============================================================

cat("\n============================================\n")
cat("SUMMARY STATISTICS\n")
cat("============================================\n")

print(summary(data))

cat("\n============================================\n")
cat("SKIM SUMMARY\n")
cat("============================================\n")

print(skim(data))

# ============================================================
# 7. MISSING VALUES
# ============================================================

cat("\n============================================\n")
cat("MISSING VALUES\n")
cat("============================================\n")

missing_values <- colSums(is.na(data))

print(missing_values)

write.csv(
  data.frame(
    Variable = names(missing_values),
    Missing_Values = as.numeric(missing_values)
  ),
  file.path(
    report_path,
    "Missing_Values.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 8. DUPLICATE CHECK
# ============================================================

cat("\n============================================\n")
cat("DUPLICATES\n")
cat("============================================\n")

duplicate_count <- sum(duplicated(data))

print(duplicate_count)

# ============================================================
# 9. CHURN DISTRIBUTION
# ============================================================

cat("\n============================================\n")
cat("CHURN DISTRIBUTION\n")
cat("============================================\n")

churn_counts <- table(data$churn)

print(churn_counts)

churn_percentages <- prop.table(churn_counts) * 100

print(churn_percentages)

# Save churn distribution
write.csv(
  data.frame(
    Churn = names(churn_counts),
    Count = as.numeric(churn_counts),
    Percentage = as.numeric(churn_percentages)
  ),
  file.path(
    report_path,
    "Churn_Distribution.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 10. NUMERICAL SUMMARY
# ============================================================

numerical_summary <- data.frame(
  Mean_Tenure = mean(data$tenure),
  Median_Tenure = median(data$tenure),
  SD_Tenure = sd(data$tenure),
  Mean_Monthly_Charges = mean(data$monthly_charges),
  Median_Monthly_Charges = median(data$monthly_charges),
  SD_Monthly_Charges = sd(data$monthly_charges),
  Mean_Total_Charges = mean(data$total_charges),
  Median_Total_Charges = median(data$total_charges),
  SD_Total_Charges = sd(data$total_charges)
)

cat("\n============================================\n")
cat("NUMERICAL SUMMARY\n")
cat("============================================\n")

print(numerical_summary)

write.csv(
  numerical_summary,
  file.path(
    report_path,
    "Numerical_Summary.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 11. HYPOTHESIS TEST 1
# MONTHLY CHARGES VS CHURN
# ============================================================

cat("\n============================================\n")
cat("HYPOTHESIS TEST 1\n")
cat("============================================\n")

cat(
  "H0: Mean monthly charges are equal for churned and non-churned customers.\n"
)

cat(
  "H1: Mean monthly charges are different for churned and non-churned customers.\n"
)

monthly_charge_test <- t.test(
  monthly_charges ~ churn,
  data = data
)

print(monthly_charge_test)

# ============================================================
# 12. HYPOTHESIS TEST 2
# CONTRACT VS CHURN
# ============================================================

cat("\n============================================\n")
cat("HYPOTHESIS TEST 2\n")
cat("============================================\n")

cat(
  "H0: Contract type and churn are independent.\n"
)

cat(
  "H1: Contract type and churn are associated.\n"
)

contract_table <- table(
  data$contract,
  data$churn
)

print(contract_table)

contract_chisq <- chisq.test(
  contract_table
)

print(contract_chisq)

# ============================================================
# 13. HYPOTHESIS TEST 3
# INTERNET SERVICE VS CHURN
# ============================================================

cat("\n============================================\n")
cat("HYPOTHESIS TEST 3\n")
cat("============================================\n")

cat(
  "H0: Internet service type and churn are independent.\n"
)

cat(
  "H1: Internet service type and churn are associated.\n"
)

internet_table <- table(
  data$internet_service,
  data$churn
)

print(internet_table)

internet_chisq <- chisq.test(
  internet_table
)

print(internet_chisq)

# ============================================================
# 14. CORRELATION ANALYSIS
# ============================================================

cat("\n============================================\n")
cat("CORRELATION ANALYSIS\n")
cat("============================================\n")

numeric_data <- data %>%
  select(
    tenure,
    monthly_charges,
    total_charges
  )

correlation_matrix <- cor(
  numeric_data,
  use = "complete.obs"
)

print(correlation_matrix)

write.csv(
  correlation_matrix,
  file.path(
    report_path,
    "Correlation_Matrix.csv"
  )
)

# ============================================================
# 15. CORRELATION TESTS
# ============================================================

cat("\n============================================\n")
cat("CORRELATION TESTS\n")
cat("============================================\n")

cor_tenure_total <- cor.test(
  data$tenure,
  data$total_charges,
  method = "pearson"
)

print(cor_tenure_total)

cor_tenure_monthly <- cor.test(
  data$tenure,
  data$monthly_charges,
  method = "pearson"
)

print(cor_tenure_monthly)

cor_monthly_total <- cor.test(
  data$monthly_charges,
  data$total_charges,
  method = "pearson"
)

print(cor_monthly_total)

# ============================================================
# 16. NORMALITY CHECK
# ============================================================

cat("\n============================================\n")
cat("NORMALITY CHECK\n")
cat("============================================\n")

set.seed(123)

tenure_sample <- sample(
  data$tenure,
  min(5000, nrow(data))
)

monthly_sample <- sample(
  data$monthly_charges,
  min(5000, nrow(data))
)

shapiro_tenure <- shapiro.test(
  tenure_sample
)

print(shapiro_tenure)

shapiro_monthly <- shapiro.test(
  monthly_sample
)

print(shapiro_monthly)

# ============================================================
# 17. VISUALIZATION 1
# MONTHLY CHARGES BY CHURN
# ============================================================

cat("\n============================================\n")
cat("DATA VISUALIZATION FOR STATISTICAL ANALYSIS\n")
cat("============================================\n")

p1 <- ggplot(
  data,
  aes(
    x = churn,
    y = monthly_charges
  )
) +
  geom_boxplot() +
  labs(
    title = "Monthly Charges by Churn Status",
    x = "Churn",
    y = "Monthly Charges"
  ) +
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "01_Monthly_Charges_Boxplot.png"
  ),
  p1,
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 18. VISUALIZATION 2
# TENURE VS TOTAL CHARGES
# ============================================================

p2 <- ggplot(
  data,
  aes(
    x = tenure,
    y = total_charges,
    color = churn
  )
) +
  geom_point(
    alpha = 0.35
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Tenure vs Total Charges",
    x = "Tenure (Months)",
    y = "Total Charges",
    color = "Churn"
  ) +
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "02_Tenure_Total_Charges.png"
  ),
  p2,
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 19. VISUALIZATION 3
# MONTHLY CHARGES DISTRIBUTION
# ============================================================

p3 <- ggplot(
  data,
  aes(
    x = monthly_charges,
    fill = churn
  )
) +
  geom_histogram(
    bins = 30,
    alpha = 0.6,
    position = "identity"
  ) +
  labs(
    title = "Distribution of Monthly Charges by Churn",
    x = "Monthly Charges",
    y = "Frequency",
    fill = "Churn"
  ) +
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "03_Monthly_Charges_Distribution.png"
  ),
  p3,
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 20. VISUALIZATION 4
# TENURE DISTRIBUTION
# ============================================================

p4 <- ggplot(
  data,
  aes(
    x = tenure,
    fill = churn
  )
) +
  geom_histogram(
    bins = 30,
    alpha = 0.6,
    position = "identity"
  ) +
  labs(
    title = "Tenure Distribution by Churn Status",
    x = "Tenure (Months)",
    y = "Frequency",
    fill = "Churn"
  ) +
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "04_Tenure_Distribution.png"
  ),
  p4,
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 21. LOGISTIC REGRESSION MODEL
# ============================================================

cat("\n============================================\n")
cat("LOGISTIC REGRESSION\n")
cat("============================================\n")

# Cleaner model:
# online_security and tech_support are excluded because
# their "No internet service" levels are redundant with
# internet_service.

model <- glm(
  churn ~
    tenure +
    monthly_charges +
    total_charges +
    contract +
    internet_service +
    payment_method +
    senior_citizen +
    partner +
    dependents +
    paperless_billing,
  family = binomial,
  data = data
)

# ============================================================
# 22. MODEL SUMMARY
# ============================================================

cat("\n============================================\n")
cat("MODEL SUMMARY\n")
cat("============================================\n")

print(summary(model))

# Tidy coefficients
model_coefficients <- tidy(
  model,
  conf.int = TRUE,
  exponentiate = TRUE
)

print(model_coefficients)

write.csv(
  model_coefficients,
  file.path(
    report_path,
    "Logistic_Regression_Coefficients.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 23. MODEL FIT
# ============================================================

model_fit <- data.frame(
  Null_Deviance = model$null.deviance,
  Residual_Deviance = model$deviance,
  AIC = AIC(model)
)

print(model_fit)

write.csv(
  model_fit,
  file.path(
    report_path,
    "Model_Fit.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 24. MULTICOLLINEARITY CHECK
# ============================================================

cat("\n============================================\n")
cat("MULTICOLLINEARITY CHECK\n")
cat("============================================\n")

vif_values <- vif(model)

print(vif_values)

# Handle both standard VIF and GVIF outputs
if (is.matrix(vif_values)) {
  
  vif_table <- data.frame(
    Variable = rownames(vif_values),
    GVIF = vif_values[, "GVIF"],
    Df = vif_values[, "Df"],
    GVIF_Adjusted = vif_values[
      ,
      "GVIF^(1/(2*Df))"
    ],
    row.names = NULL
  )
  
} else {
  
  vif_table <- data.frame(
    Variable = names(vif_values),
    VIF = as.numeric(vif_values)
  )
}

print(vif_table)

write.csv(
  vif_table,
  file.path(
    report_path,
    "VIF_Results.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 25. TRAIN / TEST SPLIT
# ============================================================

cat("\n============================================\n")
cat("TRAIN / TEST SPLIT\n")
cat("============================================\n")

set.seed(123)

train_index <- createDataPartition(
  data$churn,
  p = 0.80,
  list = FALSE
)

train_data <- data[train_index, ]

test_data <- data[-train_index, ]

cat(
  "Training rows:",
  nrow(train_data),
  "\n"
)

cat(
  "Testing rows:",
  nrow(test_data),
  "\n"
)

# ============================================================
# 26. CROSS-VALIDATION
# ============================================================

cat("\n============================================\n")
cat("5-FOLD CROSS-VALIDATION\n")
cat("============================================\n")

# caret expects the event class to be the first factor level
train_data$churn <- factor(
  train_data$churn,
  levels = c("Yes", "No")
)

test_data$churn <- factor(
  test_data$churn,
  levels = c("Yes", "No")
)

set.seed(123)

cv_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

cv_model <- train(
  churn ~
    tenure +
    monthly_charges +
    total_charges +
    contract +
    internet_service +
    payment_method +
    senior_citizen +
    partner +
    dependents +
    paperless_billing,
  data = train_data,
  method = "glm",
  family = binomial,
  metric = "ROC",
  trControl = cv_control
)

print(cv_model)

cat("\nCross-validation results:\n")

print(
  cv_model$results
)

write.csv(
  cv_model$results,
  file.path(
    report_path,
    "Cross_Validation_Results.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 27. TEST SET PREDICTIONS
# ============================================================

cat("\n============================================\n")
cat("TEST SET PREDICTIONS\n")
cat("============================================\n")

test_probability <- predict(
  cv_model,
  newdata = test_data,
  type = "prob"
)[, "Yes"]

test_prediction <- predict(
  cv_model,
  newdata = test_data,
  type = "raw"
)

# ============================================================
# 28. CONFUSION MATRIX
# ============================================================

cat("\n============================================\n")
cat("CONFUSION MATRIX\n")
cat("============================================\n")

confusion <- confusionMatrix(
  test_prediction,
  test_data$churn,
  positive = "Yes"
)

print(confusion)

# ============================================================
# 29. PERFORMANCE METRICS
# ============================================================

cat("\n============================================\n")
cat("MODEL PERFORMANCE METRICS\n")
cat("============================================\n")

accuracy <- confusion$overall["Accuracy"]

kappa <- confusion$overall["Kappa"]

sensitivity <- confusion$byClass["Sensitivity"]

specificity <- confusion$byClass["Specificity"]

precision <- confusion$byClass["Precision"]

recall <- confusion$byClass["Recall"]

f1 <- confusion$byClass["F1"]

metrics <- data.frame(
  Metric = c(
    "Accuracy",
    "Kappa",
    "Sensitivity",
    "Specificity",
    "Precision",
    "Recall",
    "F1 Score"
  ),
  Value = c(
    accuracy,
    kappa,
    sensitivity,
    specificity,
    precision,
    recall,
    f1
  )
)

print(metrics)

write.csv(
  metrics,
  file.path(
    report_path,
    "Model_Performance_Metrics.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 30. ROC CURVE AND AUC
# ============================================================

cat("\n============================================\n")
cat("ROC / AUC ANALYSIS\n")
cat("============================================\n")

roc_object <- roc(
  test_data$churn,
  test_probability,
  levels = c("No", "Yes"),
  direction = "<"
)

auc_value <- auc(
  roc_object
)

cat(
  "AUC:",
  as.numeric(auc_value),
  "\n"
)

roc_data <- data.frame(
  False_Positive_Rate = 1 - roc_object$specificities,
  True_Positive_Rate = roc_object$sensitivities
)

roc_plot <- ggplot(
  roc_data,
  aes(
    x = False_Positive_Rate,
    y = True_Positive_Rate
  )
) +
  geom_line() +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = paste0(
      "ROC Curve - AUC = ",
      round(
        as.numeric(auc_value),
        3
      )
    ),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "05_ROC_Curve.png"
  ),
  roc_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# Save AUC
write.csv(
  data.frame(
    AUC = as.numeric(auc_value)
  ),
  file.path(
    report_path,
    "ROC_AUC.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 31. PREDICTED CHURN PROBABILITY
# ============================================================

probability_data <- data.frame(
  Churn_Probability = test_probability,
  Actual_Churn = test_data$churn
)

probability_plot <- ggplot(
  probability_data,
  aes(
    x = Churn_Probability,
    fill = Actual_Churn
  )
) +
  geom_histogram(
    bins = 30,
    alpha = 0.7,
    position = "identity"
  ) +
  labs(
    title = "Predicted Churn Probability Distribution",
    x = "Predicted Probability of Churn",
    y = "Frequency",
    fill = "Actual Churn"
  ) +
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "06_Predicted_Churn_Probability.png"
  ),
  probability_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 32. MODEL DIAGNOSTICS
# ============================================================

png(
  filename = file.path(
    screenshot_path,
    "07_Model_Diagnostics.png"
  ),
  width = 1600,
  height = 1200,
  res = 180
)

par(
  mfrow = c(2, 2)
)

plot(model)

dev.off()

# Reset plotting layout
par(
  mfrow = c(1, 1)
)

# ============================================================
# 33. SAVE TEST PREDICTIONS
# ============================================================

prediction_results <- data.frame(
  Actual_Churn = test_data$churn,
  Predicted_Churn = test_prediction,
  Predicted_Probability = test_probability
)

write.csv(
  prediction_results,
  file.path(
    report_path,
    "Test_Set_Predictions.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 34. FINAL MODEL SUMMARY
# ============================================================

final_model_summary <- data.frame(
  Dataset_Rows = nrow(data),
  Training_Rows = nrow(train_data),
  Testing_Rows = nrow(test_data),
  Churn_Rate = mean(
    data$churn == "Yes"
  ),
  Accuracy = as.numeric(
    accuracy
  ),
  Sensitivity = as.numeric(
    sensitivity
  ),
  Specificity = as.numeric(
    specificity
  ),
  Precision = as.numeric(
    precision
  ),
  Recall = as.numeric(
    recall
  ),
  F1_Score = as.numeric(
    f1
  ),
  AUC = as.numeric(
    auc_value
  ),
  AIC = AIC(model)
)

cat("\n============================================\n")
cat("FINAL MODEL SUMMARY\n")
cat("============================================\n")

print(final_model_summary)

write.csv(
  final_model_summary,
  file.path(
    report_path,
    "Final_Model_Summary.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 35. FINAL OUTPUT CHECK
# ============================================================

cat("\n============================================\n")
cat("WEEK 3 ANALYSIS COMPLETED\n")
cat("============================================\n")

cat(
  "\nScreenshots generated:\n"
)

print(
  list.files(
    screenshot_path
  )
)

cat(
  "\nReport/data outputs generated:\n"
)

print(
  list.files(
    report_path
  )
)

cat(
  "\nCleaned dataset saved successfully.\n"
)

cat(
  "\nFinal Accuracy:",
  round(
    as.numeric(accuracy),
    4
  ),
  "\n"
)

cat(
  "Final F1 Score:",
  round(
    as.numeric(f1),
    4
  ),
  "\n"
)

cat(
  "Final AUC:",
  round(
    as.numeric(auc_value),
    4
  ),
  "\n"
)

cat(
  "\n============================================\n"
)
cat("END OF WEEK 3 ANALYSIS")
cat("\n============================================\n")