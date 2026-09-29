# ============================================================
# VIRTUAL R DATA ANALYST INTERNSHIP
# WEEK 4
# COMPREHENSIVE DATA ANALYSIS REPORTING AND PRESENTATION
# ============================================================
#
# This script integrates:
# Week 1  -> Data Cleaning & Preliminary Analysis
# Week 2  -> Data Visualization & Insight Communication
# Week 3  -> Statistical Analysis & Predictive Modeling
# Week 4  -> Comprehensive Final Analysis
#
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
# 2. PROJECT PATHS
# ============================================================

base_path <- "D:/Virtual R Data Analyst Intern/Week-4"

dataset_path <- file.path(
  base_path,
  "Dataset"
)

script_path <- file.path(
  base_path,
  "R-Script"
)

screenshot_path <- file.path(
  base_path,
  "Screenshots"
)

report_path <- file.path(
  base_path,
  "Report"
)


# ============================================================
# 3. CREATE REQUIRED FOLDERS
# ============================================================

dir.create(
  base_path,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  dataset_path,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  script_path,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  screenshot_path,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  report_path,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 4. DATASET PATHS
# ============================================================

week3_cleaned_dataset <- paste0(
  "D:/Virtual R Data Analyst Intern/",
  "Week-3/Dataset/",
  "Telco_Customer_Churn_Week3_Cleaned.csv"
)

week4_dataset <- file.path(
  dataset_path,
  "Telco_Customer_Churn_Week4.csv"
)


# ============================================================
# 5. CHECK WEEK 3 DATASET
# ============================================================

if (!file.exists(week3_cleaned_dataset)) {
  
  stop(
    paste(
      "Week 3 cleaned dataset was not found:",
      week3_cleaned_dataset
    )
  )
  
}


# ============================================================
# 6. COPY DATASET INTO WEEK 4
# ============================================================

file.copy(
  from = week3_cleaned_dataset,
  to = week4_dataset,
  overwrite = TRUE
)


# ============================================================
# 7. LOAD DATA
# ============================================================

data <- read.csv(
  week4_dataset,
  stringsAsFactors = FALSE
)


# ============================================================
# 8. STANDARDIZE COLUMN NAMES
# ============================================================

data <- data %>%
  clean_names()


# ============================================================
# 9. DATA TYPE PREPARATION
# ============================================================

if ("total_charges" %in% names(data)) {
  
  data$total_charges <- as.numeric(
    data$total_charges
  )
  
  data$total_charges[
    is.na(data$total_charges) &
      data$tenure == 0
  ] <- 0
  
}


# ============================================================
# 10. FACTOR CONVERSION
# ============================================================

factor_columns <- c(
  "gender",
  "senior_citizen",
  "partner",
  "dependents",
  "phone_service",
  "multiple_lines",
  "internet_service",
  "online_security",
  "online_backup",
  "device_protection",
  "tech_support",
  "streaming_tv",
  "streaming_movies",
  "contract",
  "paperless_billing",
  "payment_method",
  "churn"
)

factor_columns <- intersect(
  factor_columns,
  names(data)
)

data[factor_columns] <- lapply(
  data[factor_columns],
  factor
)


# ============================================================
# 11. CREATE CHURN NUMERIC VARIABLE
# ============================================================

data$churn_numeric <- ifelse(
  data$churn == "Yes",
  1,
  0
)


# ============================================================
# 12. CREATE TENURE GROUP
# ============================================================

data$tenure_group <- cut(
  data$tenure,
  breaks = c(
    -1,
    12,
    24,
    48,
    60,
    Inf
  ),
  labels = c(
    "0-12 Months",
    "13-24 Months",
    "25-48 Months",
    "49-60 Months",
    "60+ Months"
  )
)


# ============================================================
# 13. BASIC DATA INFORMATION
# ============================================================

cat("\n============================================\n")
cat("WEEK 4 - DATASET INFORMATION\n")
cat("============================================\n")

cat(
  "Rows:",
  nrow(data),
  "\n"
)

cat(
  "Columns:",
  ncol(data),
  "\n"
)

cat(
  "Variables:\n"
)

print(
  names(data)
)


# ============================================================
# 14. DATA STRUCTURE
# ============================================================

cat("\n============================================\n")
cat("DATA STRUCTURE\n")
cat("============================================\n")

str(data)


# ============================================================
# 15. MISSING VALUES
# ============================================================

missing_values <- colSums(
  is.na(data)
)

total_missing <- sum(
  missing_values
)

cat("\n============================================\n")
cat("MISSING VALUE ANALYSIS\n")
cat("============================================\n")

print(
  missing_values
)

cat(
  "\nTotal missing values:",
  total_missing,
  "\n"
)

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
# 16. DUPLICATE CHECK
# ============================================================

duplicate_count <- sum(
  duplicated(data)
)

cat("\n============================================\n")
cat("DUPLICATE ANALYSIS\n")
cat("============================================\n")

cat(
  "Duplicate rows:",
  duplicate_count,
  "\n"
)


# ============================================================
# 17. DATA QUALITY SUMMARY
# ============================================================

data_quality <- data.frame(
  
  Metric = c(
    "Rows",
    "Columns",
    "Missing Values",
    "Duplicate Rows"
  ),
  
  Value = c(
    nrow(data),
    ncol(data),
    total_missing,
    duplicate_count
  )
  
)

write.csv(
  data_quality,
  file.path(
    report_path,
    "Dataset_Quality_Summary.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 18. CHURN DISTRIBUTION
# ============================================================

churn_counts <- table(
  data$churn
)

churn_percentages <- prop.table(
  churn_counts
) * 100

cat("\n============================================\n")
cat("CHURN DISTRIBUTION\n")
cat("============================================\n")

print(churn_counts)

print(
  round(
    churn_percentages,
    2
  )
)

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
# 19. NUMERICAL SUMMARY
# ============================================================

numerical_summary <- data.frame(
  
  Variable = c(
    "Tenure",
    "Monthly Charges",
    "Total Charges"
  ),
  
  Mean = c(
    mean(data$tenure),
    mean(data$monthly_charges),
    mean(data$total_charges)
  ),
  
  Median = c(
    median(data$tenure),
    median(data$monthly_charges),
    median(data$total_charges)
  ),
  
  SD = c(
    sd(data$tenure),
    sd(data$monthly_charges),
    sd(data$total_charges)
  ),
  
  Minimum = c(
    min(data$tenure),
    min(data$monthly_charges),
    min(data$total_charges)
  ),
  
  Maximum = c(
    max(data$tenure),
    max(data$monthly_charges),
    max(data$total_charges)
  )
  
)

cat("\n============================================\n")
cat("NUMERICAL SUMMARY\n")
cat("============================================\n")

print(
  numerical_summary
)

write.csv(
  numerical_summary,
  file.path(
    report_path,
    "Numerical_Summary.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 20. WEEK 1 - CONTRACT ANALYSIS
# ============================================================

contract_summary <- data %>%
  
  group_by(contract) %>%
  
  summarise(
    Customers = n(),
    Churned = sum(churn == "Yes"),
    Churn_Rate = mean(churn == "Yes") * 100,
    Average_Monthly_Charges =
      mean(monthly_charges),
    Average_Tenure =
      mean(tenure),
    .groups = "drop"
  )

cat("\n============================================\n")
cat("CONTRACT ANALYSIS\n")
cat("============================================\n")

print(
  contract_summary
)

write.csv(
  contract_summary,
  file.path(
    report_path,
    "Contract_Analysis.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 21. WEEK 2 - INTERNET SERVICE ANALYSIS
# ============================================================

internet_summary <- data %>%
  
  group_by(internet_service) %>%
  
  summarise(
    Customers = n(),
    Churned = sum(churn == "Yes"),
    Churn_Rate = mean(churn == "Yes") * 100,
    Average_Monthly_Charges =
      mean(monthly_charges),
    .groups = "drop"
  )

write.csv(
  internet_summary,
  file.path(
    report_path,
    "Internet_Service_Analysis.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 22. PAYMENT METHOD ANALYSIS
# ============================================================

payment_summary <- data %>%
  
  group_by(payment_method) %>%
  
  summarise(
    Customers = n(),
    Churned = sum(churn == "Yes"),
    Churn_Rate = mean(churn == "Yes") * 100,
    .groups = "drop"
  )

write.csv(
  payment_summary,
  file.path(
    report_path,
    "Payment_Method_Analysis.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 23. TENURE GROUP ANALYSIS
# ============================================================

tenure_summary <- data %>%
  
  group_by(tenure_group) %>%
  
  summarise(
    Customers = n(),
    Churned = sum(churn == "Yes"),
    Churn_Rate = mean(churn == "Yes") * 100,
    .groups = "drop"
  )

write.csv(
  tenure_summary,
  file.path(
    report_path,
    "Tenure_Group_Analysis.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 24. SENIOR CITIZEN ANALYSIS
# ============================================================

senior_summary <- data %>%
  
  group_by(senior_citizen) %>%
  
  summarise(
    Customers = n(),
    Churned = sum(churn == "Yes"),
    Churn_Rate = mean(churn == "Yes") * 100,
    .groups = "drop"
  )

write.csv(
  senior_summary,
  file.path(
    report_path,
    "Senior_Citizen_Analysis.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 25. VISUALIZATION 1
# CUSTOMER CHURN DISTRIBUTION
# ============================================================

p1 <- ggplot(
  data,
  aes(
    x = churn
  )
) +
  
  geom_bar() +
  
  labs(
    title = "Customer Churn Distribution",
    x = "Churn Status",
    y = "Number of Customers"
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "01_Churn_Distribution.png"
  ),
  p1,
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================================
# 26. VISUALIZATION 2
# CHURN RATE BY CONTRACT
# ============================================================

p2 <- ggplot(
  contract_summary,
  aes(
    x = contract,
    y = Churn_Rate
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(
      label = paste0(
        round(
          Churn_Rate,
          1
        ),
        "%"
      )
    ),
    vjust = -0.4
  ) +
  
  labs(
    title = "Churn Rate by Contract Type",
    x = "Contract Type",
    y = "Churn Rate (%)"
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "02_Churn_Rate_by_Contract.png"
  ),
  p2,
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================================
# 27. VISUALIZATION 3
# TENURE DISTRIBUTION BY CHURN
# ============================================================

p3 <- ggplot(
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
    y = "Number of Customers",
    fill = "Churn"
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "03_Tenure_Distribution_by_Churn.png"
  ),
  p3,
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================================
# 28. VISUALIZATION 4
# MONTHLY CHARGES BY CHURN
# ============================================================

p4 <- ggplot(
  data,
  aes(
    x = churn,
    y = monthly_charges
  )
) +
  
  geom_boxplot() +
  
  labs(
    title = "Monthly Charges by Churn Status",
    x = "Churn Status",
    y = "Monthly Charges"
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "04_Monthly_Charges_by_Churn.png"
  ),
  p4,
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================================
# 29. VISUALIZATION 5
# TENURE VS TOTAL CHARGES
# ============================================================

p5 <- ggplot(
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
    title = "Relationship Between Tenure and Total Charges",
    x = "Tenure (Months)",
    y = "Total Charges",
    color = "Churn"
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "05_Tenure_vs_Total_Charges.png"
  ),
  p5,
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================================
# 30. VISUALIZATION 6
# CHURN RATE BY INTERNET SERVICE
# ============================================================

p6 <- ggplot(
  internet_summary,
  aes(
    x = internet_service,
    y = Churn_Rate
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(
      label = paste0(
        round(
          Churn_Rate,
          1
        ),
        "%"
      )
    ),
    vjust = -0.4
  ) +
  
  labs(
    title = "Churn Rate by Internet Service",
    x = "Internet Service",
    y = "Churn Rate (%)"
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "06_Churn_Rate_by_Internet_Service.png"
  ),
  p6,
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================================
# 31. VISUALIZATION 7
# CHURN RATE BY PAYMENT METHOD
# ============================================================

p7 <- ggplot(
  payment_summary,
  aes(
    x = reorder(
      payment_method,
      Churn_Rate
    ),
    y = Churn_Rate
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(
      label = paste0(
        round(
          Churn_Rate,
          1
        ),
        "%"
      )
    ),
    hjust = -0.1
  ) +
  
  coord_flip() +
  
  labs(
    title = "Churn Rate by Payment Method",
    x = "Payment Method",
    y = "Churn Rate (%)"
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "07_Churn_Rate_by_Payment_Method.png"
  ),
  p7,
  width = 9,
  height = 6,
  dpi = 300
)


# ============================================================
# 32. VISUALIZATION 8
# CHURN RATE BY TENURE GROUP
# ============================================================

p8 <- ggplot(
  tenure_summary,
  aes(
    x = tenure_group,
    y = Churn_Rate
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(
      label = paste0(
        round(
          Churn_Rate,
          1
        ),
        "%"
      )
    ),
    vjust = -0.4
  ) +
  
  labs(
    title = "Churn Rate by Customer Tenure Group",
    x = "Tenure Group",
    y = "Churn Rate (%)"
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "08_Churn_Rate_by_Tenure_Group.png"
  ),
  p8,
  width = 9,
  height = 6,
  dpi = 300
)


# ============================================================
# 33. VISUALIZATION 9
# SENIOR CITIZEN CHURN
# ============================================================

senior_plot_data <- senior_summary

senior_plot_data$Customer_Group <- ifelse(
  senior_plot_data$senior_citizen == "1",
  "Senior",
  "Non-Senior"
)

p9 <- ggplot(
  senior_plot_data,
  aes(
    x = Customer_Group,
    y = Churn_Rate
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(
      label = paste0(
        round(
          Churn_Rate,
          1
        ),
        "%"
      )
    ),
    vjust = -0.4
  ) +
  
  labs(
    title = "Churn Rate by Senior Citizen Status",
    x = "Customer Group",
    y = "Churn Rate (%)"
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "09_Churn_Rate_by_Senior_Status.png"
  ),
  p9,
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================================
# 34. HYPOTHESIS TEST 1
# MONTHLY CHARGES VS CHURN
# ============================================================

monthly_charge_test <- t.test(
  monthly_charges ~ churn,
  data = data
)

cat("\n============================================\n")
cat("WELCH T-TEST\n")
cat("============================================\n")

print(
  monthly_charge_test
)

capture.output(
  monthly_charge_test,
  file = file.path(
    report_path,
    "T_Test_Monthly_Charges.txt"
  )
)


# ============================================================
# 35. HYPOTHESIS TEST 2
# CONTRACT VS CHURN
# ============================================================

contract_table <- table(
  data$contract,
  data$churn
)

contract_chisq <- chisq.test(
  contract_table
)

cat("\n============================================\n")
cat("CHI-SQUARE TEST - CONTRACT VS CHURN\n")
cat("============================================\n")

print(
  contract_table
)

print(
  contract_chisq
)

capture.output(
  contract_chisq,
  file = file.path(
    report_path,
    "Chi_Square_Contract_Churn.txt"
  )
)


# ============================================================
# 36. HYPOTHESIS TEST 3
# INTERNET SERVICE VS CHURN
# ============================================================

internet_table <- table(
  data$internet_service,
  data$churn
)

internet_chisq <- chisq.test(
  internet_table
)

cat("\n============================================\n")
cat("CHI-SQUARE TEST - INTERNET VS CHURN\n")
cat("============================================\n")

print(
  internet_table
)

print(
  internet_chisq
)

capture.output(
  internet_chisq,
  file = file.path(
    report_path,
    "Chi_Square_Internet_Churn.txt"
  )
)


# ============================================================
# 37. CORRELATION ANALYSIS
# ============================================================

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

cat("\n============================================\n")
cat("CORRELATION MATRIX\n")
cat("============================================\n")

print(
  correlation_matrix
)

write.csv(
  correlation_matrix,
  file.path(
    report_path,
    "Correlation_Matrix.csv"
  )
)


# ============================================================
# 38. CORRELATION TESTS
# ============================================================

cor_tenure_total <- cor.test(
  data$tenure,
  data$total_charges,
  method = "pearson"
)

cor_tenure_monthly <- cor.test(
  data$tenure,
  data$monthly_charges,
  method = "pearson"
)

cor_monthly_total <- cor.test(
  data$monthly_charges,
  data$total_charges,
  method = "pearson"
)

cat("\n============================================\n")
cat("PEARSON CORRELATION TESTS\n")
cat("============================================\n")

print(
  cor_tenure_total
)

print(
  cor_tenure_monthly
)

print(
  cor_monthly_total
)

capture.output(
  cor_tenure_total,
  cor_tenure_monthly,
  cor_monthly_total,
  file = file.path(
    report_path,
    "Correlation_Tests.txt"
  )
)


# ============================================================
# 39. VISUALIZATION 10
# CORRELATION HEATMAP
# ============================================================

correlation_long <- as.data.frame(
  as.table(
    correlation_matrix
  )
)

names(correlation_long) <- c(
  "Variable1",
  "Variable2",
  "Correlation"
)

p10 <- ggplot(
  correlation_long,
  aes(
    x = Variable1,
    y = Variable2,
    fill = Correlation
  )
) +
  
  geom_tile() +
  
  geom_text(
    aes(
      label = round(
        Correlation,
        2
      )
    )
  ) +
  
  labs(
    title = "Correlation Matrix of Numerical Variables",
    x = "",
    y = ""
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "10_Correlation_Heatmap.png"
  ),
  p10,
  width = 8,
  height = 6,
  dpi = 300
)


# ============================================================
# 40. NORMALITY TESTS
# ============================================================

set.seed(123)

tenure_sample <- sample(
  data$tenure,
  min(
    5000,
    nrow(data)
  )
)

monthly_sample <- sample(
  data$monthly_charges,
  min(
    5000,
    nrow(data)
  )
)

shapiro_tenure <- shapiro.test(
  tenure_sample
)

shapiro_monthly <- shapiro.test(
  monthly_sample
)

cat("\n============================================\n")
cat("NORMALITY TESTS\n")
cat("============================================\n")

print(
  shapiro_tenure
)

print(
  shapiro_monthly
)

capture.output(
  shapiro_tenure,
  shapiro_monthly,
  file = file.path(
    report_path,
    "Normality_Tests.txt"
  )
)


# ============================================================
# 41. LOGISTIC REGRESSION
# ============================================================

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
# 42. LOGISTIC REGRESSION SUMMARY
# ============================================================

cat("\n============================================\n")
cat("LOGISTIC REGRESSION MODEL\n")
cat("============================================\n")

print(
  summary(model)
)

capture.output(
  summary(model),
  file = file.path(
    report_path,
    "Logistic_Regression_Summary.txt"
  )
)


# ============================================================
# 43. MODEL COEFFICIENTS / ODDS RATIOS
# ============================================================

model_coefficients <- tidy(
  model,
  conf.int = TRUE,
  exponentiate = TRUE
)

cat("\n============================================\n")
cat("MODEL COEFFICIENTS / ODDS RATIOS\n")
cat("============================================\n")

print(
  model_coefficients
)

write.csv(
  model_coefficients,
  file.path(
    report_path,
    "Logistic_Regression_Coefficients.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 44. MODEL FIT
# ============================================================

model_fit <- data.frame(
  
  Null_Deviance =
    model$null.deviance,
  
  Residual_Deviance =
    model$deviance,
  
  AIC =
    AIC(model)
  
)

write.csv(
  model_fit,
  file.path(
    report_path,
    "Model_Fit.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 45. MULTICOLLINEARITY
# ============================================================

vif_values <- car::vif(
  model
)

cat("\n============================================\n")
cat("MULTICOLLINEARITY / VIF\n")
cat("============================================\n")

print(
  vif_values
)

if (is.matrix(vif_values)) {
  
  vif_table <- data.frame(
    
    Variable =
      rownames(vif_values),
    
    GVIF =
      vif_values[
        ,
        "GVIF"
      ],
    
    Df =
      vif_values[
        ,
        "Df"
      ],
    
    GVIF_Adjusted =
      vif_values[
        ,
        "GVIF^(1/(2*Df))"
      ],
    
    row.names = NULL
    
  )
  
} else {
  
  vif_table <- data.frame(
    
    Variable =
      names(vif_values),
    
    VIF =
      as.numeric(vif_values)
    
  )
  
}

write.csv(
  vif_table,
  file.path(
    report_path,
    "VIF_Results.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 46. TRAIN / TEST SPLIT
# ============================================================

set.seed(123)

train_index <- createDataPartition(
  data$churn,
  p = 0.80,
  list = FALSE
)

train_data <- data[
  train_index,
]

test_data <- data[
  -train_index,
]


# ============================================================
# 47. CLASS LEVELS FOR CARET
# ============================================================

train_data$churn <- factor(
  train_data$churn,
  levels = c(
    "Yes",
    "No"
  )
)

test_data$churn <- factor(
  test_data$churn,
  levels = c(
    "Yes",
    "No"
  )
)


# ============================================================
# 48. TRAINING / TESTING INFORMATION
# ============================================================

cat("\n============================================\n")
cat("TRAIN / TEST SPLIT\n")
cat("============================================\n")

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
# 49. 5-FOLD CROSS-VALIDATION
# ============================================================

set.seed(123)

cv_control <- trainControl(
  
  method = "cv",
  
  number = 5,
  
  classProbs = TRUE,
  
  summaryFunction =
    twoClassSummary,
  
  savePredictions =
    "final"
  
)


# ============================================================
# 50. CROSS-VALIDATED LOGISTIC MODEL
# ============================================================

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


# ============================================================
# 51. CROSS-VALIDATION RESULTS
# ============================================================

cat("\n============================================\n")
cat("5-FOLD CROSS-VALIDATION RESULTS\n")
cat("============================================\n")

print(
  cv_model
)

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
# 52. TEST SET PREDICTIONS
# ============================================================

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
# 53. CONFUSION MATRIX
# ============================================================

confusion <- confusionMatrix(
  
  test_prediction,
  
  test_data$churn,
  
  positive = "Yes"
  
)

cat("\n============================================\n")
cat("CONFUSION MATRIX\n")
cat("============================================\n")

print(
  confusion
)


# ============================================================
# 54. EXTRACT PERFORMANCE METRICS
# ============================================================

accuracy <- as.numeric(
  confusion$overall[
    "Accuracy"
  ]
)

kappa <- as.numeric(
  confusion$overall[
    "Kappa"
  ]
)

sensitivity <- as.numeric(
  confusion$byClass[
    "Sensitivity"
  ]
)

specificity <- as.numeric(
  confusion$byClass[
    "Specificity"
  ]
)

precision <- as.numeric(
  confusion$byClass[
    "Precision"
  ]
)

recall <- as.numeric(
  confusion$byClass[
    "Recall"
  ]
)

f1 <- as.numeric(
  confusion$byClass[
    "F1"
  ]
)


# ============================================================
# 55. MODEL PERFORMANCE TABLE
# ============================================================

performance_metrics <- data.frame(
  
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

cat("\n============================================\n")
cat("MODEL PERFORMANCE\n")
cat("============================================\n")

print(
  performance_metrics
)

write.csv(
  performance_metrics,
  file.path(
    report_path,
    "Model_Performance_Metrics.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 56. CONFUSION MATRIX DATA
# ============================================================

confusion_table <- as.data.frame(
  confusion$table
)

write.csv(
  confusion_table,
  file.path(
    report_path,
    "Confusion_Matrix.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 57. VISUALIZATION 11
# CONFUSION MATRIX
# ============================================================

p11 <- ggplot(
  confusion_table,
  aes(
    x = Reference,
    y = Prediction
  )
) +
  
  geom_tile() +
  
  geom_text(
    aes(
      label = Freq
    ),
    size = 7
  ) +
  
  labs(
    title = "Confusion Matrix",
    x = "Actual Churn",
    y = "Predicted Churn"
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    screenshot_path,
    "11_Confusion_Matrix.png"
  ),
  p11,
  width = 7,
  height = 6,
  dpi = 300
)


# ============================================================
# 58. ROC / AUC
# ============================================================

roc_object <- roc(
  
  test_data$churn,
  
  test_probability,
  
  levels = c(
    "No",
    "Yes"
  ),
  
  direction = "<"
  
)

auc_value <- as.numeric(
  auc(
    roc_object
  )
)

cat("\n============================================\n")
cat("ROC / AUC\n")
cat("============================================\n")

cat(
  "AUC:",
  auc_value,
  "\n"
)


# ============================================================
# 59. ROC DATA
# ============================================================

roc_data <- data.frame(
  
  False_Positive_Rate =
    1 - roc_object$specificities,
  
  True_Positive_Rate =
    roc_object$sensitivities
  
)


# ============================================================
# 60. VISUALIZATION 12
# ROC CURVE
# ============================================================

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
        auc_value,
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
    "12_ROC_Curve.png"
  ),
  roc_plot,
  width = 8,
  height = 6,
  dpi = 300
)

write.csv(
  data.frame(
    AUC = auc_value
  ),
  file.path(
    report_path,
    "ROC_AUC.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 61. PREDICTED PROBABILITY DATA
# ============================================================

probability_data <- data.frame(
  
  Churn_Probability =
    test_probability,
  
  Actual_Churn =
    test_data$churn
  
)

write.csv(
  probability_data,
  file.path(
    report_path,
    "Predicted_Churn_Probabilities.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 62. VISUALIZATION 13
# PREDICTED CHURN PROBABILITY
# ============================================================

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
    "13_Predicted_Churn_Probability.png"
  ),
  probability_plot,
  width = 8,
  height = 6,
  dpi = 300
)


# ============================================================
# 63. VISUALIZATION 14
# MODEL PERFORMANCE
# ============================================================

performance_plot <- performance_metrics %>%
  
  filter(
    Metric != "Kappa"
  ) %>%
  
  ggplot(
    aes(
      x = Metric,
      y = Value
    )
  ) +
  
  geom_col() +
  
  geom_text(
    aes(
      label = paste0(
        round(
          Value * 100,
          1
        ),
        "%"
      )
    ),
    vjust = -0.4
  ) +
  
  scale_y_continuous(
    limits = c(
      0,
      1
    )
  ) +
  
  labs(
    title = "Logistic Regression Model Performance",
    x = "Performance Metric",
    y = "Score"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x =
      element_text(
        angle = 25,
        hjust = 1
      )
  )

ggsave(
  file.path(
    screenshot_path,
    "14_Model_Performance.png"
  ),
  performance_plot,
  width = 9,
  height = 6,
  dpi = 300
)


# ============================================================
# 64. MODEL DIAGNOSTICS
# ============================================================

png(
  
  filename = file.path(
    screenshot_path,
    "15_Model_Diagnostics.png"
  ),
  
  width = 1600,
  
  height = 1200,
  
  res = 180
  
)

par(
  mfrow = c(
    2,
    2
  )
)

plot(
  model
)

dev.off()

par(
  mfrow = c(
    1,
    1
  )
)


# ============================================================
# 65. SAVE TEST PREDICTIONS
# ============================================================

prediction_results <- data.frame(
  
  Actual_Churn =
    test_data$churn,
  
  Predicted_Churn =
    test_prediction,
  
  Predicted_Probability =
    test_probability
  
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
# 66. FINAL MODEL SUMMARY
# ============================================================

cv_roc <- cv_model$results$ROC[1]

cv_sensitivity <- cv_model$results$Sens[1]

cv_specificity <- cv_model$results$Spec[1]

final_model_summary <- data.frame(
  
  Dataset_Rows =
    nrow(data),
  
  Dataset_Columns =
    ncol(data),
  
  Training_Rows =
    nrow(train_data),
  
  Testing_Rows =
    nrow(test_data),
  
  Churn_Rate =
    mean(
      data$churn == "Yes"
    ),
  
  Accuracy =
    accuracy,
  
  Sensitivity =
    sensitivity,
  
  Specificity =
    specificity,
  
  Precision =
    precision,
  
  Recall =
    recall,
  
  F1_Score =
    f1,
  
  Test_AUC =
    auc_value,
  
  CV_ROC =
    cv_roc,
  
  CV_Sensitivity =
    cv_sensitivity,
  
  CV_Specificity =
    cv_specificity,
  
  AIC =
    AIC(model)
  
)

cat("\n============================================\n")
cat("FINAL MODEL SUMMARY\n")
cat("============================================\n")

print(
  final_model_summary
)

write.csv(
  final_model_summary,
  file.path(
    report_path,
    "Final_Model_Summary.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 67. KEY FINDINGS TABLE
# ============================================================

key_findings <- data.frame(
  
  Analysis = c(
    
    "Overall Churn",
    
    "Month-to-Month Contract",
    
    "One-Year Contract",
    
    "Two-Year Contract",
    
    "Average Monthly Charges - No Churn",
    
    "Average Monthly Charges - Churn",
    
    "Tenure vs Total Charges Correlation",
    
    "Monthly Charges vs Total Charges Correlation",
    
    "Test Accuracy",
    
    "Test Sensitivity",
    
    "Test Specificity",
    
    "Test AUC",
    
    "5-Fold CV ROC"
    
  ),
  
  Result = c(
    
    paste0(
      round(
        mean(data$churn == "Yes") * 100,
        2
      ),
      "%"
    ),
    
    paste0(
      round(
        contract_summary$Churn_Rate[
          contract_summary$contract ==
            "Month-to-month"
        ],
        2
      ),
      "%"
    ),
    
    paste0(
      round(
        contract_summary$Churn_Rate[
          contract_summary$contract ==
            "One year"
        ],
        2
      ),
      "%"
    ),
    
    paste0(
      round(
        contract_summary$Churn_Rate[
          contract_summary$contract ==
            "Two year"
        ],
        2
      ),
      "%"
    ),
    
    round(
      mean(
        data$monthly_charges[
          data$churn == "No"
        ]
      ),
      2
    ),
    
    round(
      mean(
        data$monthly_charges[
          data$churn == "Yes"
        ]
      ),
      2
    ),
    
    round(
      cor(
        data$tenure,
        data$total_charges
      ),
      4
    ),
    
    round(
      cor(
        data$monthly_charges,
        data$total_charges
      ),
      4
    ),
    
    paste0(
      round(
        accuracy * 100,
        2
      ),
      "%"
    ),
    
    paste0(
      round(
        sensitivity * 100,
        2
      ),
      "%"
    ),
    
    paste0(
      round(
        specificity * 100,
        2
      ),
      "%"
    ),
    
    round(
      auc_value,
      4
    ),
    
    round(
      cv_roc,
      4
    )
    
  )
  
)

write.csv(
  key_findings,
  file.path(
    report_path,
    "Key_Findings.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 68. SAVE COMPLETE SUMMARY TEXT
# ============================================================

summary_text <- c(
  
  "WEEK 4 COMPREHENSIVE TELCO CUSTOMER CHURN ANALYSIS",
  
  "",
  
  paste(
    "Dataset rows:",
    nrow(data)
  ),
  
  paste(
    "Dataset columns:",
    ncol(data)
  ),
  
  paste(
    "Total missing values:",
    total_missing
  ),
  
  paste(
    "Duplicate rows:",
    duplicate_count
  ),
  
  paste(
    "Overall churn rate:",
    round(
      mean(data$churn == "Yes") * 100,
      2
    ),
    "%"
  ),
  
  "",
  
  "STATISTICAL TESTS",
  
  paste(
    "Monthly charges t-test p-value:",
    format.pval(
      monthly_charge_test$p.value
    )
  ),
  
  paste(
    "Contract chi-square p-value:",
    format.pval(
      contract_chisq$p.value
    )
  ),
  
  paste(
    "Internet service chi-square p-value:",
    format.pval(
      internet_chisq$p.value
    )
  ),
  
  "",
  
  "MODEL PERFORMANCE",
  
  paste(
    "Accuracy:",
    round(
      accuracy,
      4
    )
  ),
  
  paste(
    "Sensitivity:",
    round(
      sensitivity,
      4
    )
  ),
  
  paste(
    "Specificity:",
    round(
      specificity,
      4
    )
  ),
  
  paste(
    "Precision:",
    round(
      precision,
      4
    )
  ),
  
  paste(
    "F1 Score:",
    round(
      f1,
      4
    )
  ),
  
  paste(
    "Test AUC:",
    round(
      auc_value,
      4
    )
  ),
  
  paste(
    "5-Fold CV ROC:",
    round(
      cv_roc,
      4
    )
  )
  
)

writeLines(
  summary_text,
  file.path(
    report_path,
    "Week4_Final_Analysis_Summary.txt"
  )
)


# ============================================================
# 69. FINAL OUTPUT CHECK
# ============================================================

cat("\n============================================\n")
cat("WEEK 4 OUTPUT CHECK\n")
cat("============================================\n")

cat("\nScreenshots generated:\n")

print(
  list.files(
    screenshot_path
  )
)

cat("\nReport/data outputs generated:\n")

print(
  list.files(
    report_path
  )
)


# ============================================================
# 70. FINAL RESULTS
# ============================================================

cat("\n============================================\n")
cat("WEEK 4 ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("============================================\n")

cat(
  "\nDataset Rows:",
  nrow(data),
  "\n"
)

cat(
  "Dataset Columns:",
  ncol(data),
  "\n"
)

cat(
  "Missing Values:",
  total_missing,
  "\n"
)

cat(
  "Duplicate Rows:",
  duplicate_count,
  "\n"
)

cat(
  "\nAccuracy:",
  round(
    accuracy,
    4
  ),
  "\n"
)

cat(
  "Sensitivity:",
  round(
    sensitivity,
    4
  ),
  "\n"
)

cat(
  "Specificity:",
  round(
    specificity,
    4
  ),
  "\n"
)

cat(
  "Precision:",
  round(
    precision,
    4
  ),
  "\n"
)

cat(
  "F1 Score:",
  round(
    f1,
    4
  ),
  "\n"
)

cat(
  "Test AUC:",
  round(
    auc_value,
    4
  ),
  "\n"
)

cat(
  "5-Fold CV ROC:",
  round(
    cv_roc,
    4
  ),
  "\n"
)

cat(
  "\n============================================\n"
)
cat(
  "END OF WEEK 4 ANALYSIS\n"
)
cat(
  "============================================\n"
)