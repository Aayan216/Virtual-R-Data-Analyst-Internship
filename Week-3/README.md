# Week 3 — Statistical Analysis & Predictive Modeling using R

## Project Overview

Week 3 focuses on applying statistical analysis and predictive modeling techniques to the **Telco Customer Churn** dataset using R. The project extends the earlier descriptive and visualization work by adding hypothesis testing, correlation analysis, assumption checking, logistic regression, cross-validation, and model evaluation.

The objective is to identify statistically significant relationships with customer churn and build a classification model that predicts whether a customer is likely to churn.

## Dataset

- **Dataset:** Telco Customer Churn
- **Rows:** 7,043
- **Original variables:** 21
- **Target variable:** Churn
- **Churn = No:** 5,174 (73.46%)
- **Churn = Yes:** 1,869 (26.54%)
- **Missing values after cleaning:** 0
- **Duplicate records:** 0

The dataset contains customer demographic information, services, contract details, payment methods, billing information, tenure, and churn status.

## Tools & Technologies

- **R**
- **RStudio**
- **tidyverse**
- **janitor**
- **skimr**
- **caret**
- **pROC**
- **broom**
- **car**
- Logistic Regression
- Statistical Hypothesis Testing
- Cross-Validation
- ROC/AUC Analysis

## Data Preparation

The following preprocessing steps were performed:

1. Loaded the Telco Customer Churn dataset.
2. Standardized column names using `clean_names()`.
3. Converted `total_charges` to numeric format.
4. Handled missing numeric values.
5. Converted categorical variables to factors.
6. Created a binary numeric representation of the churn target.
7. Checked missing values and duplicate records.
8. Saved the cleaned dataset for further analysis.

## Statistical Analysis

### 1. Descriptive Statistics

Numerical summaries were generated for:

- Tenure
- Monthly Charges
- Total Charges

Overall means:

| Variable | Mean |
|---|---:|
| Tenure | 32.37 months |
| Monthly Charges | 64.76 |
| Total Charges | 2,279.73 |

### 2. Hypothesis Testing

#### Welch Two-Sample t-test

The monthly charges of churned and non-churned customers were compared.

- Mean Monthly Charges — No Churn: **61.27**
- Mean Monthly Charges — Churn: **74.44**
- t = **-18.408**
- p < **2.2e-16**

This provides strong statistical evidence of a difference in monthly charges between the two churn groups.

#### Chi-Square Test — Contract vs Churn

- χ² = **1184.6**
- df = **2**
- p < **2.2e-16**

Observed churn rates:

| Contract | Churn Rate |
|---|---:|
| Month-to-month | 42.70% |
| One year | 11.27% |
| Two year | 2.83% |

#### Chi-Square Test — Internet Service vs Churn

- χ² = **732.31**
- df = **2**
- p < **2.2e-16**

Observed churn rates:

| Internet Service | Churn Rate |
|---|---:|
| DSL | 18.96% |
| Fiber optic | 41.89% |
| No internet service | 7.40% |

## Correlation Analysis

Pearson correlation tests were performed on the main numerical variables.

| Variable Pair | Correlation |
|---|---:|
| Tenure – Total Charges | 0.8262 |
| Tenure – Monthly Charges | 0.2479 |
| Monthly Charges – Total Charges | 0.6512 |

The strongest relationship was between **tenure and total charges (r = 0.8262)**, which is expected because total charges accumulate over the customer's tenure.

## Assumption & Multicollinearity Analysis

Shapiro-Wilk tests showed that tenure and monthly charges do not follow a normal distribution in the dataset.

Variance Inflation Factors were also examined before interpreting the logistic regression model. The largest adjusted GVIF values were:

- Tenure: **4.03**
- Total Charges: **4.56**

This indicates moderate shared information between tenure and total charges, which should be considered when interpreting their individual regression coefficients.

## Predictive Modeling

### Logistic Regression

A binary logistic regression model was developed to predict customer churn using:

- Tenure
- Monthly Charges
- Total Charges
- Contract
- Internet Service
- Payment Method
- Senior Citizen status
- Partner status
- Dependents
- Paperless Billing

### Train-Test Split

- Training set: **5,636 records**
- Testing set: **1,407 records**
- Split: **80/20**
- Stratified sampling was used to preserve the churn class distribution.

### 5-Fold Cross-Validation

The training data was evaluated using 5-fold cross-validation.

- Mean Cross-Validation ROC: **0.8412**
- ROC standard deviation: **0.0083**

## Model Performance

The final model was evaluated on the independent test set.

| Metric | Result |
|---|---:|
| Accuracy | **79.82%** |
| Sensitivity / Recall | **51.21%** |
| Specificity | **90.14%** |
| Precision | **65.19%** |
| F1 Score | **0.5736** |
| ROC-AUC | **0.8338** |
| Kappa | **0.4438** |

### Confusion Matrix

| | Actual Churn | Actual No Churn |
|---|---:|---:|
| Predicted Churn | 191 | 102 |
| Predicted No Churn | 182 | 932 |

The model identifies non-churn customers more effectively than churn customers at the selected classification threshold. In particular, **182 actual churners were classified as non-churners**, which is reflected in the sensitivity of 51.21%.

## Key Findings

- The overall churn rate is **26.54%**.
- Churned customers have higher average monthly charges than non-churned customers.
- Contract type has a strong statistical association with churn.
- Month-to-month customers have a substantially higher observed churn rate than one- and two-year contract customers.
- Internet service is significantly associated with churn, with the highest observed churn rate among fiber-optic customers.
- Tenure and total charges have a strong positive correlation.
- The logistic regression achieved a test ROC-AUC of **0.8338**.
- The model has high specificity (**90.14%**) but lower sensitivity (**51.21%**), indicating that some actual churners are missed.

## Model Strengths

- Uses statistical hypothesis testing alongside predictive modeling.
- Uses a stratified train-test split.
- Includes 5-fold cross-validation.
- Evaluates multiple classification metrics rather than accuracy alone.
- Includes ROC-AUC and diagnostic analysis.
- Checks multicollinearity before interpreting the model.

## Improvement Areas

Potential improvements for future modeling include:

- Threshold tuning to increase churn detection.
- Class-weighted modeling or resampling techniques.
- Comparing logistic regression with tree-based models such as Random Forest or Gradient Boosting.
- Feature engineering using customer service and billing information.
- Further investigation of the relationship between tenure and total charges.

## Visualizations

The `Screenshots/` folder contains:

1. Monthly Charges Boxplot
2. Tenure vs Total Charges
3. Monthly Charges Distribution
4. Tenure Distribution
5. ROC Curve
6. Predicted Churn Probability
7. Model Diagnostics

## Repository Structure

```
Week-3/
├── Dataset/
│   ├── Telco_Customer_Churn_Week3.csv
│   └── Telco_Customer_Churn_Week3_Cleaned.csv
│
├── R-Script/
│   └── Week3_Statistical_Predictive_Analysis.R
│
├── Screenshots/
│   ├── 01_Monthly_Charges_Boxplot.png
│   ├── 02_Tenure_Total_Charges.png
│   ├── 03_Monthly_Charges_Distribution.png
│   ├── 04_Tenure_Distribution.png
│   ├── 05_ROC_Curve.png
│   ├── 06_Predicted_Churn_Probability.png
│   └── 07_Model_Diagnostics.png
│
└── Report/
    ├── Statistical and model output CSV files
    └── Week3_Telco_Churn_Statistical_Predictive_Analysis_Report.docx
```

## Conclusion

Week 3 extends the Telco Customer Churn project from visualization to statistical inference and predictive modeling. The analysis demonstrates significant relationships between churn and several customer characteristics and develops a logistic regression model evaluated through both cross-validation and an independent test set.

The results provide a statistical and predictive foundation for further churn analysis, threshold optimization, feature engineering, and comparison with alternative machine learning models.

## Author

**Aayan**

B.Tech — Computer Science & Information Technology
