# Week 4 — Comprehensive Data Analysis

## Project Overview

Week 4 brings together the complete **customer churn analysis workflow** using R and RStudio, from data preparation and visualization to statistical analysis and predictive modeling.

## Objectives

- Prepare and validate the customer churn dataset
- Analyze important churn patterns
- Perform statistical and correlation analysis
- Build a logistic regression model
- Evaluate model performance
- Present findings and practical insights

## Dataset

**Telco Customer Churn Dataset**

- 7,043 customer records
- Customer demographics, services, contract, payment, tenure, and charges
- Target variable: **Churn**

## Analysis Performed

- Data cleaning and validation
- Exploratory data analysis
- Churn analysis by contract, tenure, internet service, payment method, and customer group
- Hypothesis testing
- Correlation analysis
- Multicollinearity analysis using VIF
- Logistic regression
- 80/20 train-test split
- 5-fold cross-validation
- Confusion matrix
- ROC curve and AUC
- Model diagnostics

## Key Results

- Overall churn rate: **26.54%**
- Month-to-month contract churn: **42.7%**
- One-year contract churn: **11.3%**
- Two-year contract churn: **2.8%**
- Highest churn among internet services: **Fiber optic — 41.9%**
- Highest churn among payment methods: **Electronic check — 45.3%**
- Test accuracy: **79.8%**
- Test sensitivity: **51.2%**
- Test specificity: **90.1%**
- Test ROC-AUC: **0.834**
- 5-fold cross-validation ROC: **0.841**

## Tools & Technologies

- R
- RStudio
- tidyverse
- janitor
- skimr
- ggplot2
- caret
- pROC
- broom
- car

## Repository Structure

\`\`\`text
Week-4/
├── Dataset/
├── R-Script/
├── Screenshots/
├── Report/
└── README.md
\`\`\`

## Outcome

The project demonstrates a complete data analytics and predictive modeling workflow:

**Data Preparation → Visualization → Statistical Analysis → Predictive Modeling → Evaluation → Interpretation**

## Author

**Aayan**  
B.Tech — Computer Science & Information Technology
