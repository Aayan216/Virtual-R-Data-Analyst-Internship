# Week 2 — Data Visualization and Insight Communication using R

## Project Overview

Week 2 focuses on transforming customer data into meaningful visual insights using **R, RStudio, and ggplot2**. The project uses the **Telco Customer Churn** dataset to identify patterns and differences in customer churn across contract type, tenure, billing, internet service, payment method, and customer characteristics.

The main objective is to go beyond creating charts by explaining the purpose of each visualization, why the chart type was selected, what the visualization shows, and what the findings may mean from a business perspective.

## Dataset

- **Dataset:** Telco Customer Churn
- **Records:** 7,043 customers
- **Original variables:** 21
- **Target variable:** Churn
- **Source file:** `Telco_Customer_Churn_Week2.csv`
- **Cleaned file:** `Telco_Customer_Churn_Week2_Cleaned.csv`

## Tools and Technologies

- R
- RStudio
- tidyverse
- ggplot2
- janitor
- skimr

## Data Preparation

The dataset was prepared before visualization by:

- Standardizing column names using `janitor::clean_names()`
- Converting `TotalCharges` to numeric format
- Handling missing `TotalCharges` values for zero-tenure customers
- Converting relevant categorical variables to factors
- Creating a numeric churn indicator
- Creating customer tenure groups for lifecycle analysis
- Calculating category-level churn rates

## Visualizations Created

Nine visualizations were created to communicate different aspects of customer churn:

1. **Customer Churn Distribution** — compares the number of customers who stayed and churned.
2. **Churn Rate by Contract Type** — compares churn percentages across month-to-month, one-year, and two-year contracts.
3. **Tenure Distribution by Churn Status** — shows how customer tenure is distributed between churned and retained customers.
4. **Monthly Charges by Churn Status** — compares the distribution of monthly charges for churned and retained customers.
5. **Tenure vs Total Charges** — examines the relationship between customer tenure and accumulated total charges.
6. **Churn Rate by Internet Service** — compares churn rates across DSL, fiber optic, and customers without internet service.
7. **Churn Rate by Payment Method** — compares churn rates across payment methods.
8. **Churn Rate by Customer Tenure Group** — identifies churn differences across customer lifecycle stages.
9. **Churn Rate by Senior Citizen Status** — compares churn between senior and non-senior customers.

## Key Findings

- The overall observed churn rate is approximately **26.54%**.
- **Month-to-month customers** have substantially higher churn than customers on one-year or two-year contracts.
- Customers in the **0–12 month tenure group** have the highest churn rate.
- Churned customers have **higher average monthly charges** than customers who stayed.
- **Fiber optic** customers have the highest churn rate among internet-service categories.
- Customers using **electronic check** have the highest churn rate among payment methods.
- Total charges show a strong upward relationship with customer tenure.
- **Senior customers** have a higher observed churn rate than non-senior customers.

These findings represent descriptive associations in the dataset and do not establish that any single factor directly causes churn.

## Analytical Approach

Each visualization was documented using four elements:

1. **Purpose** — the analytical question being addressed.
2. **Visualization rationale** — why the selected chart type is appropriate.
3. **Key insight** — the main pattern observed in the chart.
4. **Business interpretation** — how the finding could be useful for understanding customer retention.

This approach is intended to make the analysis understandable to both technical and non-technical audiences.

## Project Structure

```text
Week-2/
├── README.md
├── Dataset/
│   ├── Telco_Customer_Churn_Week2.csv
│   └── Telco_Customer_Churn_Week2_Cleaned.csv
├── R-Script/
│   └── Week2_Telco_Visualization.R
├── Screenshots/
│   ├── 01_Churn_Distribution.png
│   ├── 02_Churn_Rate_by_Contract.png
│   ├── 03_Tenure_Distribution_by_Churn.png
│   ├── 04_Monthly_Charges_by_Churn.png
│   ├── 05_Tenure_vs_Total_Charges.png
│   ├── 06_Churn_Rate_by_Internet_Service.png
│   ├── 07_Churn_Rate_by_Payment_Method.png
│   ├── 08_Churn_Rate_by_Tenure_Group.png
│   └── 09_Churn_Rate_by_Senior_Status.png
└── Report/
    └── Week2_Telco_Churn_Visualization_Report.docx
```

## Report

The Week 2 report contains all nine visualizations along with their purpose, visualization rationale, R code, key findings, business interpretation, overall insights, recommendations, and conclusion.

**Report:** `Week2_Telco_Churn_Visualization_Report.docx`

## Conclusion

The Week 2 project demonstrates how R and ggplot2 can be used to transform customer-level data into clear, decision-oriented visual insights. The analysis identifies contract type and customer tenure as particularly important churn-related segments, while billing, internet service, payment method, and demographic characteristics provide additional areas for investigation.
