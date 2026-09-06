library(tidyverse)
library(janitor)
library(skimr)

setwd("D:/Virtual R Data Analyst Intern/Week-1/Dataset")
data <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

head(data)
dim(data)
names(data)
str(data)
summary(data)
skim(data)

missing_values <- colSums(is.na(data))
print(missing_values)

blank_values <- sapply(data, function(x) sum(trimws(as.character(x)) == ""))
print(blank_values)

data <- data %>%
  mutate(across(where(is.character), ~na_if(trimws(.), "")))

data <- data %>% distinct()

names(data)[names(data) == "TotalCharges"] <- "total_charges"
data$total_charges <- as.numeric(data$total_charges)
data$total_charges <- ifelse(is.na(data$total_charges) & data$tenure == 0, 0, data$total_charges)

data <- clean_names(data)

data <- data %>%
  mutate(
    gender = as.factor(gender),
    partner = as.factor(partner),
    dependents = as.factor(dependents),
    phone_service = as.factor(phone_service),
    multiple_lines = as.factor(multiple_lines),
    internet_service = as.factor(internet_service),
    online_security = as.factor(online_security),
    online_backup = as.factor(online_backup),
    device_protection = as.factor(device_protection),
    tech_support = as.factor(tech_support),
    streaming_tv = as.factor(streaming_tv),
    streaming_movies = as.factor(streaming_movies),
    contract = as.factor(contract),
    paperless_billing = as.factor(paperless_billing),
    payment_method = as.factor(payment_method),
    churn = as.factor(churn)
  )

summary_stats <- data %>%
  summarise(
    mean_tenure = mean(tenure, na.rm = TRUE),
    median_tenure = median(tenure, na.rm = TRUE),
    sd_tenure = sd(tenure, na.rm = TRUE),
    mean_monthly_charges = mean(monthly_charges, na.rm = TRUE),
    median_monthly_charges = median(monthly_charges, na.rm = TRUE),
    sd_monthly_charges = sd(monthly_charges, na.rm = TRUE),
    mean_total_charges = mean(total_charges, na.rm = TRUE),
    median_total_charges = median(total_charges, na.rm = TRUE),
    sd_total_charges = sd(total_charges, na.rm = TRUE)
  )

print(summary_stats)

table(data$gender)
table(data$contract)
table(data$internet_service)
table(data$churn)
prop.table(table(data$churn)) * 100

monthly_by_churn <- data %>%
  group_by(churn) %>%
  summarise(average_monthly_charges = mean(monthly_charges, na.rm = TRUE))
print(monthly_by_churn)

tenure_by_churn <- data %>%
  group_by(churn) %>%
  summarise(average_tenure = mean(tenure, na.rm = TRUE))
print(tenure_by_churn)

contract_churn_rate <- data %>%
  group_by(contract) %>%
  summarise(
    customers = n(),
    churned = sum(churn == "Yes"),
    churn_rate = churned / customers * 100
  )
print(contract_churn_rate)

library(ggplot2)

ggplot(data, aes(x = churn)) + geom_bar() +
  labs(title = "Customer Churn Distribution", x = "Churn Status", y = "Number of Customers") +
  theme_minimal()

ggplot(data, aes(x = contract)) + geom_bar() +
  labs(title = "Customer Distribution by Contract Type", x = "Contract Type", y = "Number of Customers") +
  theme_minimal()

ggplot(data, aes(x = monthly_charges)) + geom_histogram(bins = 30) +
  labs(title = "Distribution of Monthly Charges", x = "Monthly Charges", y = "Number of Customers") +
  theme_minimal()

ggplot(data, aes(x = tenure)) + geom_histogram(bins = 30) +
  labs(title = "Distribution of Customer Tenure", x = "Tenure (Months)", y = "Number of Customers") +
  theme_minimal()

ggplot(data, aes(x = churn, y = monthly_charges)) + geom_boxplot() +
  labs(title = "Monthly Charges by Churn Status", x = "Churn Status", y = "Monthly Charges") +
  theme_minimal()

ggplot(data, aes(x = contract, fill = churn)) + geom_bar(position = "fill") +
  labs(title = "Churn Proportion by Contract Type", x = "Contract Type", y = "Proportion", fill = "Churn") +
  theme_minimal()

cat("\nFinal missing values:\n")
print(colSums(is.na(data)))
cat("\nFinal duplicate rows:\n")
print(sum(duplicated(data)))
cat("\nFinal dimensions:\n")
print(dim(data))
cat("\nWEEK 1 ANALYSIS COMPLETED SUCCESSFULLY\n")
