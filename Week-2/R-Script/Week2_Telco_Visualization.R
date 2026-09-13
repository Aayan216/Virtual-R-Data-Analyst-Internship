library(tidyverse)
library(janitor)
library(skimr)

base_path <- "D:/Virtual R Data Analyst Intern/Week-2"

dataset_path <- file.path(
  base_path,
  "Dataset",
  "Telco_Customer_Churn_Week2.csv"
)

dataset_output <- file.path(
  base_path,
  "Dataset",
  "Telco_Customer_Churn_Week2_Cleaned.csv"
)

screenshot_path <- file.path(
  base_path,
  "Screenshots"
)

if (!dir.exists(screenshot_path)) {
  dir.create(screenshot_path, recursive = TRUE)
}

data <- read.csv(
  dataset_path,
  stringsAsFactors = FALSE
)

data <- data %>%
  clean_names()

data$total_charges <- as.numeric(data$total_charges)

data$total_charges[
  is.na(data$total_charges) & data$tenure == 0
] <- 0

data <- data %>%
  mutate(
    gender = as.factor(gender),
    senior_citizen = as.factor(senior_citizen),
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

data <- data %>%
  mutate(
    churn_numeric = ifelse(churn == "Yes", 1, 0),
    tenure_group = cut(
      tenure,
      breaks = c(-1, 12, 24, 48, 60, Inf),
      labels = c(
        "0-12 Months",
        "13-24 Months",
        "25-48 Months",
        "49-60 Months",
        "60+ Months"
      )
    )
  )

write.csv(
  data,
  dataset_output,
  row.names = FALSE
)

p1 <- ggplot(data, aes(x = churn)) +
  geom_bar() +
  labs(
    title = "Customer Churn Distribution",
    x = "Churn Status",
    y = "Number of Customers"
  ) +
  theme_minimal()

p1

ggsave(
  file.path(screenshot_path, "01_Churn_Distribution.png"),
  p1,
  width = 8,
  height = 5,
  dpi = 300
)

p2 <- data %>%
  count(contract, churn) %>%
  group_by(contract) %>%
  mutate(
    churn_rate = n / sum(n) * 100
  ) %>%
  filter(churn == "Yes") %>%
  ggplot(
    aes(
      x = contract,
      y = churn_rate
    )
  ) +
  geom_col() +
  labs(
    title = "Churn Rate by Contract Type",
    x = "Contract Type",
    y = "Churn Rate (%)"
  ) +
  theme_minimal()

p2

ggsave(
  file.path(screenshot_path, "02_Churn_Rate_by_Contract.png"),
  p2,
  width = 8,
  height = 5,
  dpi = 300
)

p3 <- ggplot(
  data,
  aes(x = tenure, fill = churn)
) +
  geom_histogram(
    bins = 30,
    position = "identity",
    alpha = 0.6
  ) +
  labs(
    title = "Tenure Distribution by Churn Status",
    x = "Tenure (Months)",
    y = "Number of Customers"
  ) +
  theme_minimal()

p3

ggsave(
  file.path(screenshot_path, "03_Tenure_Distribution_by_Churn.png"),
  p3,
  width = 8,
  height = 5,
  dpi = 300
)

p4 <- ggplot(
  data,
  aes(x = churn, y = monthly_charges)
) +
  geom_boxplot() +
  labs(
    title = "Monthly Charges by Churn Status",
    x = "Churn Status",
    y = "Monthly Charges"
  ) +
  theme_minimal()

p4

ggsave(
  file.path(screenshot_path, "04_Monthly_Charges_by_Churn.png"),
  p4,
  width = 8,
  height = 5,
  dpi = 300
)

p5 <- ggplot(
  data,
  aes(
    x = tenure,
    y = total_charges,
    color = churn
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Tenure and Total Charges",
    x = "Tenure (Months)",
    y = "Total Charges"
  ) +
  theme_minimal()

p5

ggsave(
  file.path(screenshot_path, "05_Tenure_vs_Total_Charges.png"),
  p5,
  width = 8,
  height = 5,
  dpi = 300
)

p6 <- data %>%
  count(internet_service, churn) %>%
  group_by(internet_service) %>%
  mutate(
    churn_rate = n / sum(n) * 100
  ) %>%
  filter(churn == "Yes") %>%
  ggplot(
    aes(
      x = internet_service,
      y = churn_rate
    )
  ) +
  geom_col() +
  labs(
    title = "Churn Rate by Internet Service",
    x = "Internet Service",
    y = "Churn Rate (%)"
  ) +
  theme_minimal()

p6

ggsave(
  file.path(screenshot_path, "06_Churn_Rate_by_Internet_Service.png"),
  p6,
  width = 8,
  height = 5,
  dpi = 300
)

p7 <- data %>%
  count(payment_method, churn) %>%
  group_by(payment_method) %>%
  mutate(
    churn_rate = n / sum(n) * 100
  ) %>%
  filter(churn == "Yes") %>%
  ggplot(
    aes(
      x = reorder(payment_method, churn_rate),
      y = churn_rate
    )
  ) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Churn Rate by Payment Method",
    x = "Payment Method",
    y = "Churn Rate (%)"
  ) +
  theme_minimal()

p7

ggsave(
  file.path(screenshot_path, "07_Churn_Rate_by_Payment_Method.png"),
  p7,
  width = 9,
  height = 6,
  dpi = 300
)

p8 <- data %>%
  count(tenure_group, churn) %>%
  group_by(tenure_group) %>%
  mutate(
    churn_rate = n / sum(n) * 100
  ) %>%
  filter(churn == "Yes") %>%
  ggplot(
    aes(
      x = tenure_group,
      y = churn_rate
    )
  ) +
  geom_col() +
  labs(
    title = "Churn Rate by Customer Tenure Group",
    x = "Tenure Group",
    y = "Churn Rate (%)"
  ) +
  theme_minimal()

p8

ggsave(
  file.path(screenshot_path, "08_Churn_Rate_by_Tenure_Group.png"),
  p8,
  width = 9,
  height = 6,
  dpi = 300
)

p9 <- data %>%
  count(senior_citizen, churn) %>%
  group_by(senior_citizen) %>%
  mutate(
    churn_rate = n / sum(n) * 100
  ) %>%
  filter(churn == "Yes") %>%
  mutate(
    senior_citizen = recode(
      senior_citizen,
      `0` = "Non-Senior",
      `1` = "Senior"
    )
  ) %>%
  ggplot(
    aes(
      x = senior_citizen,
      y = churn_rate
    )
  ) +
  geom_col() +
  labs(
    title = "Churn Rate by Senior Citizen Status",
    x = "Customer Group",
    y = "Churn Rate (%)"
  ) +
  theme_minimal()

p9

ggsave(
  file.path(screenshot_path, "09_Churn_Rate_by_Senior_Status.png"),
  p9,
  width = 8,
  height = 5,
  dpi = 300
)

contract_summary <- data %>%
  group_by(contract) %>%
  summarise(
    customers = n(),
    churned = sum(churn == "Yes"),
    churn_rate = mean(churn == "Yes") * 100,
    average_monthly_charges = mean(monthly_charges),
    average_tenure = mean(tenure),
    .groups = "drop"
  )

internet_summary <- data %>%
  group_by(internet_service) %>%
  summarise(
    customers = n(),
    churned = sum(churn == "Yes"),
    churn_rate = mean(churn == "Yes") * 100,
    average_monthly_charges = mean(monthly_charges),
    .groups = "drop"
  )

payment_summary <- data %>%
  group_by(payment_method) %>%
  summarise(
    customers = n(),
    churned = sum(churn == "Yes"),
    churn_rate = mean(churn == "Yes") * 100,
    .groups = "drop"
  )

tenure_summary <- data %>%
  group_by(tenure_group) %>%
  summarise(
    customers = n(),
    churned = sum(churn == "Yes"),
    churn_rate = mean(churn == "Yes") * 100,
    .groups = "drop"
  )

charge_summary <- data %>%
  group_by(churn) %>%
  summarise(
    customers = n(),
    average_monthly_charges = mean(monthly_charges),
    median_monthly_charges = median(monthly_charges),
    average_total_charges = mean(total_charges),
    average_tenure = mean(tenure),
    .groups = "drop"
  )

cat("\n================ DATASET OVERVIEW ================\n")
cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")

cat("\n================ CONTRACT SUMMARY ================\n")
print(contract_summary)

cat("\n================ INTERNET SERVICE SUMMARY ================\n")
print(internet_summary)

cat("\n================ PAYMENT METHOD SUMMARY ================\n")
print(payment_summary)

cat("\n================ TENURE SUMMARY ================\n")
print(tenure_summary)

cat("\n================ CHARGE SUMMARY ================\n")
print(charge_summary)

cat("\n================ CHURN SUMMARY ================\n")
print(table(data$churn))

cat("\n================ MISSING VALUES ================\n")
print(sum(is.na(data)))

cat("\n================ DUPLICATE ROWS ================\n")
print(sum(duplicated(data)))

cat("\n================ OVERALL CHURN RATE ================\n")
print(mean(data$churn == "Yes") * 100)

cat("\n================ AVERAGE MONTHLY CHARGES ================\n")

print(
  data %>%
    group_by(churn) %>%
    summarise(
      average_monthly_charges = mean(monthly_charges),
      .groups = "drop"
    )
)