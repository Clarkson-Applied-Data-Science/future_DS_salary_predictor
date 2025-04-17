set.seed(123)

salary_filtered <- salary_data_no_category %>%
  filter(
    work_year < 2025,
    employee_residence == "US",
    employment_type == "FT",
    experience_level == "EN"
  ) %>%
  dplyr::select(-salary, -salary_currency) %>%   
  mutate(across(where(is.character), as.factor))
salary_filtered <- salary_filtered %>%
  mutate(remote_ratio = case_when(
    remote_ratio == 0 ~ "On-site",
    remote_ratio == 50 ~ "Hybrid",
    remote_ratio == 100 ~ "Remote",
    TRUE ~ "Other"
  ))
salary_filtered <- salary_filtered %>%
  mutate(
    year_bucket = case_when(
      work_year <= 2020 ~ "Pre-COVID",
      work_year == 2021 ~ "During-COVID",
      work_year >= 2022 ~ "Post-COVID"
    ) %>% factor(levels = c("Pre-COVID", "During-COVID", "Post-COVID"))
  )

Q1 <- quantile(salary_filtered$salary_in_usd, 0.25)
Q3 <- quantile(salary_filtered$salary_in_usd, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR


salary_filtered <- salary_filtered %>%
  filter(salary_in_usd >= lower_bound & salary_in_usd <= upper_bound)

train_index <- createDataPartition(salary_filtered$salary_in_usd, p = 0.8, list = FALSE)
train_data <- salary_filtered[train_index, ]
test_data  <- salary_filtered[-train_index, ]

model <- rpart(salary_in_usd ~ ., data = train_data,
 control = rpart.control(cp = 0.001, minsplit = 5, maxdepth = 5)
)


test_data$predicted_salary <- predict(model, newdata = test_data)

mse <- mean((test_data$predicted_salary - test_data$salary_in_usd)^2)
rmse <- sqrt(mse)
r2 <- 1 - sum((test_data$predicted_salary - test_data$salary_in_usd)^2) / 
  sum((mean(train_data$salary_in_usd) - test_data$salary_in_usd)^2)

cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", r2, "\n")
salary_filtered




