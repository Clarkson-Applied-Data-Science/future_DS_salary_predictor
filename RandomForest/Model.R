# Load libraries

library(randomForest)

# Set seed for reproducibility
set.seed(123)

# Filter and prepare the dataset: US + Full-time + Entry-level + Before 2025
salary_filtered <- salary_data_no_category %>%
  filter(
    work_year < 2025,
    employee_residence == "US",
    employment_type == "FT",
    experience_level == "EN"
  ) %>%
  mutate(across(where(is.character), as.factor))

# Train the Random Forest model
rf_model <- randomForest(
  salary_in_usd ~ job_title + remote_ratio + company_location + company_size + work_year,
  data = salary_filtered,
  importance = TRUE
)

# Create 2025 data from unique combos
predict_2025 <- salary_filtered %>%
  group_by(job_title, remote_ratio, company_location, company_size) %>%
  summarise(
    experience_level = "EN",
    employment_type = "FT",
    employee_residence = "US",
    work_year = 2025,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.character), as.factor))

# Predict 2025 salaries
predict_2025$predicted_salary <- predict(rf_model, newdata = predict_2025)

# Get the top-paying job
top_salary_row <- predict_2025 %>%
  arrange(desc(predicted_salary)) %>%
  slice(1)

print(top_salary_row)

# Optional: Feature importance plot
importance(rf_model)
varImpPlot(rf_model)
top_5_jobs <- predict_2025 %>%
  arrange(desc(predicted_salary)) %>%
  slice(1:5)
# Plot
ggplot(top_5_jobs, aes(x = reorder(job_title, predicted_salary), y = predicted_salary)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +  # 👈 formats salaries nicely
  labs(
    title = "Top 5 Predicted Highest-Paying Jobs in 2025 (US, FT, Entry-Level)",
    x = "Job Title",
    y = "Predicted Salary (USD)"
  ) +
  theme_minimal()