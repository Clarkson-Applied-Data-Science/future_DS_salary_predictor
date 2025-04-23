set.seed(123)

# Remove the 'job_title' column from predictors, as it's already encoded
salary_data_encoded <- salary_data %>% dplyr::select(-job_title,)  

model <- lm(salary_in_usd ~ ., data = salary_data_encoded)
summary(model)
summary(model$residuals)

# Predict the salaries using the trained model
salary_data$predicted_salary <- predict(model, newdata = salary_data_encoded)

# Plot model diagnostics
par(mfrow = c(2, 2))
plot(model)

# Apply one-hot encoding to the dataset
test_salary_data_one_hot <- predict(dummy, newdata = test_data)
test_salary_data_one_hot <- data.frame(test_salary_data_one_hot)
test_salary_data_combined <- cbind(test_data, test_salary_data_one_hot)
test_data <- test_salary_data_combined[, !names(test_salary_data_combined) %in% c("work_year", "experience_level", "employment_type",  "remote_ratio", "company_size","job_category")]

test_data <- test_data %>%
  mutate(salary_in_usd = (salary_in_usd - min_salary) / (max_salary - min_salary))




#Test the model

test_data$predicted_salary <- predict(model, newdata = test_data)

test_data$salary_in_usd <- (test_data$salary_in_usd * (max_salary - min_salary)) + min_salary
test_data$predicted_salary_original <- (test_data$predicted_salary * (max_salary - min_salary)) + min_salary
head(test_data[, c("job_title", "salary_in_usd", "predicted_salary_original")])



# Calculate RMSE and MAE
rmse_value <- rmse(test_data$salary_in_usd, test_data$predicted_salary_original)
mae_value <- mae(test_data$salary_in_usd, test_data$predicted_salary_original)
#r_squared <- rsq(test_data$salary_in_usd, test_data$predicted_salary_original)

rss <- sum((test_data$salary_in_usd - test_data$predicted_salary_original)^2)  # Residual Sum of Squares
tss <- sum((test_data$salary_in_usd - mean(test_data$salary_in_usd))^2)        # Total Sum of Squares

r_squared <- 1 - rss/tss
print(r_squared)

# Print RMSE and MAE
print(paste("RMSE:", rmse_value))
print(paste("MAE:", mae_value))
#print(paste("R2:", r_squared))
