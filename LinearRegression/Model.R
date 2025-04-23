set.seed(123)
view(salary_data)
# Remove the 'job_title' column from predictors, as it's already encoded
salary_data_encoded <- salary_data %>% dplyr::select(-job_title,)  

model <- lm(salary_in_usd ~ ., data = salary_data_encoded)


# Predict the salaries using the trained model
salary_data$predicted_salary <- predict(model, newdata = salary_data_encoded)

# Plot model diagnostics
par(mfrow = c(2, 2))
plot(model)

if(FALSE){
  # Apply one-hot encoding to the test dataset to make similar to train
  test_salary_data_one_hot <- predict(dummy, newdata = test_data)
  test_salary_data_one_hot <- data.frame(test_salary_data_one_hot)
  test_salary_data_combined <- cbind(test_data, test_salary_data_one_hot)
  test_data <- test_salary_data_combined[, !names(test_salary_data_combined) %in% 
                                           c("work_year", "experience_level", "employment_type",  "remote_ratio", "company_size","job_category")]
}

#Scaling the test data 
test_data <- test_data %>%
  mutate(salary_in_usd = (salary_in_usd - min_salary) / (max_salary - min_salary))

#Test the model
test_data$predicted_salary <- predict(model, newdata = test_data)

#Changing it back to normal scale for viewing
test_data$salary_in_usd <- (test_data$salary_in_usd * (max_salary - min_salary)) + min_salary
test_data$predicted_salary_original <- (test_data$predicted_salary * (max_salary - min_salary)) + min_salary
view(test_data)

# Calculate RMSE and MAE
rmse_value <- rmse(test_data$salary_in_usd, test_data$predicted_salary_original)
mae_value <- mae(test_data$salary_in_usd, test_data$predicted_salary_original)

rss <- sum((test_data$salary_in_usd - test_data$predicted_salary_original)^2)  # Residual Sum of Squares
tss <- sum((test_data$salary_in_usd - mean(test_data$salary_in_usd))^2)        # Total Sum of Squares

r_squared <- 1 - rss/tss
print(paste("RMSE:", rmse_value))
print(paste("MAE:", mae_value))
print(paste("R2:", r_squared))
