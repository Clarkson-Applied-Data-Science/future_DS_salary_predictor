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
