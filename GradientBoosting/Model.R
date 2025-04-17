set.seed(123)

salary_data_encoded <- salary_data %>% dplyr::select(-job_title,)  
model <- gbm(
  formula = salary_in_usd ~ .,     # Target variable is salary_in_usd
  data = salary_data_encoded,                # Training data
  distribution = "gaussian",       # For regression tasks
  n.trees = 500,                  # Number of trees
  interaction.depth = 5,           # Max depth of trees
  shrinkage = 0.1,                 # Learning rate
  cv.folds = 5,                    # Cross-validation folds
  verbose = TRUE                   # Print progress
)

summary(model)
summary(model$residuals)

# Predict the salaries using the trained model
salary_data$predicted_salary <- predict(model, newdata = salary_data_encoded)

# Plot model diagnostics
par(mfrow = c(2, 2))
plot(model)