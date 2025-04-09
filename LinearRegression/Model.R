
set.seed(123)  

# Create stratified train-test split based on 'job_title'
split_index <- createDataPartition(salary_data_not_split$job_title, p = 0.7, list = FALSE)

# Split the data into training and testing sets
salary_data <- salary_data_not_split[split_index, ]
test_data <- salary_data_not_split[-split_index, ]

# Replicate entries in train set to balance job titles
# Calculate the maximum number of instances per job_title
max_per_title <- max(table(salary_data$job_title))

# Replicate rows in the training data to match the max count
salary_data <- salary_data %>%
  group_by(job_title) %>%
  slice(rep(1:n(), length.out = max_per_title)) %>%
  ungroup()

model <- lm(salary ~ job_title + experience_level + employment_type + company_size , data = salary_data)
summary(model)
summary(model$residuals)

plot(model) 
rmse(salary_data$salary, salary_data$predicted_salary)
mae(salary_data$salary, salary_data$predicted_salary)


plot(model) 
rmse(salary_data$salary, salary_data$predicted_salary)
mae(salary_data$salary, salary_data$predicted_salary)



