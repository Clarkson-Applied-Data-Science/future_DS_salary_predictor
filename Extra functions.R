#Convert non-numeric columns to factors and run the ANOVA
```{r} 
salary_data_not_split$salary <- as.numeric(salary_data_not_split$salary)

non_numeric_cols <- sapply(salary_data_not_split, function(x) !is.numeric(x))

# Remove factors with only one level
salary_data_not_split <- salary_data_not_split[, sapply(salary_data_not_split, function(x) length(unique(x)) > 1)]

salary_data_not_split[non_numeric_cols] <- lapply(salary_data_not_split[non_numeric_cols], as.factor)

factor_names <- names(salary_data_not_split)[non_numeric_cols]
formula_str <- paste("salary ~", paste(factor_names, collapse = " + "))
anova_formula <- as.formula(formula_str)

anova_model <- aov(anova_formula, data = salary_data_not_split)
summary(anova_model)
```

#Plots the average salary per job title with 95% confidence intervals.
```{r}
top_jobs <- names(sort(table(salary_data_not_split$job_title), decreasing = TRUE)[1:10])
filtered_data <- salary_data_not_split[salary_data_not_split$job_title %in% top_jobs, ]

anova_model <- aov(salary ~ job_title, data = filtered_data)
emm <- emmeans(anova_model, ~ job_title)
plot(emm)
``` 