---
title: "What Determines Employee Salaries: A Regression Analysis Study "
author: "Neeraj Namani & Srisailam Gitte"
date: "`r Sys.Date()`"
output:
  pdf_document: default
---





```{r}
# Read the data from Salary data csv file
data1 <- read.csv("~/Downloads/Salary Data.csv")
```

Exploratory Data Analysis:

```{r}
# Display the first few rows of the data
head(data1)
```

```{r}
# Display the last few rows of the data
tail(data1)
```

```{r}
# Display the shape of the dataset
dim(data1)
```

```{r}
# Check for missing values
colSums(is.na(data1))
```

```{r}
cleaned_data <- na.omit(data1)
nrow(cleaned_data)
```

```{r}
# Re check for missing values in the data
colSums(is.na(cleaned_data))
```

```{r}
# Data Frame
str(cleaned_data)
```

```{r}
# Load the necessary libraries
library(dplyr)
# Dataset Structure
glimpse(cleaned_data)
```

```{r}
# Generate Summary statistics for the data
summary(cleaned_data)
```

```{r}
# Load the required libraries for EDA
library(dplyr) # For data manipulation
```

```{r}
library(ggplot2) # For data visualization
```

```{r}
# Implement Pairwise Scatterplot
pairs(cleaned_data[sapply(cleaned_data,is.numeric)])
```

```{r}
library(GGally)
library(ggplot2)
```


```{r}
# Use ggpairs to plot numerical variables
ggpairs(cleaned_data[sapply(cleaned_data,is.numeric)])
```



```{r}
# Define a function to plot histograms for numeric columns
plot_numeric_histograms <- function(cleaned_data) {
  par(mfrow = c(1,3)) # Set up a 1x3 grid for plotting
  
  # Get the names of numeric columns
  numeric_cols <- names(cleaned_data)[sapply(cleaned_data, is.numeric)]
  
  # Iterate through numeric columns and create histograms
  for (col_name in numeric_cols) {
    hist(cleaned_data[[col_name]], main = col_name, xlab = col_name)
  }
}
# Call the function with my dataset
plot_numeric_histograms(cleaned_data)
```

```{r}
# Set up the layout for the boxplots
par(mfrow = c(1,3)) # 1 row and 3 columns
# Get the names of numeric columns
numeric_cols <- names(cleaned_data)[sapply(cleaned_data, is.numeric)]
# Iterate through numeric columns and create boxplots
for (col_name in numeric_cols) {
  boxplot(cleaned_data[[col_name]], main = col_name, xlab = col_name)
}

# Reset the layout to the default
```
```{r}
library(ggplot2)

# Create a scatter plot with Age on the x-axis and Salary on the y-axis
ggplot(cleaned_data, aes(x = Age, y = log(Salary), color = Gender)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter plot of Age vs Salary", x = "Age", y = "log(Salary)")
```
```{r}
# Create a scatterplot with years of experience on the x-axis, Salary on the y-axis and Color by Education level
ggplot(cleaned_data, aes(x = `Years.of.Experience`,y = Salary, color = `Education.Level`)) +
  geom_point()+
  theme_minimal() +
  labs(title = "Scatter plot of Experience Level vs Salary", x = "Experience", y = "Salary")
```


```{r}
# Bar plot for Gender Frequency
ggplot(cleaned_data, aes(x = Gender)) + 
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal() +
  ggtitle("Frequency of Gender") +
  xlab("Gender") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
# Bar plot for Education Frequency
ggplot(cleaned_data, aes(x = Education.Level)) +
  geom_bar(fill = "coral", color = "black") +
  theme_minimal() +
  ggtitle("Frequency of Education Level") +
  xlab("Education Level") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
# Box plot of Salary by Education Level
ggplot(cleaned_data, aes(x = Education.Level, y = Salary)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Salary Distribution by Education Level") +
  xlab("Education Level") +
  ylab("Salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
# Let us calculate the average salary for each gender
average_salary_by_gender <- cleaned_data %>%
  group_by(Gender) %>%
  summarise(Average_Salary = mean(Salary, na.rm = TRUE))

# Print the results
print(average_salary_by_gender)
```

```{r}
# Bar plot for Average Salary by Gender
ggplot(average_salary_by_gender, aes(x = Gender, y = Average_Salary, fill = Gender)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() + 
  ggtitle("Average Salary by Gender") +
  xlab("Gender") +
  ylab("Average Salary") 
```

```{r}
# Calculate the average salary for different education levels
average_salary_by_education <- cleaned_data %>%
  group_by(Education.Level) %>%
  summarise(Average_Salary = mean(Salary, na.rm = TRUE))

print(average_salary_by_education)
```

```{r}
ggplot(average_salary_by_education, aes(x = Education.Level, y = Average_Salary, fill = Education.Level)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  ggtitle("Average Salary by Education Level") +
  xlab("Education Level") +
  ylab("Average Salary") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
```

```{r}
# Load the necessary library
library(dplyr)

# Assuming your data is already loaded into a data frame called `data`

# Define the breaks and labels for the experience categories
exp_breaks <- c(-Inf, 5, 10, 15, 20, Inf)
exp_labels <- c("0-5 years", "6-10 years", "11-15 years", "16-20 years", "20+ years")

# Group years of experience into categories and calculate the average salary
salary_by_exp <- cleaned_data %>%
  mutate(Experience_Category = cut(Years.of.Experience, breaks = exp_breaks, labels = exp_labels, right = FALSE)) %>%
  group_by(Experience_Category) %>%
  summarise(Average_Salary = mean(Salary, na.rm = TRUE)) %>%
  arrange(desc(Average_Salary))

# Format the average salary as currency
salary_by_exp$Average_Salary <- scales::dollar(salary_by_exp$Average_Salary)

# Print the result
print(salary_by_exp)

```

```{r}
# Load the necessary library
library(ggplot2)
library(dplyr)

# Assuming your data is already loaded into a variable called `data`

# Categorize 'Years of Experience' and compute the average salary for each category
salary_by_exp <- cleaned_data %>%
  mutate(Experience_Category = cut(Years.of.Experience,
                                   breaks = c(-Inf, 5, 10, 15, 20, Inf),
                                   labels = c("0-5 years", "6-10 years", "11-15 years", "16-20 years", "20+ years"),
                                   right = FALSE)) %>%
  group_by(Experience_Category) %>%
  summarise(Average_Salary = mean(Salary, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Average_Salary))

# Create a simple bar plot
ggplot(salary_by_exp, aes(x = Experience_Category, y = Average_Salary, fill = Experience_Category)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(title = "Average Salary by Years of Experience",
       x = "Years of Experience",
       y = "Average Salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x labels for readability
        plot.title = element_text(hjust = 0.5)) # Center the plot title


```

```{r}
library(reshape2)

# Create a correlation matrix for your data
corr_matrix <- cor(cleaned_data[sapply(cleaned_data,is.numeric)])

# Reduce the size of the correlation matrix
melted_corr_matrix <- melt(corr_matrix)

# Create a correlation heatmap using ggplot2
ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient(low = "grey40", high = "grey80")+
  labs(title = "Correlation Heatmap")
```

```{r}
# Install and load the fastDummies library if not already installed
# install.packages("fastDummies")
library(fastDummies)

# Encode the 'Education Level' categorical data
df_encoded <- dummy_cols(cleaned_data, select_columns = "Education.Level", remove_first_dummy = TRUE)

# View the first few rows of the new data frame
head(df_encoded)

```

```{r}
# Perform simple linear Regression
full_model <- lm(Salary ~ ., data = cleaned_data)
summary(full_model)
```

```{r}
# Sample Data Creation
# Let's assume you have a column 'Job.Title' and 'Actual_Salary' in your dataset
data <- data.frame(
  Job.Title = c("Senior", "CEO", "Junior", "HR", "Manager"),
  Actual_Salary = c(50000, 150000, 45000, 70000, 60000)
)

# Categorizing job titles
data$Job.Category <- ifelse(grepl("Senior", data$Job.Title), "Senior",
                        ifelse(grepl("CEO|Executive", data$Job.Title), "CEO",
                        ifelse(grepl("Junior", data$Job.Title), "Junior",
                        ifelse(grepl("HR", data$Job.Title), "HR",
                        ifelse(grepl("Manager", data$Job.Title), "Manager", "Other")))))

# Fit the model (using a hypothetical cleaned data)
# model <- lm(Actual_Salary ~ Job.Category, data = data)

# For demonstration, let's create some predicted salaries based on arbitrary coefficients
data$Predicted_Salary <- with(data, ifelse(Job.Category == "Senior", 55000,
                                  ifelse(Job.Category == "CEO", 155000,
                                  ifelse(Job.Category == "Junior", 40000,
                                  ifelse(Job.Category == "HR", 65000,
                                  ifelse(Job.Category == "Manager", 65000, 50000))))))

# Calculate the error
data$Error <- with(data, Actual_Salary - Predicted_Salary)

# Print the data frame
print(data[, c("Job.Title", "Actual_Salary", "Predicted_Salary", "Error")])

```
Now, let us remove the `Job.Title` from the full model as it contains many categories and let us see how much variance is captured without the `Job.Title` in the next model (simple_model)

```{r}
simple_model <- lm(Salary ~ Age + Years.of.Experience + Education.Level + Gender, data = cleaned_data)
summary(simple_model)
```
Now, this model will be our first model since it captures maximum variance without the Job.title column and can be helpful in salary determining.

Intepretation: 

1. Model Formula: The linear model predicts `Salary` using `Age`, `Years of Experience`, `Education Level`, and `Gender`.
2. Coefficients: Significant predictors include `Age`, `Years of Experience`, and higher levels of `Education`, with higher education and age associated with increased salary. Being male also predicts a higher salary.
3. Model Fit: The model explains approximately 90.3% of the variance in salaries, indicating a strong fit.
4. Statistical Significance: The model is statistically significant (F-statistic: 683.1, p < 2.2e-16), affirming the reliability of the predictors.


```{r}
# Log transformations
log_model <- lm(log(Salary) ~ Age + Years.of.Experience + Education.Level + Gender, data = cleaned_data)
summary(log_model)
```
Interpretation:

1. Model Formula: The model uses the natural logarithm of `Salary` as a dependent variable, regressed on `Age`, `Years of Experience`, `Education Level`, and `Gender`.
2. Coefficients and Significance: Positive coefficients for `Age`, `Years of Experience`, both higher education levels, and `GenderMale` indicate their respective contributions to higher salary logarithm values, with all but `Years of Experience` and `GenderMale` showing strong statistical significance.
3. Model Fit: The model accounts for approximately 71.59% of the variability in the logarithmic salary (Adjusted R-squared: 0.7121), suggesting a good fit.
4. Statistical Significance of Model: With an F-statistic of 185 and a highly significant p-value (p < 2.2e-16), the overall model fit is statistically significant, validating the effectiveness of these predictors in explaining salary variations.

```{r}
poly_model <- lm(poly(Salary,2) ~ Age + Years.of.Experience + Education.Level + Gender, data = cleaned_data)
summary(poly_model)
```

Interpretation:

Response 1 Interpretation: $\\$
1. The model uses `Age`, `Years of Experience`, `Education Level`, and `Gender` to predict an unnamed variable (`1`), explaining 90.3% of its variance. $\\$
2. All predictors are statistically significant, with positive effects on the response variable, suggesting increasing age, experience, higher education, and being male are associated with higher values of `1`. $\\$
3. The model is highly statistically significant (p < 2.2e-16), indicating reliable predictors. 

Response 2 Interpretation: $\\$
1. A different model uses the same predictors for another unnamed variable (`2`), but only explains 3.78% of its variance, indicating a weak model fit. $\\$
2. `Age` and `Years of Experience` significantly influence `2`, with age negatively affecting it and experience positively affecting it.$\\$
3. `Education Level` and `Gender` are not statistically significant, suggesting they do not impact the response variable in this model context. $\\$
4. Despite low explanatory power, the model overall is statistically significant (p = 0.01431), suggesting some relationship between the variables and response `2`. 


```{r}
interaction_model <- lm(Salary ~ Age * Years.of.Experience + Education.Level + Gender, data = cleaned_data)
summary(interaction_model)
```
Interpretation:

1. Model Details: The linear regression model predicts `Salary` using `Age`, `Years of Experience`, their interaction (`Age * Years of Experience`), `Education Level`, and `Gender`. $\\$
2. Significance and Coefficients: `Age`, `Years of Experience`, `Education Level` (both Master's and PhD), and `GenderMale` are significant predictors with positive coefficients, indicating their positive impact on `Salary`. $\\$
3. Interaction Term: The interaction term (`Age:Years.of.Experience`) is not significant (p = 0.429839), suggesting that the combined effect of age and years of experience does not significantly differ from their individual effects on salary. $\\$
4. Model Fit: The model explains 90.31% of the variability in `Salary` (Adjusted R-squared: 0.9015) and is statistically significant (p < 2.2e-16), indicating a strong fit to the data. 


```{r}
par(mfrow = c(2,2))
plot(simple_model)
```



```{r}
# Assuming your encoded dataframe is named df_encoded

# Selecting features by dropping specific columns
X <- df_encoded[, !(names(df_encoded) %in% c("Job Title", "Salary", "Gender"))]

# Selecting the target variable
y <- df_encoded[["Salary"]]

```

```{r}
head(X)
```

```{r}
# Load the caret package
library(caret)
library(lattice)
# Assuming your features and target variable are stored in X and y respectively

# Set seed for reproducibility
set.seed(90)

# Split the data
trainIndex <- createDataPartition(y, p = .8, 
                                  list = FALSE, 
                                  times = 1)

X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

```

```{r}
# Assuming df_encoded is your dataframe from which you want to exclude 'Job.Title', 'Salary', and 'Gender' for features (X)
# and use 'Salary' as the target variable (y)

# Selecting features excluding 'Job Title', 'Salary', and 'Gender'
X <- subset(df_encoded, select = -c(Job.Title, Salary, Gender))

# Selecting the target variable 'Salary'
y <- df_encoded$Salary

# Set seed for reproducibility
set.seed(30)

# Define training control for 10-fold cross-validation
train_control1 <- trainControl(method = "cv", number = 10)

# Train the model using linear regression with 10-fold cross-validation
model1 <- train(x = X, y = y, method = "lm", trControl = train_control1)

# Print the results
print(model1)


```
Interpretation:

1. Dataset and Method: The model used 373 samples and 5 predictors, evaluated using 10-fold cross-validation, ensuring robust estimates by training on various subsets of the data. $\\$
2. Performance Metrics: The model achieved an RMSE (Root Mean Squared Error) of 15,658.61, an R-squared of 0.897, and an MAE (Mean Absolute Error) of 11,291.03, indicating strong predictive accuracy and consistency. $\\$
3. Model Consistency: High R-squared value suggests that about 89.7% of the variance in the dependent variable is predictable from the independent variables. $\\$
4. Tuning and Stability: The intercept was held constant, ensuring the model includes a baseline level from which the effects of predictors are measured, stabilizing comparisons and interpretations across different models.

```{r}
# Define training control for 10-fold cross-validation
train_control2 <- trainControl(method = "cv", number = 10)

# Train the model using linear regression with 10-fold cross-validation
model2 <- train(x = X, y = y, method = "rf", trControl = train_control2)

# Print the results
print(model2)
```
Interpretation:

1. Model: The random forest model used 373 samples and 5 predictors, validated through 10-fold cross-validation to ensure robustness across different subsets of the data. 

2. Tuning Parameter (mtry) Selection: The model was tested with different values of `mtry` (number of variables randomly sampled as candidates at each split): 2, 3, and 5. This parameter affects the complexity and potential overfitting of the model.

3. Performance Metrics: Among the tested mtry values, `mtry = 3` yielded the best results with an RMSE of 15,065.59, an R-squared of 0.9071, and an MAE of 10,307.93, indicating the highest model accuracy and prediction consistency compared to the other values.

4. Optimal Model Choice: The model with `mtry = 3` was selected as the optimal configuration based on having the lowest RMSE, signifying the best balance between model complexity and predictive accuracy. This model provided a good fit and explained approximately 90.71% of the variability in the target variable.


```{r}
# Create a data frame for Linear Regression results
linear_regression_results <- data.frame(
  Model = "Linear Regression",
  RMSE = 15658.61,
  Rsquared = 0.8973575,
  MAE = 11291.03
)

# Create a data frame for Random Forest results
random_forest_results <- data.frame(
  Model = "Random Forest",
  RMSE = 15065.59,  # Using the best RMSE corresponding to mtry = 3
  Rsquared = 0.9071382,  # Rsquared for mtry = 3
  MAE = 10307.93  # MAE for mtry = 3
)

# Combine the results into a single data frame
comparison_results <- rbind(linear_regression_results, random_forest_results)

# Print the results to compare the models
print(comparison_results)

```


```{r}
# Make predictions on the test set
predicted_salary1 <- round(predict(model1, newdata = X_test))

# Create a data frame with Actual Salary, Predicted Salary, and Error
predicted_df1 <- data.frame(
  Actual_Salary = y_test,
  Predicted_Salary = predicted_salary1,
  Error = predicted_salary1 - y_test
)

# View the first few rows of the data frame
head(predicted_df1)

```

```{r}
# Make predictions on the test set
predicted_salary2 <- round(predict(model2, newdata = X_test))

# Create a data frame with Actual Salary, Predicted Salary, and Error
predicted_df2 <- data.frame(
  Actual_Salary = y_test,
  Predicted_Salary = predicted_salary2,
  Error = predicted_salary2 - y_test
)

# View the first few rows of the data frame
head(predicted_df2)
```

```{r}
ggplot(predicted_df1, aes(x = Actual_Salary, y = predicted_salary1)) +
  geom_point(alpha = 0.8, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Linear Regression Line") +
  theme_minimal() 
```




```{r}

# Load necessary libraries
library(randomForest)
library(caret) # For creating dummy variables and train/test splits

suppressPackageStartupMessages(library(randomForest))

# Assuming df_encoded is your dataframe and it contains 'Job.Title', 'Salary', 'Gender' among other features

# Convert 'Job.Title' to a factor if it's not already
df_encoded$Job.Title <- as.factor(df_encoded$Job.Title)

# Create dummy variables for all categorical features, excluding 'Gender' and 'Salary'
df_dummies <- dummyVars(Salary ~ ., data = df_encoded, fullRank = TRUE)
df_transformed <- data.frame(predict(df_dummies, newdata = df_encoded))

# Ensure 'Salary' is not included in the transformed data frame
df_transformed <- df_transformed[, !(names(df_transformed) %in% c("Salary", "X.Education.Level_Master.s.","Education.Level_PhD"))]
#print(names(df_transformed))

# Selecting the target variable 'Salary'
y <- df_encoded$Salary

# Set seed for reproducibility
set.seed(30)

# Split the data into training and testing sets
trainIndex <- createDataPartition(y, p = .8, list = FALSE, times = 1)
X_train <- df_transformed[trainIndex, ]
X_test <- df_transformed[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Train a Random Forest model
rf_model <- randomForest(x = X_train, y = y_train)

# Print the model summary
print(rf_model)

# Predict on the test set
predictions <- predict(rf_model, X_test)
```

```{r}
# metrics for the test data
test_results <- postResample(pred = predictions, obs = y_test)
print(round(test_results,3))
```

```{r}
# Create a data frame for Linear Regression results
linear_regression_testing <- data.frame(
  Model = "Linear Regression",
  RMSE = 14352.312,
  Rsquared = 0.8825102,
  MAE = 9470.211
)

# Create a data frame for Random Forest results
random_forest_testing <- data.frame(
  Model = "Random Forest",
  RMSE = 14197.533,  # Using the best RMSE corresponding to mtry = 3
  Rsquared = 0.9162342,  # Rsquared for mtry = 3
  MAE = 8840.320  # MAE for mtry = 3
)

# Combine the results into a single data frame
testing_results <- rbind(linear_regression_testing, random_forest_testing)

# Print the results to compare the models
print(testing_results)
```


```{r}
# Plot variable importance
varImpPlot(rf_model, main = "Variable Importance Plot", cex = 0.7, n.var = 10)
```

```{r}
p <- predict(rf_model, newdata = X_test)

p_df <- data.frame(Actual_Salary = y_test, Predicted_Salary = p)

library(ggplot2)

ggplot(p_df, aes(x = Actual_Salary, y = Predicted_Salary)) +
  geom_point() +
  geom_smooth(method = "lm",se = FALSE, color = "red") +
  ggtitle("Random Forest Line") +
  theme_minimal() +
  xlab("Actual Salary") +
  ylab("Predicted Salary")
```
```{r}
# Create a data frame for the metrics
metrics_df <- data.frame(
  Model = c("Linear Regression", "Random Forest", "Linear Regression", "Random Forest"),
  Dataset = c("Training", "Training", "Testing", "Testing"),
  RMSE = c(15658.61, 15065.59, 14352.31, 14197.53),
  Rsquared = c(0.8973575, 0.9071382, 0.8825102, 0.9162342),
  MAE = c(11291.03, 10307.93, 9470.211, 8840.320)
)

# Print the data frame
print(metrics_df)

```


```{r}
# Adding fitted values to the original data frame
cleaned_data$Fitted_Values <- fitted(simple_model)

# Load necessary library
library(ggplot2)

# Plotting Age vs Fitted Salary using color for fitted values
ggplot(cleaned_data, aes(x = Age, y = log(Salary), color = Fitted_Values)) +
  geom_point(alpha = 0.6) +  # Using semi-transparent points
  scale_color_gradient(low = "gray", high = "red") +  # Gradient color from blue to red
  labs(title = "Age vs Salary (Colored by Fitted Values)", x = "Age", y = "Salary") +
  theme_minimal()  # Using a minimal theme for better visibility

```

```{r}
# Plotting Years of Experience vs Salary using color for fitted values
ggplot(cleaned_data, aes(x = Years.of.Experience, y = Salary, color = Fitted_Values)) +
  geom_point(alpha = 0.6) +  # Using semi-transparent points
  scale_color_gradient(low = "gray", high = "red") +  # Gradient color from blue to red
  labs(title = "Years of Experience vs Salary (Colored by Fitted Values)", x = "Years of Experience", y = "Salary") +
  theme_minimal()  # Using a minimal theme for better visibility

```




