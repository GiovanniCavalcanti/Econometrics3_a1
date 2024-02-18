# Clear workspace and load necessary packages

library(tidyverse)
library(quantmod)
library(forecast)
library(lubridate)
library(gt)
library(tibble)
library(knitr)
library(kableExtra)
library(readxl)

# Load data
# df_inflation_complete <- read.csv("df_inflation_complete.csv")
df_inflation_authors_yearly <- read.csv("data/output/df_inflation_authors_yearly.csv") %>%
  mutate(across(ends_with("year"), ~ .x * 100 )) %>%
  select(!X)
df_surveys_complete <- read.csv("data/output/df_surveys_complete.csv") %>%
  select(!X)

# Prepare data
df_data <- df_inflation_authors_yearly %>%
  left_join(df_surveys_complete) %>% 
  filter(quarter >= "1952 Q2" & quarter <= "2002 Q4")

fit_models <- function(inflation_var, forecast_var, data) {
  formula_str <- paste(inflation_var, "~", forecast_var)
  model <- lm(as.formula(formula_str), data = data)
  return(model)
}

update_forecasts <- function(data, inflation_vars, forecast_vars) {
  for (inflation_var in inflation_vars) {
    for (forecast_var in forecast_vars) {
      # Initial model fit
      model <- fit_models(inflation_var, forecast_var, data)
      
      # Adjust forecasts for each time period (assuming data is already split by time if needed)
      adjusted_forecast_var <- paste(forecast_var, "2", sep = "")
      predictions <- predict(model, newdata = data)
      # Creating or updating the adjusted forecast column
      data[[adjusted_forecast_var]] <- predictions
    }
  }
  return(data)
}

# Define your inflation and forecast variables
inflation_vars <- c("punew_year", "puxhs_year", "puxx_year", "pce_year")
forecast_vars <- c("liv_year", "mich_year", "spf_year")

# Adjust forecasts recursively
adjusted_data1 <- update_forecasts(df_data, inflation_vars, forecast_vars)


## temptative trial to implement model 3 (nonlinear adjustment to surveys) -----
# Calculate the 8-quarter moving average for each inflation variable
# for (inflation_var in inflation_vars) {
#  moving_average_var <- paste(inflation_var, "ma", sep = "_")
#  df_data[[moving_average_var]] <- stats::filter(df_data[[inflation_var]], rep(1/8, 8), sides = 1)
#}

# Create the dummy variable Dt for each inflation variable
# for (inflation_var in inflation_vars) {
#  Dt_var <- paste(inflation_var, "Dt", sep = "_")
#  moving_average_var <- paste(inflation_var, "ma", sep = "_")
#  df_data[[Dt_var]] <- ifelse(df_data[[inflation_var]] - df_data[[moving_average_var]] > 0, 1, 0)
#}

# Adjust the forecast based on the new regression including the dummy variable
#adjust_forecast_with_dummy <- function(data, inflation_var, forecast_var) {
#  # The new regression formula includes the dummy variable
#  Dt_var <- paste(inflation_var, "Dt", sep = "_")
#  formula_str <- paste(inflation_var, "~", forecast_var, "+", Dt_var)
#  formula <- as.formula(formula_str)
 # 
 # # Fit the model
 # model <- lm(formula, data = data)
  
  # Make predictions using the model
#  adjusted_forecasts <- predict(model, newdata = data)
  
  # Create a new column for the adjusted forecasts
#  adjusted_forecast_var <- paste(forecast_var, "3", sep = "")
#  data[[adjusted_forecast_var]] <- adjusted_forecasts
  
#  return(data)
#}

# Apply the function to adjust the forecasts for each combination of inflation and forecast variables
# for (inflation_var in inflation_vars) {
#  for (forecast_var in forecast_vars) {
#    adjusted_data2 <- adjust_forecast_with_dummy(df_data, inflation_var, forecast_var)
#  }
#}

## Calculating RMSE's -------------

calculate_rmse <- function(column1, column2, df) {
  # Remove rows with NAs in either of the specified columns
  df_cleaned <- na.omit(df[, c(column1, column2)])
  
  # Calculate the squared differences
  squared_diffs <- (df_cleaned[[column1]] - df_cleaned[[column2]])^2
  
  # Calculate the mean of squared differences
  mean_squared_diffs <- mean(squared_diffs)
  
  # Calculate the square root of the mean squared differences
  rmse <- sqrt(mean_squared_diffs)
  
  # Return the RMSE
  return(rmse)
}

# Define the column groups
group1 <- c('punew_year', 'puxhs_year', 'puxx_year', 'pce_year')
group2 <- c('spf_year', 'liv_year', 'mich_year')

# Initialize an empty dataframe to store results
results_df_model1 <- data.frame(matrix(ncol = 3))
colnames(results_df_model1) <- c("Column1", "Column2", "RMSE")

# Double loop to calculate RMSE for each pair
for (col1 in group1) {
  for (col2 in group2) {
    rmse_value <- calculate_rmse(col1, col2, adjusted_data1)
    # Append results to the dataframe
    results_df_model1 <- rbind(results_df_model1, c(col1, col2, rmse_value))
  }
}

# Converting RMSE column to numeric, as it was coerced to factor/character by rbind
results_df_model1$RMSE <- as.numeric(as.character(results_df_model1$RMSE))
results_df_model1 <- results_df_model1[2:nrow(results_df_model1),]


# Define the column groups
group1 <- c('punew_year', 'puxhs_year', 'puxx_year', 'pce_year')
group2 <- c('spf_year2', 'liv_year2', 'mich_year2')

# Initialize an empty dataframe to store results
results_df_model2 <- data.frame(matrix(ncol = 3))
colnames(results_df_model2) <- c("Column1", "Column2", "RMSE")

# Double loop to calculate RMSE for each pair
for (col1 in group1) {
  for (col2 in group2) {
    rmse_value <- calculate_rmse(col1, col2, adjusted_data1)
    # Append results to the dataframe
    results_df_model2 <- rbind(results_df_model2, c(col1, col2, rmse_value))
  }
}

# Converting RMSE column to numeric, as it was coerced to factor/character by rbind
results_df_model2$RMSE <- as.numeric(as.character(results_df_model2$RMSE))
results_df_model2 <- results_df_model2[2:nrow(results_df_model2),]

print(results_df_model1)
print(results_df_model2)
