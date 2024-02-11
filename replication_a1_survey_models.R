# Clear workspace and load necessary packages
rm(list = ls())
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
df_inflation_authors_yearly <- read.csv("df_inflation_authors_yearly.csv") %>%
  mutate(across(ends_with("year"), ~ .x * 100 )) %>%
  select(!X)
df_surveys_complete <- read.csv("df_surveys_complete.csv") %>%
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

# Calculate the 8-quarter moving average for each inflation variable
for (inflation_var in inflation_vars) {
  moving_average_var <- paste(inflation_var, "ma", sep = "_")
  df_data[[moving_average_var]] <- stats::filter(df_data[[inflation_var]], rep(1/8, 8), sides = 1)
}

# Create the dummy variable Dt for each inflation variable
for (inflation_var in inflation_vars) {
  Dt_var <- paste(inflation_var, "Dt", sep = "_")
  moving_average_var <- paste(inflation_var, "ma", sep = "_")
  df_data[[Dt_var]] <- ifelse(df_data[[inflation_var]] - df_data[[moving_average_var]] > 0, 1, 0)
}

# Adjust the forecast based on the new regression including the dummy variable
adjust_forecast_with_dummy <- function(data, inflation_var, forecast_var) {
  # The new regression formula includes the dummy variable
  Dt_var <- paste(inflation_var, "Dt", sep = "_")
  formula_str <- paste(inflation_var, "~", forecast_var, "+", Dt_var)
  formula <- as.formula(formula_str)
  
  # Fit the model
  model <- lm(formula, data = data)
  
  # Make predictions using the model
  adjusted_forecasts <- predict(model, newdata = data)
  
  # Create a new column for the adjusted forecasts
  adjusted_forecast_var <- paste(forecast_var, "3", sep = "")
  data[[adjusted_forecast_var]] <- adjusted_forecasts
  
  return(data)
}

# Apply the function to adjust the forecasts for each combination of inflation and forecast variables
for (inflation_var in inflation_vars) {
  for (forecast_var in forecast_vars) {
    adjusted_data2 <- adjust_forecast_with_dummy(df_data, inflation_var, forecast_var)
  }
}

