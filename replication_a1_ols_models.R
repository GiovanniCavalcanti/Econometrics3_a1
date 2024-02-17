# Clear workspace and load necessary packages
# rm(list = ls())
library(tidyverse)
library(quantmod)
library(forecast)
library(lubridate)
library(gt)
library(tibble)
library(knitr)
library(kableExtra)
library(readxl)
library(sandwich)

# ---
# Load data ----
# ---

# this value will come from other part of the markdown
# arma11_rmse <- c(0.8938677, 0.8336303, 0.9984834, 0.8883241)

# df_inflation_complete <- read.csv("df_inflation_complete.csv")
df_inflation_authors <- read.csv("data/output/df_inflation_authors.csv")
df_realmeasures_authors <- read.csv("data/output/df_realmeasures_authors.csv")

# Prepare data
df_phillips <- df_inflation_authors %>%
  left_join(df_realmeasures_authors) %>% 
  tibble() %>%
  arrange(FirstDate)

# ---
# Functions ----
# ---

# a function just to lag our dataset
lag_df <- function(df, y, x, lag_until = 5) {
  models <- list()
  bic_models <- c()
  df_lag <- df
  
  # first we loop for the y variable
  for (p in 1:lag_until) {
    df_lag <- df_lag %>% 
      mutate(!!str_c('lag', p, '_', y) := lag(!!sym(y), p))
  }
  
  # then we check whether x is a vector and do the adequate operation 
  if (is.character(x) && length(x) == 1) {
    # If x is a single string, create lagged column for it
    for (p in 1:lag_until) {
      df_lag <- df_lag %>% 
        mutate(!!str_c('lag', p, '_', x) := lag(!!sym(x), p))
    }
  } else if (is.character(x) && length(x) > 1) {
    # If x is a character vector, create lagged columns for each element
    for (var in x) {
      for (p in 1:lag_until) {
        df_lag <- df_lag %>% 
          mutate(!!str_c('lag', p, '_', var) := lag(!!sym(var), p))
      }
    }
  }
  
  return(df_lag)
}

# a function to choose the best lag of the ols based on bic
best_lag_ols <- function(df_lag, y, lag_until=5, criteria='bic'){
  # defining relevant variables
  dependent_var <- y
  independent_var <- c()
  
  # variables to store
  models <- list()
  bic_models <- c()
  
  # loop of lags
  for (p in 1:lag_until) {
    independent_var_plus <- df_lag %>% select(starts_with(paste0('lag', p))) %>% colnames()
    independent_var <- c(independent_var, independent_var_plus)
    reg_formula <- formula(paste(dependent_var, '~', paste(independent_var, collapse=' + ')))
    models[[p]] <- lm(reg_formula, data=df_lag)
    bic_models[p] <- BIC(models[[p]])
  }
  
  # then find the best order
  best_order <- which(min(bic_models) == bic_models)
  
  return(models[[best_order]])
}

# todo: use a rolling window?
# a function to predict for a ols model
ols_predict <- function(train_data, test_data, y, x, max_lag=5){
  # choose the best lag and returns the model
  model <- best_lag_ols(train_data, y, lag_until = max_lag)
  
  # forecast values
  forecast_values <- forecast(model, test_data)
  
  return(forecast_values$mean)
}

# ---
# Run ----
# ---

# ---
## Phillips models ----
# ---

all_y <- df_phillips %>% select(starts_with('inflation')) %>% colnames()  # "inflation_punew" "inflation_puxhs" "inflation_puxx"  "inflation_pce"
x_phillips <- df_phillips %>% select(gdpg:fac) %>% colnames()  # "gdpg"            "gap1"            "gap2"            "unrate"          "lshr"            "xli"             "xli2"            "fac"

# todo: we need more one loop for the other group: 1995

# two loops, one for the dependent variable, other for the independent variable
rmse <- c()
lambda_phillips <- c()
hh_se_phillips <- c()
nw_se_phillips <- c()
for (y in all_y) {
  print(y)
  for (x in x_phillips) {
    print(x)
    # define all the dataframes relevant
    df_lag <- lag_df(df_phillips, y, x)
    train_data <- df_lag %>% filter(group <= 1985)
    test_data <- df_lag %>% filter(group > 1985)
    
    # get forecast
    forecast_values <- ols_predict(train_data, test_data, y, x)
    
    # todo: store the arma11 rmse from previous exercise
    # compute (relative) rmse
    rmse <- c(rmse, sqrt(sum((test_data[y] - forecast_values)^2, na.rm=T)))
    
    # compute 1-lambda
    reg_formula_lambda <- formula(paste0(y, '~ arma11_forecasts[["inflation_punew"]] + forecast_values'))
    model_lambda <- lm(reg_formula_lambda, data=test_data)
    lambda_phillips <- c(lambda_phillips ,coef(model_lambda)[3])
    # standard-error first with HH-error
    hh_se_phillips <- c(hh_se_phillips ,kernHAC(model_lambda, kernel = "Truncated")[3,3])
    # then with NW-error
    nw_se_phillips <- c(nw_se_phillips, NeweyWest(model_lambda)[3,3])
  }
}

# storing data for table
rmse_phillips <- matrix(NA, length(x_phillips), length(all_y))  # then we just complete and the order of completetion first run by rows then by columns
n_values <- length(all_y)*length(x_phillips)
# prettify
colnames(rmse_phillips) <- all_y
rownames(rmse_phillips) <- x_phillips

# note: order is important here, since we do first in the collumn (y) direction, then in the row (x) direction
rmse_phillips[1:n_values] <- rmse
# operation collumn-wise
rmse_phillips_relative <- sweep(rmse_phillips, 2, arma11_rmse, "/")

# ---
# now do it for models with double x's
# ---
doublex_phillips <- list(c('gap1', 'lshr'), c('gap2', 'lshr'))
rmse <- c()
lambda_phillips <- c()
hh_se_phillips <- c()
nw_se_phillips <- c()
# loop
for (y in all_y) {
  print(y)
  for (x in doublex_phillips) {
    print(x)
    # define all the dataframes relevant
    df_lag <- lag_df(df_phillips, y, x)
    train_data <- df_lag %>% filter(group <= 1985)
    test_data <- df_lag %>% filter(group > 1985)
    
    # get forecast
    forecast_values <- ols_predict(train_data, test_data, y, x)
    
    # todo: store the arma11 rmse from previous exercise
    # compute (relative) rmse
    rmse <- c(rmse_doublex, sqrt(sum((test_data[y] - forecast_values)^2, na.rm=T)))
    
    # compute 1-lambda
    reg_formula_lambda <- formula(paste0(y, '~ arma11_forecasts[["inflation_punew"]] + forecast_values'))
    model_lambda <- lm(reg_formula_lambda, data=test_data)
    lambda_phillips <- c(lambda_phillips ,coef(model_lambda)[3])
    # standard-error first with HH-error
    hh_se_phillips <- c(hh_se_phillips ,kernHAC(model_lambda, kernel = "Truncated")[3,3])
    # then with NW-error
    nw_se_phillips <- c(nw_se_phillips, NeweyWest(model_lambda)[3,3])
  }
}




#
print(rmse_phillips_relative)

# ---
## Term Structure ----
# ---

# basically the same as before, plus rate


# ---
# Test section  ----
# ---

# a similar to the main loop above

y <- 'inflation_punew'
x <- c('gap1', 'lshr')

# define all the dataframes relevant
df_lag <- lag_df(df_phillips, y, x)
train_data <- df_lag %>% filter(group <= 1985)
test_data <- df_lag %>% filter(group > 1985)

# get forecast
forecast_values <- ols_predict(train_data, test_data, y, x)

# todo: store the arma11 rmse from previous exercise
# compute (relative) rmse
rmse_test <- sqrt(sum((test_data[y] - forecast_values)^2, na.rm=T))
