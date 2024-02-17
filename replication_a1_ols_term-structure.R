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

# creates a matrix with (y*x) rows and (estats) columns
create_table_inflation_models <- function(values_estats, y, x, estats=c('relative_rmse', '1-lambda', 'hh_error', 'nw_error')){
  # values_stats should be a list of same size as the estats, each value being y*x size (in that order)
  # storing data for table
  table_paper <- matrix(NA, length(y)*length(x), length(estats))  # just for a single estimation period
  # prettify
  colnames(table_paper) <- estats
  names_inflation <- rep(y, each=length(x))
  names_models <- rep(x, length(y))
  rownames(table_paper) <- paste(names_inflation, names_models, sep=' | ')
  
  # adding values
  for (p in 1:length(estats)) {
    table_paper[,p] <- values_estats[[p]]
  }
  
  return(table_paper)
}

# ---
# Run ----
# ---

# change here to change periods (either 1985 or 1995 yet)
initial_estimation_period <- 1995
arma11_rmse <- arma11_rmse1995
arma11_forecasts <- arma11_forecasts1995

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
    train_data <- df_lag %>% filter(group <= initial_estimation_period)
    test_data <- df_lag %>% filter(group > initial_estimation_period)
    
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

# calculate the relative rmse to arma11
relative_rmse <- rmse/rep(arma11_rmse, each=length(x_phillips))
# define a list to pass to the function to create tables paper-like
values_estats <- list(relative_rmse, lambda_phillips, hh_se_phillips, nw_se_phillips)
table_ols <- create_table_inflation_models(values_estats, all_y, x_phillips)


# ---
# now do it for models with double x's
# ---
doublex_phillips <- list(c('gap1', 'lshr'), c('gap2', 'lshr'))
x_names <- sapply(doublex_phillips, function(x) paste(x, collapse=' + '))
x_now <- doublex_phillips
rmse <- c()
lambda_phillips <- c()
hh_se_phillips <- c()
nw_se_phillips <- c()
# loop
for (y in all_y) {
  print(y)
  for (x in x_now) {
    print(x)
    # define all the dataframes relevant
    df_lag <- lag_df(df_phillips, y, x)
    train_data <- df_lag %>% filter(group <= initial_estimation_period)
    test_data <- df_lag %>% filter(group > initial_estimation_period)
    
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

# calculate the relative rmse to arma11
relative_rmse <- rmse/rep(arma11_rmse, each=length(x_names))
# define a list to pass to the function to create tables paper-like
values_estats <- list(relative_rmse, lambda_phillips, hh_se_phillips, nw_se_phillips)
table_ols_2x <- create_table_inflation_models(values_estats, all_y, x_names)
print(table_ols_2x)

# ---
## Term Structure ----
# ---

# basically the same as before, plus rate

all_y <- df_phillips %>% select(starts_with('inflation')) %>% colnames()  # "inflation_punew" "inflation_puxhs" "inflation_puxx"  "inflation_pce"
x_phillips <- df_phillips %>% select(gdpg:fac) %>% colnames()  # "gdpg"            "gap1"            "gap2"            "unrate"          "lshr"            "xli"             "xli2"            "fac"
x_with_rate <- lapply(x_phillips, function(x) c(x, "rate"))
x_names <- sapply(x_with_rate, function(x) paste(x, collapse=' + '))
x_now <- x_with_rate

# two loops, one for the dependent variable, other for the independent variable
rmse <- c()
lambda_phillips <- c()
hh_se_phillips <- c()
nw_se_phillips <- c()
for (y in all_y) {
  print(y)
  for (x in x_now) {
    print(x)
    # define all the dataframes relevant
    df_lag <- lag_df(df_phillips, y, x)
    train_data <- df_lag %>% filter(group <= initial_estimation_period)
    test_data <- df_lag %>% filter(group > initial_estimation_period)
    
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

# calculate the relative rmse to arma11
relative_rmse <- rmse/rep(arma11_rmse, each=length(x_names))
# define a list to pass to the function to create tables paper-like
values_estats <- list(relative_rmse, lambda_phillips, hh_se_phillips, nw_se_phillips)
table_term_structure <- create_table_inflation_models(values_estats, all_y, x_names)
print(table_term_structure)

# ---
# now do it for some other specific
# ---
term_structure_x_specific <- list(c('yield'), c('rate', 'yield'), c('gdpg', 'rate', 'yield'))
x_names <- sapply(term_structure_x_specific, function(x) paste(x, collapse=' + '))
x_now <- term_structure_x_specific
rmse <- c()
lambda_phillips <- c()
hh_se_phillips <- c()
nw_se_phillips <- c()
# loop
for (y in all_y) {
  print(y)
  for (x in x_now) {
    print(x)
    # define all the dataframes relevant
    df_lag <- lag_df(df_phillips, y, x)
    train_data <- df_lag %>% filter(group <= initial_estimation_period)
    test_data <- df_lag %>% filter(group > initial_estimation_period)
    
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

# calculate the relative rmse to arma11
relative_rmse <- rmse/rep(arma11_rmse, each=length(x_names))
# define a list to pass to the function to create tables paper-like
values_estats <- list(relative_rmse, lambda_phillips, hh_se_phillips, nw_se_phillips)
table_term_structure_specific <- create_table_inflation_models(values_estats, all_y, x_names)
print(table_term_structure_specific)


# # ---
# # Test section  ----
# # ---
# 
# # a similar to the main loop above
# 
# y <- 'inflation_punew'
# x <- c('gap1', 'lshr')
# 
# # define all the dataframes relevant
# df_lag <- lag_df(df_phillips, y, x)
# train_data <- df_lag %>% filter(group <= initial_estimation_period)
# test_data <- df_lag %>% filter(group > initial_estimation_period)
# 
# # get forecast
# forecast_values <- ols_predict(train_data, test_data, y, x)
# 
# # todo: store the arma11 rmse from previous exercise
# # compute (relative) rmse
# rmse_test <- sqrt(sum((test_data[y] - forecast_values)^2, na.rm=T))
# rmse_test <- sqrt(sum((test_data[y] - forecast_values)^2, na.rm=T))
