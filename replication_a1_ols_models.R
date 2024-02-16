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
df_inflation_authors <- read.csv("df_inflation_authors.csv")
df_realmeasures_authors <- read.csv("df_realmeasures_authors.csv")

# Prepare data
df_phillips <- df_inflation_authors %>%
  left_join(df_realmeasures_authors) %>% 
  tibble() %>%
  arrange(FirstDate)

y <- 'inflation_punew'
x <- 'gdpg'
teste <- df_phillips
for (p in 1:3) {
  teste <- teste %>% 
    mutate(!!str_c('lag', p, '_', y) := dplyr::lag(!!sym(y), p),
           !!str_c('lag', p, '_', x) := dplyr::lag(!!sym(x), p))
}

dependent_var <- y
independent_var <- teste %>% select(starts_with('lag1')) %>% colnames()

for (p in 2:3) {
  independent_var_plus <- teste %>% select(starts_with(paste0('lag', p))) %>% colnames()
  independent_var <- c(independent_var, independent_var_plus)
}

reg_formula <- formula(paste(dependent_var, '~', paste(independent_var, collapse=' + ')))
model <- lm(reg_formula, data = teste)
# a function just to lag our dataset
lag_df <- function(df, y, x, lag_until=5){
  models <- list()
  bic_models <- c()
  df_lag <- df
  # alter the data to contain the lags you want
  for (p in 1:lag_until) {
    df_lag <- df_lag %>% 
      mutate(!!str_c('lag', p, '_', y) := dplyr::lag(!!sym(y), p),
             !!str_c('lag', p, '_', x) := dplyr::lag(!!sym(x), p))
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

all_y <- df_phillips %>% select(starts_with('inflation')) %>% colnames()  # "inflation_punew" "inflation_puxhs" "inflation_puxx"  "inflation_pce"
all_x <-df_phillips %>% select(gdpg:fac) %>% colnames()  # "gdpg"            "gap1"            "gap2"            "unrate"          "lshr"            "xli"             "xli2"            "fac"

# todo: we need more one loop for the other group: 1995

# two loops, one for the dependent variable, other for the independent variable
rmse <- c()
for (y in all_y) {
  print(y)
  for (x in all_x) {
    print(x)
    # define all the dataframes relevant
    df_lag <- lag_df(df_phillips, y, x)
    train_data <- df_lag %>% filter(group <= 1985)
    test_data <- df_lag %>% filter(group > 1985)
    
    # get forecast
    forecast_values <- ols_predict(train_data, test_data, y, x)
    
    # todo: store the arma11 rmse from previous exercise
    # compute (relative) rmse
    rmse <- c(rmse, sqrt(sum((test_data[y] - forecast_values)^2)))
    
    # compute 1-lambda
    # model_lambda
    # first with HH-error
    # then with NW-error
    
  }
}

# todo: think how to do this table
# storing data for table
rmse_phillips <- matrix(NA, length(all_y), length(all_x))  # then we just complete and the order of completetion first run by rows then by columns
rmse_phillips[1:nrow(rmse_phillips)*ncol(rmse_phillips)] <- rmse

# separately we run the other two models


# test: ok, except the fact that we have NAs in the end, which giovanni actually already corrected
rmse <- c()
y <- 'inflation_punew'
x <- 'gdpg'
# define all the dataframes relevant
df_lag <- lag_df(df_phillips, y, x)
train_data <- df_lag %>% filter(group <= 1985)
test_data <- df_lag %>% filter(group > 1985)

# get forecast
forecast_values <- ols_predict(train_data, test_data, y, x)

# todo: store the arma11 rmse from previous exercise
# compute (relative) rmse
rmse <- c(rmse, sqrt(sum((test_data[y] - forecast_values)^2)))

