# 01 Packages and environment -----------------------

# Limpar o ambiente de trabalho
rm(list = ls()) 

# Carreggar pacotes necessários
library(tidyverse)
library(dplyr)
library(quantmod)
library(forecast)
library(lubridate)
library(gt)
library(tibble)
library(knitr)
library(kableExtra)
library(readxl)

# 02 load dataframes used ------------------------

df_inflation_complete <- read.csv(file = "df_inflation_complete.csv")
df_inflation_authors <- read.csv(file = "df_inflation_authors.csv")
df_inflation_authors_yearly <- read.csv(file = "df_inflation_authors_yearly.csv")

# 03 models ------------------------------------

# Function to perform rolling window forecasting with ARMA(1,1) over years
rolling_forecast_arma <- function(data, series_name, order_ar=1, order_ma=1, initial_end=as.Date('1985-12-01'), final_date=as.Date('2002-12-01'), window_size=4, return_rmse=T) {
  # adding a forecast vector to store results
  forecasts <- numeric((year(final_date) - year(initial_end))*window_size)
  
  # loop for the rolling window
  for (y in year(initial_end):year(final_date)) {
    train_data <- data %>% filter(FirstDate <= as.Date(str_c(y, '-12-01')))  # Use data up to the current index for training
    train_series <- train_data %>% .[series_name]
    
    # Fit ARMA(1,1) model: yt = mu + phi*yt-1 + epsilon_t + psi*epsilon_{t-1}
    arma_model <- arima(train_series, order = c(order_ar, 0, order_ma))
    # get coefficients + residuals
    phi <- arma_model$coef[1]
    psi <- arma_model$coef[2]
    mu <- arma_model$coef[3]
    residuals <- arma_model$residuals
    
    # Forecast for the next 4 quarters according to equation on paper
    forecast_q <- c()
    for (q in 1:4) {
      forecast_q[q] <- (1/(1-phi)) * (4 - (phi * (1-phi^4))/(1-phi)) * mu + ((phi * (1-phi^4))/(1-phi)) * train_series[nrow(train_series) - (q-1),] + (psi * (1-phi^4))/(1-phi) * residuals[nrow(train_series) - (q-1)]
    }
    
    # just making sure
    forecast_values <- as.numeric(forecast_q)
    # Store forecast values
    forecasts[((y - year(initial_end))*window_size + 1):((y - year(initial_end)+1)*window_size)] <- forecast_values
  }
  
  # decide whether to return forecasts or root mean squared errors (RMSE)
  if (return_rmse) {
    # get data to validate forecasts
    test_series <- data %>% filter(FirstDate > as.Date(initial_end)) %>% .[series_name]
    rmse <- sqrt(sum((test_series - forecasts)^2))
    return(rmse)
  } else{
    return(forecasts)
  }
}

# very similar to the last function, 
# with the difference that now we'll choose AR(p) recursively by SIC
rolling_forecast_ar_p <- function(data, series_name, initial_end=as.Date('1985-12-01'), final_date=as.Date('2002-12-01'), window_size=4, return_rmse=T) {
  # creating a forecast vector to store results
  forecasts <- numeric((year(final_date) - year(initial_end))*window_size)
  
  # loop for the rolling window
  for (y in year(initial_end):year(final_date)) {
    train_data <- data %>% filter(FirstDate <= as.Date(str_c(y, '-12-01')))  # Use data up to the current index for training
    train_series <- train_data %>% select(series_name)
    
    # find best ar order
    ar_order <- best_arma_order(train_series)
    # Fit AR(p) model
    arma_model <- stats::arima(train_series, order = c(ar_order, 0, 0))
    
    # Forecast for the next 4 quarters
    #  ... a little note: here we abstract from the way the paper is doing, since it would imply unecessary complexity (look at the equation for the quarterly forecast in page 1174)
    forecast_values <- as.numeric(forecast(arma_model, h = 4)$mean)
    # Store forecast values
    forecasts[((y - year(initial_end))*window_size + 1):((y - year(initial_end)+1)*window_size)] <- forecast_values
  }
  
  # decide whether to return forecasts or root mean squared errors (RMSE)
  if (return_rmse) {
    # get data to validate forecasts
    test_series <- data %>% filter(FirstDate > as.Date(initial_end)) %>% .[series_name]
    rmse <- sqrt(sum((test_series - forecasts)^2))
    return(rmse)
  } else{
    return(forecasts)
  }
}

# to find the best arma model based on some criteria (still don't contemplate ma orders or other info_criteria than BIC/SIC)
best_arma_order <- function(series, ar_until = 10, ma_order = 0, info_criteria = 'bic'){
  arma_models <- list()
  bic_arma <- c()
  for (p in 1:ar_until) {
    arma_models[[p]] <- arima(series, order = c(p, 0, ma_order))
    # BIC = SIC
    bic_arma[p] <- BIC(arma_models[[p]])
  }
  arma_best_order <- which(min(bic_arma) == bic_arma)
  return(arma_best_order)
}

# running the functions
inflation_series <- df_inflation_authors %>% select(starts_with('inflation')) %>% colnames()

arma11_rmse <- c()
arp_rmse <- c()
for (x in 1:length(inflation_series)) {
  arma11_rmse[x] <- rolling_forecast_arma(df_inflation_authors, inflation_series[x])
  arp_rmse[x] <- rolling_forecast_ar_p(df_inflation_authors, inflation_series[x])
}

rolling_forecast_arma(df_inflation_authors, inflation_series[1], return_rmse = T)
