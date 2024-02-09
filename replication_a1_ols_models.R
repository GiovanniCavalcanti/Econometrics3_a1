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
df_inflation_complete <- read.csv("df_inflation_complete.csv")
df_inflation_authors <- read.csv("df_inflation_authors.csv")
df_realmeasures_authors <- read.csv("df_realmeasures_authors.csv")

# Prepare data
data <- df_inflation_authors %>%
  left_join(df_realmeasures_authors)

# post 1985 period regressions -----------
train_data <- filter(data, group <= 1985)
test_data <- filter(data, group > 1985)

## PC1 -------

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC1 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC1) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC1 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors

# Column indices for inflation variables and their corresponding lags
inflation_vars <- c("inflation_punew", "inflation_puxhs", "inflation_puxx", "inflation_pce")
lag_vars <- c(3, 4, 5, 6) # Assuming these are the correct column indices for cpi_lag and act_lag

# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 9], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 9], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC1[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC1[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC1[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC1) <- "PC1"

## PC2 ----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC2 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC2) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC2 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 10], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 10], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC2[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC2[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC2[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC2) <- "PC2"


## PC3 -----
  
# Initialize placeholders for predictions and evaluations
forecastA_tab_PC3 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC3) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC3 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 11], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 11], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC3[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC3[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC3[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC3) <- "PC3"

## PC4 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC4 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC4) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC4 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 13], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 13], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC4[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC4[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC4[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC4) <- "PC4"

## PC5 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC5 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC5) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC5 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 12], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 12], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC5[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC5[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC5[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC5) <- "PC5"

## PC6 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC6 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC6) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC6 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 14], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 14], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC6[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC6[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC6[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC6) <- "PC6"

## PC7 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC7 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC7) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC7 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 15], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 15], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC7[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC7[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC7[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC7) <- "PC7"


## PC8 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC8 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC8) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC8 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 16], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 16], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC8[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC8[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC8[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC8) <- "PC8"
## PC9 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC9 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC9) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC9 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 10], n = 3), act_lag2 = lag(.[, 13], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 10], n = 3), act_lag2 = lag(.[, 13], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag1 + act_lag2")), data = train_data)
  forecastA_tab_PC9[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC9[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC9[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC9) <- "PC9"

## PC10 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC10 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC10) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC10 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 11], n = 3), act_lag2 = lag(.[, 13], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 11], n = 3), act_lag2 = lag(.[, 13], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag1 + act_lag2")), data = train_data)
  forecastA_tab_PC10[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC10[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC10[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC10) <- "PC10"

## Merge list and results ------------

# List of forecast table names
forecast_table_names <- c("forecastA_tab_PC1", "forecastA_tab_PC2", "forecastA_tab_PC3", "forecastA_tab_PC4", "forecastA_tab_PC5",
                          "forecastA_tab_PC6", "forecastA_tab_PC7", "forecastA_tab_PC8", "forecastA_tab_PC9", "forecastA_tab_PC10")

# Loop through each forecast table name, modify, and reassign
for (name in forecast_table_names) {
  # Assuming each table variable is in the global environment
  assign(name, `[[<-`(get(name), "FirstDate", value = test_data$FirstDate), envir = .GlobalEnv)
}

# appending findings

forecastA_a<-list(forecastA_tab_PC1,forecastA_tab_PC2,forecastA_tab_PC3,forecastA_tab_PC4,
                  forecastA_tab_PC5,forecastA_tab_PC6,forecastA_tab_PC7,forecastA_tab_PC8,
                  forecastA_tab_PC9,forecastA_tab_PC10)

valuesAPC_a<-as.data.frame(t(cbind(valuesAPC1,valuesAPC2,valuesAPC3,valuesAPC4,valuesAPC5,
                 valuesAPC6,valuesAPC7,valuesAPC8,valuesAPC9,valuesAPC10))) 

colnames(valuesAPC_a) <- c("PUNEW", "PUXHS", "PUXX", "PCE")


# post 1995 period regressions ----------
train_data <- filter(data, group <= 1995)
test_data <- filter(data, group > 1995)

## PC1 -------

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC1 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC1) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC1 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors

# Column indices for inflation variables and their corresponding lags
inflation_vars <- c("inflation_punew", "inflation_puxhs", "inflation_puxx", "inflation_pce")
lag_vars <- c(3, 4, 5, 6) # Assuming these are the correct column indices for cpi_lag and act_lag

# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 9], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 9], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC1[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC1[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC1[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC1) <- "PC1"

## PC2 ----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC2 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC2) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC2 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 10], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 10], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC2[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC2[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC2[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC2) <- "PC2"


## PC3 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC3 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC3) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC3 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 11], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 11], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC3[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC3[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC3[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC3) <- "PC3"

## PC4 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC4 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC4) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC4 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 13], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 13], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC4[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC4[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC4[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC4) <- "PC4"

## PC5 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC5 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC5) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC5 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 12], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 12], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC5[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC5[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC5[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC5) <- "PC5"

## PC6 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC6 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC6) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC6 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 14], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 14], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC6[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC6[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC6[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC6) <- "PC6"

## PC7 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC7 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC7) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC7 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 15], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 15], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC7[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC7[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC7[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC7) <- "PC7"


## PC8 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC8 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC8) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC8 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 16], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 16], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC8[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC8[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC8[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC8) <- "PC8"
## PC9 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC9 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC9) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC9 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 10], n = 3), act_lag2 = lag(.[, 13], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 10], n = 3), act_lag2 = lag(.[, 13], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag1 + act_lag2")), data = train_data)
  forecastA_tab_PC9[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC9[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC9[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC9) <- "PC9"

## PC10 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC10 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC10) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC10 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 11], n = 3), act_lag2 = lag(.[, 13], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 11], n = 3), act_lag2 = lag(.[, 13], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag1 + act_lag2")), data = train_data)
  forecastA_tab_PC10[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(136:203), 3] - forecastA_tab_PC10[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC10[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC10) <- "PC10"

## Merge list and results ------------

# List of forecast table names
forecast_table_names <- c("forecastA_tab_PC1", "forecastA_tab_PC2", "forecastA_tab_PC3", "forecastA_tab_PC4", "forecastA_tab_PC5",
                          "forecastA_tab_PC6", "forecastA_tab_PC7", "forecastA_tab_PC8", "forecastA_tab_PC9", "forecastA_tab_PC10")

# Loop through each forecast table name, modify, and reassign
for (name in forecast_table_names) {
  # Assuming each table variable is in the global environment
  assign(name, `[[<-`(get(name), "FirstDate", value = test_data$FirstDate), envir = .GlobalEnv)
}

# appending findings

forecastA_b<-list(forecastA_tab_PC1,forecastA_tab_PC2,forecastA_tab_PC3,forecastA_tab_PC4,
                  forecastA_tab_PC5,forecastA_tab_PC6,forecastA_tab_PC7,forecastA_tab_PC8,
                  forecastA_tab_PC9,forecastA_tab_PC10)

valuesAPC_b<-as.data.frame(t(cbind(valuesAPC1,valuesAPC2,valuesAPC3,valuesAPC4,valuesAPC5,
                                   valuesAPC6,valuesAPC7,valuesAPC8,valuesAPC9,valuesAPC10))) 

colnames(valuesAPC_b) <- c("PUNEW", "PUXHS", "PUXX", "PCE")

# Extended sample america ----------------

rm(list=setdiff(ls(), c("forecastA_a", "forecastA_b", "valuesAPC_a", "valuesAPC_b")))

# Load data
df_inflation_complete <- read.csv("df_inflation_complete.csv")
df_realmeasures_complete <- read.csv("df_realmeasures_complete.csv")

# Prepare data
data <- df_inflation_complete %>%
  left_join(df_realmeasures_complete)

# post 1985 period regressions -----------
train_data <- filter(data, group <= 1985)
test_data <- filter(data, group > 1985)

## PC1 -------

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC1 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC1) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC1 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors

# Column indices for inflation variables and their corresponding lags
inflation_vars <- c("inflation_punew", "inflation_puxhs", "inflation_puxx", "inflation_pce")
lag_vars <- c(3, 4, 5, 6) # Assuming these are the correct column indices for cpi_lag and act_lag

# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 9], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 9], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC1[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC1[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC1[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC1) <- "PC1"

## PC2 ----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC2 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC2) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC2 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 10], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 10], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC2[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC2[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC2[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC2) <- "PC2"


## PC3 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC3 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC3) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC3 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 11], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 11], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC3[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC3[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC3[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC3) <- "PC3"

## PC4 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC4 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC4) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC4 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 13], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 13], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC4[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC4[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC4[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC4) <- "PC4"

## PC5 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC5 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC5) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC5 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 12], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 12], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC5[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC5[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC5[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC5) <- "PC5"

## PC6 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC6 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC6) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC6 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 14], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 14], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC6[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC6[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC6[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC6) <- "PC6"

## PC7 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC7 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC7) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC7 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 15], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 15], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC7[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC7[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC7[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC7) <- "PC7"


## PC8 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC8 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC8) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC8 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 16], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 16], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC8[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC8[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC8[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC8) <- "PC8"
## PC9 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC9 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC9) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC9 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 10], n = 3), act_lag2 = lag(.[, 13], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 10], n = 3), act_lag2 = lag(.[, 13], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag1 + act_lag2")), data = train_data)
  forecastA_tab_PC9[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC9[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC9[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC9) <- "PC9"

## PC10 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC10 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC10) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC10 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 11], n = 3), act_lag2 = lag(.[, 13], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 11], n = 3), act_lag2 = lag(.[, 13], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag1 + act_lag2")), data = train_data)
  forecastA_tab_PC10[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC10[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC10[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC10) <- "PC10"

## Merge list and results ------------

# List of forecast table names
forecast_table_names <- c("forecastA_tab_PC1", "forecastA_tab_PC2", "forecastA_tab_PC3", "forecastA_tab_PC4", "forecastA_tab_PC5",
                          "forecastA_tab_PC6", "forecastA_tab_PC7", "forecastA_tab_PC8", "forecastA_tab_PC9", "forecastA_tab_PC10")

# Loop through each forecast table name, modify, and reassign
for (name in forecast_table_names) {
  # Assuming each table variable is in the global environment
  assign(name, `[[<-`(get(name), "FirstDate", value = test_data$FirstDate), envir = .GlobalEnv)
}

# appending findings

forecastA_a_extended<-list(forecastA_tab_PC1,forecastA_tab_PC2,forecastA_tab_PC3,forecastA_tab_PC4,
                  forecastA_tab_PC5,forecastA_tab_PC6,forecastA_tab_PC7,forecastA_tab_PC8,
                  forecastA_tab_PC9,forecastA_tab_PC10)

valuesAPC_a_extended<-as.data.frame(t(cbind(valuesAPC1,valuesAPC2,valuesAPC3,valuesAPC4,valuesAPC5,
                                   valuesAPC6,valuesAPC7,valuesAPC8,valuesAPC9,valuesAPC10))) 

colnames(valuesAPC_a_extended) <- c("PUNEW", "PUXHS", "PUXX", "PCE")


# post 1995 period regressions ----------
train_data <- filter(data, group <= 1995)
test_data <- filter(data, group > 1995)

## PC1 -------

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC1 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC1) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC1 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors

# Column indices for inflation variables and their corresponding lags
inflation_vars <- c("inflation_punew", "inflation_puxhs", "inflation_puxx", "inflation_pce")
lag_vars <- c(3, 4, 5, 6) # Assuming these are the correct column indices for cpi_lag and act_lag

# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 9], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 9], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC1[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC1[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC1[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC1) <- "PC1"

## PC2 ----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC2 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC2) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC2 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 10], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 10], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC2[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC2[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC2[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC2) <- "PC2"


## PC3 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC3 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC3) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC3 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 11], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 11], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC3[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC3[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC3[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC3) <- "PC3"

## PC4 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC4 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC4) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC4 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 13], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 13], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC4[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC4[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC4[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC4) <- "PC4"

## PC5 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC5 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC5) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC5 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 12], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 12], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC5[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC5[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC5[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC5) <- "PC5"

## PC6 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC6 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC6) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC6 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 14], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 14], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC6[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC6[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC6[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC6) <- "PC6"

## PC7 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC7 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC7) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC7 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 15], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 15], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC7[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC7[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC7[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC7) <- "PC7"


## PC8 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC8 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC8) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC8 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 16], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag = lag(.[, 16], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag")), data = train_data)
  forecastA_tab_PC8[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC8[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC8[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC8) <- "PC8"
## PC9 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC9 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC9) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC9 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 10], n = 3), act_lag2 = lag(.[, 13], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 10], n = 3), act_lag2 = lag(.[, 13], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag1 + act_lag2")), data = train_data)
  forecastA_tab_PC9[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC9[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC9[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC9) <- "PC9"

## PC10 -----

# Initialize placeholders for predictions and evaluations
forecastA_tab_PC10 <- data.frame(matrix(nrow = nrow(test_data), ncol = 4))
colnames(forecastA_tab_PC10) <- c("PUNEW", "PUXHS", "PUXX", "PCE")
valuesAPC10 <- matrix(nrow = 4, ncol = 1) # To store root mean squared errors


# Loop through each variable to fit models and make predictions
for (i in seq_along(inflation_vars)) {
  var <- inflation_vars[i]
  cpi_lag_index <- lag_vars[i]
  
  # Add lags
  train_data <- train_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 11], n = 3), act_lag2 = lag(.[, 13], n = 3))
  test_data <- test_data %>%
    mutate(cpi_lag = lag(.[, cpi_lag_index], n = 3), act_lag1 = lag(.[, 11], n = 3), act_lag2 = lag(.[, 13], n = 3))
  
  # Fit model and predict
  model <- lm(as.formula(paste(var, "~ cpi_lag + act_lag1 + act_lag2")), data = train_data)
  forecastA_tab_PC10[, i] <- predict(model, newdata = test_data)
  
  # Evaluate
  msre <- data[c(156:307), 3] - forecastA_tab_PC10[, i]
  msre <- msre[-c(1, 2, 3)]
  valuesAPC10[i, 1] <- sqrt(mean(msre^2, na.rm = TRUE))
}

# rename for summary
colnames(valuesAPC10) <- "PC10"

## Merge list and results ------------

# List of forecast table names
forecast_table_names <- c("forecastA_tab_PC1", "forecastA_tab_PC2", "forecastA_tab_PC3", "forecastA_tab_PC4", "forecastA_tab_PC5",
                          "forecastA_tab_PC6", "forecastA_tab_PC7", "forecastA_tab_PC8", "forecastA_tab_PC9", "forecastA_tab_PC10")

# Loop through each forecast table name, modify, and reassign
for (name in forecast_table_names) {
  # Assuming each table variable is in the global environment
  assign(name, `[[<-`(get(name), "FirstDate", value = test_data$FirstDate), envir = .GlobalEnv)
}

# appending findings

forecastA_b_extended<-list(forecastA_tab_PC1,forecastA_tab_PC2,forecastA_tab_PC3,forecastA_tab_PC4,
                  forecastA_tab_PC5,forecastA_tab_PC6,forecastA_tab_PC7,forecastA_tab_PC8,
                  forecastA_tab_PC9,forecastA_tab_PC10)

valuesAPC_b_extended<-as.data.frame(t(cbind(valuesAPC1,valuesAPC2,valuesAPC3,valuesAPC4,valuesAPC5,
                                   valuesAPC6,valuesAPC7,valuesAPC8,valuesAPC9,valuesAPC10))) 

colnames(valuesAPC_b_extended) <- c("PUNEW", "PUXHS", "PUXX", "PCE")

rm(list=setdiff(ls(), c("forecastA_a", "forecastA_b", "valuesAPC_a", "valuesAPC_b",
                        "forecastA_a_extended", "forecastA_b_extended",
                        "valuesAPC_a_extended", "valuesAPC_b_extended")))
