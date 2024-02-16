# 01 Environment and packages --------------------------------------------------
# Clear the workspace to ensure a clean environment for the analysis.
rm(list = ls())

# Load necessary R packages for data manipulation, visualization, and analysis.
library(tidyverse)    # Collection of packages for data science (includes dplyr, ggplot2, etc.)
library(dplyr)        # Data manipulation
library(quantmod)     # Quantitative financial modelling framework
library(forecast)     # Forecasting functions for time series
library(lubridate)    # Date and time manipulation
library(gt)           # Creating tables
library(tibble)       # Modern version of data frames
library(knitr)        # Dynamic reporting
library(kableExtra)   # Enhancements for knitr::kable() output
library(readxl)       # Read Excel files
library(GetBCBData)   # Fetch data from the Brazilian Central Bank
library(rbcb)         # Access Brazilian Central Bank Web Services


# 02 Load and adjust inflation data --------------------------------------------

# For retrieving specific series by ID from the Brazilian Central Bank, visit:
# https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries
# Use the provided link to locate series by their code numbers.

## Extract inflation data
# Define IDs for the IPCA and EXFE series.
my.id <- c('ipca' = 433, 'exfe' = 28751)
# Fetch series data within a 30-year range from today.
df <- gbcbd_get_series(my.id, cache.path = tempdir(),
                       first.date = Sys.Date() - 30 * 365,
                       last.date = Sys.Date())
# Process the fetched data into a cleaner format.
df_inflation_brazil <- df %>%
  select(!id.num) %>%
  pivot_wider(names_from = series.name, values_from = value)

## Extract real economic measures
# Define IDs for GDP growth, two unemployment measures, and labor participation.
my.id <- c('gdpg' = 4380, 'unemp_desat' = 1620, 'unemp_pnad' = 24369, "lbr_part" = 28544)
# Fetch and process real measures data similar to inflation data.
df <- gbcbd_get_series(my.id, cache.path = tempdir(),
                       first.date = Sys.Date() - 30 * 365,
                       last.date = Sys.Date())
df_realmeasures_brazil <- df %>%
  select(!id.num) %>%
  pivot_wider(names_from = series.name, values_from = value)

## Extract surveys on economic indicators
# Define the economic indicators of interest.
indic <- c("IPCA", "IGP-DI", "IGP-M", "INPC")
# Fetch survey data for the defined indicators.
df_surveys_brazil <- get_annual_market_expectations(indic) %>%
  select(indic, FirstDate = date, reference_date, median) %>%
  pivot_wider(names_from = indic, values_from = median, names_prefix = "indic_", values_fn = mean) %>%
  filter(reference_date == year(FirstDate) + 1) %>%
  filter(day(FirstDate) == 1 & # Filter for surveys taken on the first day of the month
           month(FirstDate) %in% c(1, 4, 7, 10)) # and in January, April, July, or October.
