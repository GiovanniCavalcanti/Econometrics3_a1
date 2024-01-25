# 01 Packages and environment -----------------------

rm(list = ls()) 

library(tidyverse)
library(dplyr)
library(quantmod)
library(forecast)
library(lubridate)
library(gt)

# 02 Load and adjust inflation data -----------------------

process_inflation_data <- function(file_path, inflation_label, period_column = "Period", label_column = "Label", value_column = "Value") {
  df <- read_csv(file_path) %>%
    select(Period = period_column, Label = label_column, P = value_column) %>%
    mutate(
      DATE = dmy(paste("01", substr(Period, 2, 3), substr(Label, 1, 4), sep = "-")), # Generate DATE column early to use for arrangement
      P_lag = lag(P, n =2), # Create a lagged version of the price column
      !!inflation_label := log(P / P_lag) # Dynamically name the inflation column
    ) %>%
    na.omit() %>%
    arrange(DATE) %>%
    mutate(Quarter = paste(year(DATE), quarter(DATE), sep = "-Q")) %>%
    group_by(Quarter) %>%
    summarise(
      LastDate = last(DATE),
      LastP = last(P),
      LastP_lag = last(P_lag),
      !!inflation_label := last(!!sym(inflation_label))
    ) %>%
    select(Quarter, LastDate, LastP, LastP_lag, !!inflation_label)
}

# Process each dataset
punew_df <- process_inflation_data("punew_1947-2023.csv", "inflation_punew")
puxhs_df <- process_inflation_data("puxhs_1947-2023.csv", "inflation_puxhs")
puxx_df <- process_inflation_data("puxx_1957-2023.csv", "inflation_puxx")

rm(process_inflation_data)

pce_df <- read_csv("pce_DPCERD3Q086SBEA_1947-2023.csv") %>%
  rename(P = "DPCERD3Q086SBEA") %>%
  mutate(
    P_lag = lag(P),
    inflation_pce = log(P / P_lag),
    LastDate = DATE %m-% months(1) # Subtract 3 months directly here
  )


# Merge all dataframes on the DATE column
merged_df <- reduce(list(punew_df, puxhs_df, puxx_df, pce_df), full_join, by = "LastDate") %>%
  select("LastDate", contains("inflation")) %>%
  mutate(quarter = paste(year(LastDate), quarter(LastDate), sep = "-Q")) 

# write.csv(merged_df, file = "inflation_panel.csv")

rm(pce_df, punew_df, puxhs_df, puxx_df)

## 02.1 Recreate table 1 Summary Statistics - original --------------------------

# Filter the data based on the specified date ranges for each series
punew_puxhs_filter <- filter(merged_df, LastDate >= as.Date("1952-04-01") & LastDate <= as.Date("2003-01-01")) %>%
  select("LastDate", "inflation_punew", "inflation_puxhs", "quarter")
puxx_filter <- filter(merged_df, LastDate >= as.Date("1958-04-01") & LastDate <= as.Date("2003-01-01")) %>%
  select("LastDate", "inflation_puxx", "quarter")
pce_filter <- filter(merged_df, LastDate >= as.Date("1960-04-01") & LastDate <= as.Date("2003-01-01")) %>%
  select("LastDate", "inflation_pce", "quarter")

# Join the first two dataframes
joined_df <- full_join(punew_puxhs_filter, puxx_filter, by = "LastDate")

# Now join the third dataframe with the result of the first join
original_df <- full_join(joined_df, pce_filter, by = "LastDate") %>%
  mutate(sequence = row_number()) %>%
  mutate(group = 1951+(sequence - 1) %/% 4 + 1) %>%
  select("LastDate", "inflation_punew", "inflation_puxhs", "inflation_puxx", "inflation_pce", "quarter" = "quarter.x", "group") 

punew_year <- original_df %>%
  group_by(group) %>%
  summarise(punew_year = sum(inflation_punew, na.rm = TRUE))
puxhs_year <- original_df %>%
  group_by(group) %>%
  summarise(puxhs_year = sum(inflation_puxhs, na.rm = TRUE))
puxx_year <- original_df %>%
  group_by(group) %>%
  summarise(puxx_year = sum(inflation_puxx, na.rm = TRUE))
pce_year <- original_df %>%
  group_by(group) %>%
  summarise(pce_year = sum(inflation_pce, na.rm = TRUE))

rm(joined_df, pce_filter, punew_puxhs_filter, puxx_filter)

original_year_df <- full_join(punew_year, puxhs_year, by = "group") %>%
  full_join(., puxx_year, by = "group") %>%
  full_join(., pce_year, by = "group") %>%
  mutate(across(everything(), ~na_if(.x, 0)))
  
rm(pce_year, punew_year, puxhs_year, puxx_year)

# Calculate the mean, standard deviation for each series
summary_stats <- original_year_df %>%
  summarise(across(c(punew_year, puxhs_year, puxx_year, pce_year), list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  )))
