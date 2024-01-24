# 01 Packages and environment -----------------------

rm(list = ls()) 

# to install the Bureau of Labor Statistics API
## library(devtools)
## install_github("mikeasilva/blsAPI")

library(tidyverse)
library(dplyr)
library(blsAPI)
library(rjson)
library(quantmod)

# 02 Fetch inflation data -----------------------



# Initialize the list to store data
punew_y <- list()

# Loop by year, in steps of 10 years
for (y in seq(1952, 2023, by = 10)) {
  # Prepare the payload for the API call
  payload_punew <- list(
    'seriesid' = c('CUSR0000SA0'),
    'startyear' = y,
    'endyear' = 2023)
  
  # Make the API call
  response_punew <- blsAPI(payload_punew)
  
  # Parse the JSON response
  json_punew <- fromJSON(response_punew)
  
  # Extract the data and store it in the list
  # Adjust this part according to how your API response is structured and how the apiDF function is defined
  punew_y[[as.character(y)]] <- apiDF(json_punew$Results$series[[1]]$data)
}

# Join all yearly data frames into one
df_punew <- bind_rows(punew_y)

## Fetch PUXHS
payload_puxhs <- list(
  'seriesid'=c('CUSR0000SA0L2'),
  'startyear'=1952,
  'endyear'=2024)
response_puxhs <- blsAPI(payload_puxhs)
json_puxhs <- fromJSON(response_puxhs)
df_puxhs <- apiDF(json_puxhs$Results$series[[1]]$data)

rm(json_puxhs, payload_puxhs, response_puxhs)

## Fetch PUXX
payload_puxx <- list(
  'seriesid'=c('CUSR0000SA0L1E'),
  'startyear'=1958,
  'endyear'=2024)
response_puxx <- blsAPI(payload_puxx)
json_puxx <- fromJSON(response_puxx)
df_puxx <- apiDF(json_puxx$Results$series[[1]]$data)

rm(json_puxx, payload_puxx, response_puxx)

## Fetch PCE
# Specify the start and end dates for the data retrieval
start_date <- as.Date("1960-01-01")
end_date <- as.Date("2023-12-31")

# Fetch the data
getSymbols("PCEC", src = "FRED", from = start_date, to = end_date)

# Access the data
df_pce <- as.data.frame(get("PCEC"))

rm(end_date, start_date, PCEC)