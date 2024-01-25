# 01 Packages and envirnment -----------------------

rm(list = ls()) 

# to install the Bureau of Labor Statistics API
library(devtools)
install_github("mikeasilva/blsAPI")

library(tidyverse)
library(dplyr)
library(blsAPI)
library(rjson)
library(quantmod)

# 02 Fetch inflation data -----------------------



## Fetch PUNEW
punew_y <- list()
# loop by year
for (y in 1947:2023) {
  payload_punew <- list(
    'seriesid'=c('CUSR0000SA0'),
    'startyear'=y,
    'endyear'=y)
  response_punew <- blsAPI(payload_punew)
  json_punew <- fromJSON(response_punew)
  punew_y[[as.character(y)]] <- apiDF(json_punew$Results$series[[1]]$data)
}

# join all years in all df
df_punew <- bind_rows(punew_y)

# test
payload_punew <- list(
  'seriesid'=c('CUSR0000SA0'),
  'startyear'=2010,
  'endyear'=2013)
response_punew <- blsAPI(payload_punew)
json_punew <- fromJSON(response_punew)
teste <- apiDF(json_punew$Results$series[[1]]$data)


rm(json_punew, payload_punew, response_punew)

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