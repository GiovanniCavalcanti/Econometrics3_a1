# 01 Packages and environment -----------------------

rm(list = ls()) 
setwd("C:/Users/Giovanni Cavalcanti/OneDrive - Insper - Institudo de Ensino e Pesquisa/trimestre_4/econometrics 3/replication assignments/replication_a1")

# to install the Bureau of Labor Statistics API
## library(devtools)
## install_github("mikeasilva/blsAPI")

library(tidyverse)
library(dplyr)
library(quantmod)

# 02 Fetch inflation data -----------------------
## Fetch PUNEW
punew_df <- read_csv("punew_1947-2023.csv")
## Fetch PUXHS
puxhs_df <- read_csv("puxhs_1947-2023.csv")
## Fetch PUXX
puxx_df <- read_csv("puxx_1957-2023.csv")

## Fetch PCE
# Specify the start and end dates for the data retrieval
start_date <- as.Date("1959-01-01")
end_date <- as.Date("2023-12-31")

# Fetch the data
getSymbols("PCEC", src = "FRED", from = start_date, to = end_date)

# Access the data
df_pce <- as.data.frame(get("PCEC"))

rm(end_date, start_date, PCEC)

# Define the quartely inflation rate column 

punew_df_test <- punew_df %>%
  
  