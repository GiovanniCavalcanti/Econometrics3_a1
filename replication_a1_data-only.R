# 00 Packages and environment -----------------------

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

# 01 Load and adjust inflation data -----------------------

# function to process inflation data
# file_path = file_path, ex: "~/data/punew_1947-2023.csv"
# inflation_label = series_name, ex: "inflation_punew"

process_inflation_data <- function(file_path, inflation_label, period_column = "Period", label_column = "Label", value_column = "Value") {
  df <- read_csv(file_path) %>%
    select(Period = period_column, Label = label_column, P = value_column) %>%
    mutate(
      DATE = dmy(paste("01", substr(Period, 2, 3), substr(Label, 1, 4), sep = "-")), # Generate DATE column early to use for arrangement
      P_lag = lag(P, n =3), # Create a lagged version of the price column
      !!inflation_label := log(P / P_lag) # Dynamically name the inflation column
    ) %>%
    na.omit() %>%
    arrange(DATE) %>%
    mutate(Quarter = paste(year(DATE), quarter(DATE), sep = "-Q")) %>%
    group_by(Quarter) %>%
    summarise(
      FirstDate = first(DATE),
      FirstP = first(P),
      FirstP_lag = first(P_lag),
      !!inflation_label := first(!!sym(inflation_label))
    ) %>%
    select(Quarter, FirstDate, FirstP, FirstP_lag, !!inflation_label)
}

# Process each dataset
punew_df <- process_inflation_data("data/input/inflation_data/punew_1947-2023.csv", "inflation_punew")
puxhs_df <- process_inflation_data("data/input/inflation_data/puxhs_1947-2023.csv", "inflation_puxhs")
puxx_df <- process_inflation_data("data/input/inflation_data/puxx_1957-2023.csv", "inflation_puxx")

rm(process_inflation_data)

pce_df <- read_csv("data/pce_DPCERD3Q086SBEA_1947-2023.csv") %>%
  rename(P = "DPCERD3Q086SBEA") %>%
  mutate(
    P_lag = lag(P),
    inflation_pce = log(P / P_lag),
    FirstDate = DATE
  ) %>%
  na.omit()


# Merge all dataframes on the FirstDate column
df_inflation_complete <- reduce(list(punew_df, puxhs_df, puxx_df, pce_df), full_join, by = "FirstDate") %>%
  select("FirstDate", contains("inflation")) %>%
  mutate(quarter = as.yearqtr(FirstDate)) %>%
  mutate(group = year(FirstDate))

# calebe: thank you for noting this!
##___________________________________________________________________##
#   Save the df_inflation_complete as df_inflation_complete.csv.      
#   This is the the complete inflation panel!                           
##___________________________________________________________________##
# write.csv(df_inflation_complete, file = "df_inflation_complete.csv")

rm(pce_df, punew_df, puxhs_df, puxx_df)

# Now, we aggregate inflation by year.

punew_year <- df_inflation_complete %>%
  mutate(punew_year = inflation_punew + lag(inflation_punew, 1) +
           lag(inflation_punew, 2) + lag(inflation_punew, 3)) %>%
  select(!contains("inflation"))
puxhs_year <- df_inflation_complete %>%
  mutate(puxhs_year = inflation_puxhs + lag(inflation_puxhs, 1) +
           lag(inflation_puxhs, 2) + lag(inflation_puxhs, 3)) %>%
  select(!contains("inflation"))
puxx_year <- df_inflation_complete %>%
  mutate(puxx_year = inflation_puxx + lag(inflation_puxx, 1) +
           lag(inflation_puxx, 2) + lag(inflation_puxx, 3)) %>%
  select(!contains("inflation"))
pce_year <- df_inflation_complete %>%
  mutate(pce_year = inflation_pce + lag(inflation_pce, 1) +
           lag(inflation_pce, 2) + lag(inflation_pce, 3)) %>%
  select(!contains("inflation"))

df_inflation_complete_yearly <- full_join(punew_year, puxhs_year ) %>%
  full_join(., puxx_year ) %>%
  full_join(., pce_year ) 

##____________________________________________________________________________##
#   df_inflation_complete_yearly is the yearly aggregated inflation     
#   This is the complete yearly inflation panel  
##____________________________________________________________________________##
# write.csv(df_inflation_complete_yearly, file = "df_inflation_complete_yearly.csv")

rm(pce_year, punew_year, puxhs_year, puxx_year)

## 02.0 Recreate Authors' inflation panel -------------------------------------------

##______________________________________________________________________________##
# To recreate Authors' inflation panel, it requires to filter the dates accordingly
# Their sample period is:
#- 1952:Q2–2002:Q4 for PUNEW and PUXHS,
#- 1958:Q2–2002:Q4 for PUXX,
#- 1960:Q2–2002:Q4 for PCE
##______________________________________________________________________________##

# Filter the data based on the specified date ranges for each series
punew_puxhs_filter <- filter(df_inflation_complete, FirstDate >= as.Date("1952-04-01") & FirstDate <= as.Date("2002-10-01")) %>%
  select("FirstDate", "inflation_punew", "inflation_puxhs", "quarter")
puxx_filter <- filter(df_inflation_complete, FirstDate >= as.Date("1958-04-01") & FirstDate <= as.Date("2002-10-01")) %>%
  select("FirstDate", "inflation_puxx", "quarter")
pce_filter <- filter(df_inflation_complete, FirstDate >= as.Date("1960-04-01") & FirstDate <= as.Date("2002-10-01")) %>%
  select("FirstDate", "inflation_pce", "quarter")

# Now, join those dataframes
df_inflation_authors <- full_join(punew_puxhs_filter, puxx_filter, by = "FirstDate") %>%
  full_join(., pce_filter, by = "FirstDate")  %>%
  mutate(group = year(FirstDate)) %>%
  select("FirstDate", "inflation_punew", "inflation_puxhs", "inflation_puxx", "inflation_pce", "quarter" = "quarter.x", "group") 

##____________________________________________________________________________##
#   Save the df_inflation_authors as original_inflation_panel.csv.  
#   This is the the authors' original inflation panel        
##____________________________________________________________________________##
# write.csv(df_inflation_authors, file = "df_inflation_authors.csv")

# Now, we agregate inflation by year.

punew_year <- df_inflation_authors %>%
  mutate(punew_year = inflation_punew + lag(inflation_punew, 1) +
           lag(inflation_punew, 2) + lag(inflation_punew, 3)) %>%
  select(!contains("inflation"))
puxhs_year <- df_inflation_complete %>%
  mutate(puxhs_year = inflation_puxhs + lag(inflation_puxhs, 1) +
           lag(inflation_puxhs, 2) + lag(inflation_puxhs, 3)) %>%
  select(!contains("inflation"))
puxx_year <- df_inflation_complete %>%
  mutate(puxx_year = inflation_puxx + lag(inflation_puxx, 1) +
           lag(inflation_puxx, 2) + lag(inflation_puxx, 3)) %>%
  select(!contains("inflation"))
pce_year <- df_inflation_complete %>%
  mutate(pce_year = inflation_pce + lag(inflation_pce, 1) +
           lag(inflation_pce, 2) + lag(inflation_pce, 3)) %>%
  select(!contains("inflation"))

df_inflation_authors_yearly <- full_join(punew_year, puxhs_year ) %>%
  full_join(., puxx_year ) %>%
  full_join(., pce_year ) 
##____________________________________________________________________________##
#   df_inflation_authors_yearly is the yearly aggregated inflation     
#   This is the the authors' original yearly inflation panel  
##____________________________________________________________________________##
# write.csv(df_inflation_authors_yearly, file = "df_inflation_authors_yearly.csv")

rm(pce_year, punew_year, puxhs_year, puxx_year)


# 02 Load and adjust real activities measures ----------------------------------

## gdpg dataframe
df_gdp_complete <- read_csv("data/input/real_measures_data/gdp.csv") %>%
  select("FirstDate" = "DATE", "gdp" = "GDPC1") %>%
  mutate(gdp_lag = lag(gdp, n=1), 
         gdpg = log(gdp / gdp_lag), 
         group = year(FirstDate),
         FirstDate = as.Date(FirstDate)) %>%
  select("FirstDate", "gdpg", "group") %>%
  mutate(quarter = as.yearqtr(FirstDate)) %>%
  na.omit()

## gap1 dataframe ((quadratic) detrended log GDP as a measure of the output gap last period)
df_gap1_complete <- read.csv("data/input/real_measures_data/gdp.csv") %>%
  select("FirstDate" = "DATE", "gdp" = "GDPC1") %>%
  mutate(FirstDate = as.Date(FirstDate)) %>%
  mutate(quarter = as.yearqtr(FirstDate), 
         gap1 = log(lag(gdp, n =1))^2,
         group = year(FirstDate),
         FirstDate = as.Date(FirstDate)) %>%
  select("FirstDate", "gap1", "quarter", "group") %>%
  na.omit()

## gap2 dataframe

library(hpfilter) # implements the modified filter for gap2

gap2 <- read_csv("data/input/real_measures_data/gdp.csv") %>%
  select("gdp" = "GDPC1") 

filter <- hp1(gap2, lambda = 1600)

gap2 <- read.csv("data/input/real_measures_data/gdp.csv") %>%
  select("FirstDate" = "DATE", "gdp" = "GDPC1")

gap2$gap_2 <- filter

df_gap2_complete <- gap2 %>%
  select("FirstDate" = as.character("FirstDate"), "gap2" = "gap_2") %>%
  unnest(gap2) %>%
  mutate(FirstDate = as.Date(FirstDate),
         gap2 = gdp,
         quarter = as.yearqtr(FirstDate),
         group = year(FirstDate)) %>%
  select(!gdp)

rm(gap2, filter)

## li/lix dataframes

a <- readLines("data/input/real_measures_data/experimental_leading_indexes/xindex.ASC", skip = 10)
a <- a[11:550]


str_split(a[50], "\\s+")

year <- c()

for (x in 1:length(a)) {
  line_split <- str_split(a[x], "\\s+")
  print(line_split)
  year[x] <- line_split[[1]][2]
}

xli <- c()

for (x in 1:length(a)) {
  line_split <- str_split(a[x], "\\s+")
  print(line_split)
  xli[x] <- line_split[[1]][4]
}

xli2 <- c()

for (x in 1:length(a)) {
  line_split <- str_split(a[x], "\\s+")
  print(line_split)
  xli2[x] <- line_split[[1]][8]
}

teste <- data.frame(year, xli, xli2)

df_experimentalindex_complete <- teste[-1,]

df_experimentalindex_complete <- df_experimentalindex_complete %>% 
  mutate(FirstDate = dmy(paste("01", substr(year, 6, 7), substr(year, 1, 4), sep = "-")),
         xli = as.numeric(xli),
         xli2 = as.numeric(xli2)) %>%
  mutate(quarter = as.yearqtr(FirstDate),
         group = year(FirstDate)) %>%
  select(FirstDate, xli, xli2, quarter, group) %>%
  group_by(quarter) %>%
  summarise(
    FirstDate = first(FirstDate), 
    xli = first(xli),
    xli2 = first(xli2),
    group = first(group)
    )

rm(teste, line_split, a, x)

## fac dataframe

a <- readLines("data/input/real_measures_data/factor.txt")
a <- a[3:513]

factor <- c()

for (x in 1:length(a)) {
  line_split <- str_split(a[x], "\\s+")
  print(line_split)
  factor[x] <- line_split[[1]][2]
}

teste <- data.frame(as.numeric(factor)) 

# Generate the sequence of dates by month
date_sequence <- seq.Date(from = as.Date("1959-02-01"), to = as.Date("2001-08-01"), by = "month")

# Create a data frame with this date sequence as a column
df_months <- data.frame(FirstDate = date_sequence)

df_fac_complete <- bind_cols(teste, df_months) %>%
  mutate(fac = as.numeric.factor.) %>%
  mutate(quarter = as.yearqtr(FirstDate),
         group = year(FirstDate)) %>%
  select(FirstDate, fac, quarter, group) %>%
  group_by(quarter) %>%
  summarise(
    FirstDate = first(FirstDate), 
    fac = first(fac),
    group = first(group)
  )

## unemployment  dataframe
df_unemp_complete <- read.csv("data/input/real_measures_data/unemp.csv") %>%
  arrange(DATE) %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(Quarter = as.yearqtr(DATE)) %>%
  group_by(Quarter) %>%
  summarise(
    FirstDate = first(DATE),
    UNRATE = first(UNRATE)) %>%
  mutate(FirstDate = as.Date(FirstDate),
         group = year(FirstDate)) %>%
  select("FirstDate", "unrate" = "UNRATE", "quarter" = "Quarter", "group")

## labor share dataframe
df_lshr_complete <- read.csv("data/input/real_measures_data/lshr.csv") %>%
  select("FirstDate" = "DATE", "lshr" = "PRS85006173") %>%
  mutate(group = year(FirstDate),
         FirstDate = as.Date(FirstDate)) %>%
  mutate(quarter = as.yearqtr(FirstDate)) %>%
  select("FirstDate", "lshr", "quarter", "group")

rm(x, xli, xli2, year, a, date_sequence, teste, df_months, factor, line_split)

##fb_rate dataframe 

a <- readLines("data/input/real_measures_data/FBrate.txt")
a <- a[3:609]

factor <- c()

for (x in 1:length(a)) {
  line_split <- str_split(a[x], "\\s+")
  print(line_split)
  factor[x] <- line_split[[1]][2]
}

teste <- data.frame(as.numeric(factor)) 

# Generate the sequence of dates by month
date_sequence <- seq.Date(from = as.Date("1952-07-01"), to = as.Date("2003-01-01"), by = "month")

# Create a data frame with this date sequence as a column
df_months <- data.frame(FirstDate = date_sequence)

df_rate_complete <- bind_cols(teste, df_months) %>%
  mutate(rate = as.numeric.factor.) %>%
  mutate(quarter = as.yearqtr(FirstDate),
         group = year(FirstDate)) %>%
  select(FirstDate, rate, quarter, group) %>%
  group_by(quarter) %>%
  summarise(
    FirstDate = first(FirstDate), 
    rate = first(rate),
    group = first(group)
  )

##fb_yield dataframe 

a <- readLines("data/input/real_measures_data/FByield.txt")
a <- a[3:609]

factor <- c()

for (x in 1:length(a)) {
  line_split <- str_split(a[x], "\\s+")
  print(line_split)
  factor[x] <- line_split[[1]][2]
}

teste <- data.frame(as.numeric(factor)) 

# Generate the sequence of dates by month
date_sequence <- seq.Date(from = as.Date("1952-07-01"), to = as.Date("2003-01-01"), by = "month")

# Create a data frame with this date sequence as a column
df_months <- data.frame(FirstDate = date_sequence)

df_yield_complete <- bind_cols(teste, df_months) %>%
  mutate(yield = as.numeric.factor.) %>%
  mutate(quarter = as.yearqtr(FirstDate),
         group = year(FirstDate)) %>%
  select(FirstDate, yield, quarter, group) %>%
  group_by(quarter) %>%
  summarise(
    FirstDate = first(FirstDate), 
    yield = first(yield),
    group = first(group)
  )

## join all dataframees
df_realmeasures_complete <- full_join(df_gdp_complete, df_unemp_complete, ) %>%
  full_join(., df_lshr_complete) %>%
  full_join(., df_gap1_complete) %>%
  full_join(., df_gap2_complete) %>%
  full_join(., df_experimentalindex_complete) %>%
  full_join(., df_fac_complete) %>%
  full_join(., df_rate_complete) %>%
  full_join(., df_yield_complete) %>%
  select("FirstDate", "gdpg", "gap1", "gap2", "unrate", "lshr", "xli", "xli2", "fac", "rate", "yield", "quarter", "group")

df_realmeasures_complete <- df_realmeasures_complete[1:(nrow(df_realmeasures_complete) - 2),]

##____________________________________________________________________________##
#   df_realmeasures_complete is the quarterly real measures data    
#   This is the complete quarterly real measures data 
##____________________________________________________________________________##
# write.csv(df_realmeasures_complete, file = "df_realmeasures_complete.csv")

rm(df_gdp_complete, df_unemp_complete, df_lshr_complete, df_gap1_complete, df_gap2_complete)

## 0.3.0 Recreate Authors' Real measures panel

##______________________________________________________________________________##
# To recreate Authors' real measures panel, it requires to filter the dates accordingly
# Their sample period is:
#- 1952:Q2–2001:Q4 for all the real activity measures, except
#- 1959:Q1–2001:Q3 for Bernanke–Boivin–Eliasz real activity factor
##______________________________________________________________________________##

df_realmeasures_authors <- filter(df_realmeasures_complete, FirstDate >= as.Date("1952-04-01") & FirstDate <= as.Date("2001-10-01")) 

##____________________________________________________________________________##
#   df_realmeasures_authors is the quarterly real measures data    
#   This is the Authors' quarterly real measures data 
##____________________________________________________________________________##
# write.csv(df_realmeasures_authors, file = "df_realmeasures_authors.csv")

# 03 Load and adjust survey measures -------------------------------------

# Load survey data from Excel, adjust CPI calculations, and resample bi-quarterly
df_livingston_complete <- read_excel('data/input/survey_data/livingston_survey.xlsx', sheet = 'CPI', na = "#N/A") %>%
  mutate(
    liv_year = if_else(is.na(CPI_12M), NA_real_, (log(CPI_12M) - log(CPI_BP)) * 100),
    Date = as.Date(Date),
    quarter = as.yearqtr(Date),
    group = year(Date)
  ) %>%
  select(quarter, group, liv_year)

# Load and adjust SPF survey data
df_spf_complete <- read_excel("data/input/survey_data/spf_survey.xlsx", na = "#N/A") %>%
  select(year = YEAR, quarter = QUARTER, CPIB) %>%
  unite("quarter", year:quarter, sep = " Q") %>%
  mutate(quarter = as.yearqtr(quarter)) %>%
  rename(spf_year = CPIB)

# Load and adjust Michigan survey data, resample annually
df_michigan_complete <- read.csv("data/input/survey_data/michigan_survey_inflation.csv") %>%
  rename(date = DATE) %>%
  mutate(
    date = as.Date(date),
    quarter = as.yearqtr(date)) %>%
  group_by(quarter = as.yearqtr(date - months(12))) %>% # Shift by 4 quarters (1 year) directly
  summarise(mich_year = mean(MICH, na.rm = TRUE), .groups = 'drop')

df_surveys_complete <- full_join(df_livingston_complete, df_michigan_complete) %>%
  full_join(df_spf_complete) %>%
  mutate(group = year(quarter)) %>%
  arrange(quarter)

# Save all created dataframes as csv
write.csv(df_experimentalindex_complete, file = "data/output/df_experimentalindex_complete.csv")
write.csv(df_fac_complete, file = "data/output/df_fac_complete.csv")
write.csv(df_inflation_authors, file = "data/output/df_inflation_authors.csv")
write.csv(df_inflation_authors_yearly, file = "data/output/df_inflation_authors_yearly.csv")
write.csv(df_inflation_complete, file = "data/output/df_inflation_complete.csv")
write.csv(df_inflation_complete_yearly, file = "data/output/df_inflation_complete_yearly.csv")
write.csv(df_livingston_complete, file = "data/output/df_livingston_complete.csv")
write.csv(df_michigan_complete, file = "data/output/df_michigan_complete.csv")
write.csv(df_realmeasures_authors, file = "data/output/df_realmeasures_authors.csv")
write.csv(df_experimentalindex_complete, file = "data/output/df_experimentalindex_complete.csv")
write.csv(df_realmeasures_complete, file = "data/output/df_realmeasures_complete.csv")
write.csv(df_spf_complete, file = "data/output/df_spf_complete.csv")
write.csv(df_surveys_complete, file = "data/output/df_surveys_complete.csv")





