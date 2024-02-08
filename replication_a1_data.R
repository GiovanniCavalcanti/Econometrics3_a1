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

# 02 Load and adjust inflation data -----------------------

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
punew_df <- process_inflation_data("data/punew_1947-2023.csv", "inflation_punew")
puxhs_df <- process_inflation_data("data/puxhs_1947-2023.csv", "inflation_puxhs")
puxx_df <- process_inflation_data("data/puxx_1957-2023.csv", "inflation_puxx")

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

##___________________________________________________________________##
#   Save the df_inflation_complete as df_inflation_complete.csv.      
#   This is the the complete inflation panel!                           
##___________________________________________________________________##
# write.csv(df_inflation_complete, file = "df_inflation_complete.csv")

rm(pce_df, punew_df, puxhs_df, puxx_df)

# Now, we aggregate inflation by year.

punew_year <- df_inflation_complete %>%
  group_by(group) %>%
  summarise(punew_year = sum(inflation_punew, na.rm = TRUE))
puxhs_year <- df_inflation_complete %>%
  group_by(group) %>%
  summarise(puxhs_year = sum(inflation_puxhs, na.rm = TRUE))
puxx_year <- df_inflation_complete %>%
  group_by(group) %>%
  summarise(puxx_year = sum(inflation_puxx, na.rm = TRUE))
pce_year <- df_inflation_complete %>%
  group_by(group) %>%
  summarise(pce_year = sum(inflation_pce, na.rm = TRUE))

df_inflation_complete_yearly <- full_join(punew_year, puxhs_year, by = "group") %>%
  full_join(., puxx_year, by = "group") %>%
  full_join(., pce_year, by = "group") %>%
  mutate(across(everything(), ~na_if(.x, 0)))

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
  group_by(group) %>%
  summarise(punew_year = sum(inflation_punew, na.rm = TRUE))
puxhs_year <- df_inflation_authors %>%
  group_by(group) %>%
  summarise(puxhs_year = sum(inflation_puxhs, na.rm = TRUE))
puxx_year <- df_inflation_authors %>%
  group_by(group) %>%
  summarise(puxx_year = sum(inflation_puxx, na.rm = TRUE))
pce_year <- df_inflation_authors %>%
  group_by(group) %>%
  summarise(pce_year = sum(inflation_pce, na.rm = TRUE))

rm(pce_filter, punew_puxhs_filter, puxx_filter)

df_inflation_authors_yearly <- full_join(punew_year, puxhs_year, by = "group") %>%
  full_join(., puxx_year, by = "group") %>%
  full_join(., pce_year, by = "group") %>%
  mutate(across(everything(), ~na_if(.x, 0)))



##____________________________________________________________________________##
#   df_inflation_authors_yearly is the yearly aggregated inflation     
#   This is the the authors' original yearly inflation panel  
##____________________________________________________________________________##
# write.csv(df_inflation_authors_yearly, file = "df_inflation_authors_yearly.csv")

rm(pce_year, punew_year, puxhs_year, puxx_year)

## 02.1 Recreate table 1 Summary Statistics - original --------------------------

### Panel A --------------------------------------------------

data_variables <- select(df_inflation_authors_yearly, ends_with('year'))

# Calculate summary statistics
means <- 100*round(colMeans(data_variables, na.rm = T),4)
sds <- data_variables %>% summarise(across(everything(), ~ 100* sd(., na.rm=T)))
autocorrelation_quaterly <- df_inflation_authors %>% 
  select(starts_with("inflation")) %>%
  summarise(across(everything(), ~ cor(., lag(., 4), use='complete.obs'))) %>%
  select("punew_year" = "inflation_punew", "puxhs_year" = "inflation_puxhs", "puxx_year" = "inflation_puxx", "pce_year" = "inflation_pce")
corr_table <- round(cor(data_variables, use = 'complete.obs'), 2)
corr_table[!lower.tri(corr_table)] <- NA

panel_a <- bind_rows(list(means, sds, autocorrelation_quaterly, as.data.frame(corr_table)))
  
# New column to add at the start
statistics_labels <- c("Mean", "Standard deviation", "Autocorrelation", "Correlations", "PUXHS", "PUXX", "PCE")

# Adding the new column at the start of the dataframe
panel_a <- panel_a %>%
  mutate(Statistic = statistics_labels) %>%
  select(Statistic, everything()) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Generate the table 1, panel A (latex code)
kable(panel_a, "latex", booktabs = TRUE, align = 'c', col.names = c("", "PUNEW", "PUXHS", "PUXX", "PCE")) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" " = 1, "Panel A: 1952:Q2–2002:Q4" = 4)) %>%
  pack_rows("Mean", 1, 1) %>%
  pack_rows("Standard deviation", 2, 2) %>%
  pack_rows("Autocorrelation", 3, 3) %>%
  pack_rows("Correlations", 4, 6)

rm(panel_a, sds, means, autocorrelation_quaterly, corr_table, data_variables)

### Panel B -----------------------------------------------

df_inflation_authors_B <- df_inflation_authors %>%
  filter(FirstDate >= "1986-01-01" & FirstDate <= "2002-10-01")

punew_year <- df_inflation_authors_B %>%
  group_by(group) %>%
  summarise(punew_year = sum(inflation_punew, na.rm = TRUE))
puxhs_year <- df_inflation_authors_B %>%
  group_by(group) %>%
  summarise(puxhs_year = sum(inflation_puxhs, na.rm = TRUE))
puxx_year <- df_inflation_authors_B %>%
  group_by(group) %>%
  summarise(puxx_year = sum(inflation_puxx, na.rm = TRUE))
pce_year <- df_inflation_authors_B %>%
  group_by(group) %>%
  summarise(pce_year = sum(inflation_pce, na.rm = TRUE))

df_inflation_authors_yearly_B <- full_join(punew_year, puxhs_year, by = "group") %>%
  full_join(., puxx_year, by = "group") %>%
  full_join(., pce_year, by = "group") %>%
  mutate(across(everything(), ~na_if(.x, 0)))

rm(pce_year, punew_year, puxhs_year, puxx_year)

data_variables <- select(df_inflation_authors_yearly_B, ends_with('year'))

means <- 100*round(colMeans(data_variables, na.rm = T),4)
sds <- data_variables %>% summarise(across(everything(), ~ 100* sd(., na.rm=T)))
autocorrelation_quaterly <- df_inflation_authors_B %>% 
  select(starts_with("inflation")) %>%
  summarise(across(everything(), ~ cor(., lag(., 4), use='complete.obs'))) %>%
  select("punew_year" = "inflation_punew", "puxhs_year" = "inflation_puxhs", "puxx_year" = "inflation_puxx", "pce_year" = "inflation_pce")
corr_table <- round(cor(data_variables, use = 'complete.obs'), 2)
corr_table[!lower.tri(corr_table)] <- NA

panel_b <- bind_rows(list(means, sds, autocorrelation_quaterly, as.data.frame(corr_table)))

# Adding the new column at the start of the dataframe
panel_b <- panel_b %>%
  mutate(Statistic = statistics_labels) %>%
  select(Statistic, everything()) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Generate the table 1, panel B (latex code)
kable(panel_b, "latex", booktabs = TRUE, align = 'c', col.names = c("", "PUNEW", "PUXHS", "PUXX", "PCE")) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" " = 1, "Panel B: 1986:Q1– 2002:Q4" = 4)) %>%
  pack_rows("Mean", 1, 1) %>%
  pack_rows("Standard deviation", 2, 2) %>%
  pack_rows("Autocorrelation", 3, 3) %>%
  pack_rows("Correlations", 4, 6)

rm(panel_b, sds, means, autocorrelation_quaterly, corr_table, data_variables)

### Panel C -----------------------------------------------

df_inflation_authors_C <- df_inflation_authors %>%
  filter(FirstDate >= "1996-01-01" & FirstDate <= "2002-10-01")

punew_year <- df_inflation_authors_C %>%
  group_by(group) %>%
  summarise(punew_year = sum(inflation_punew, na.rm = TRUE))
puxhs_year <- df_inflation_authors_C %>%
  group_by(group) %>%
  summarise(puxhs_year = sum(inflation_puxhs, na.rm = TRUE))
puxx_year <- df_inflation_authors_C %>%
  group_by(group) %>%
  summarise(puxx_year = sum(inflation_puxx, na.rm = TRUE))
pce_year <- df_inflation_authors_C %>%
  group_by(group) %>%
  summarise(pce_year = sum(inflation_pce, na.rm = TRUE))

df_inflation_authors_yearly_C <- full_join(punew_year, puxhs_year, by = "group") %>%
  full_join(., puxx_year, by = "group") %>%
  full_join(., pce_year, by = "group") %>%
  mutate(across(everything(), ~na_if(.x, 0)))

rm(pce_year, punew_year, puxhs_year, puxx_year)

data_variables <- select(df_inflation_authors_yearly_C, ends_with('year'))

means <- 100*round(colMeans(data_variables, na.rm = T),4)
sds <- data_variables %>% summarise(across(everything(), ~ 100* sd(., na.rm=T)))
autocorrelation_quaterly <- df_inflation_authors_C %>% 
  select(starts_with("inflation")) %>%
  summarise(across(everything(), ~ cor(., lag(., 4), use='complete.obs'))) %>%
  select("punew_year" = "inflation_punew", "puxhs_year" = "inflation_puxhs", "puxx_year" = "inflation_puxx", "pce_year" = "inflation_pce")
corr_table <- round(cor(data_variables, use = 'complete.obs'), 2)
corr_table[!lower.tri(corr_table)] <- NA

panel_c <- bind_rows(list(means, sds, autocorrelation_quaterly, as.data.frame(corr_table)))

# Adding the new column at the start of the dataframe
panel_c <- panel_c %>%
  mutate(Statistic = statistics_labels) %>%
  select(Statistic, everything()) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Generate the table 1, panel C (latex code)
kable(panel_c, "latex", booktabs = TRUE, align = 'c', col.names = c("", "PUNEW", "PUXHS", "PUXX", "PCE")) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" " = 1, "Panel C: 1996:Q1– 2002:Q4" = 4)) %>%
  pack_rows("Mean", 1, 1) %>%
  pack_rows("Standard deviation", 2, 2) %>%
  pack_rows("Autocorrelation", 3, 3) %>%
  pack_rows("Correlations", 4, 6)


# Clear environment
rm(panel_c, sds, means, autocorrelation_quaterly, corr_table, data_variables)
rm(df_inflation_authors_B, df_inflation_authors_C, df_inflation_authors_yearly_B, df_inflation_authors_yearly_C, statistics_labels)

## 02.2 Recreate table 1 Summary Statistics - complete --------------------------

### Panel A --------------------------------------------------

data_variables <- select(df_inflation_complete_yearly, ends_with('year'))

# Calculate summary statistics
means <- 100*round(colMeans(data_variables, na.rm = T),4)
sds <- data_variables %>% summarise(across(everything(), ~ 100* sd(., na.rm=T)))
autocorrelation_quaterly <- df_inflation_complete %>% 
  select(starts_with("inflation")) %>%
  summarise(across(everything(), ~ cor(., lag(., 4), use='complete.obs'))) %>%
  select("punew_year" = "inflation_punew", "puxhs_year" = "inflation_puxhs", "puxx_year" = "inflation_puxx", "pce_year" = "inflation_pce")
corr_table <- round(cor(data_variables, use = 'complete.obs'), 2)
corr_table[!lower.tri(corr_table)] <- NA

panel_a <- bind_rows(list(means, sds, autocorrelation_quaterly, as.data.frame(corr_table)))

# New column to add at the start
statistics_labels <- c("Mean", "Standard deviation", "Autocorrelation", "Correlations", "PUXHS", "PUXX", "PCE")

# Adding the new column at the start of the dataframe
panel_a <- panel_a %>%
  mutate(Statistic = statistics_labels) %>%
  select(Statistic, everything()) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Generate the table 1, panel A (latex code)
kable(panel_a, "latex", booktabs = TRUE, align = 'c', col.names = c("", "PUNEW", "PUXHS", "PUXX", "PCE")) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" " = 1, "Panel A: 1947:Q1–2023:Q4" = 4)) %>%
  pack_rows("Mean", 1, 1) %>%
  pack_rows("Standard deviation", 2, 2) %>%
  pack_rows("Autocorrelation", 3, 3) %>%
  pack_rows("Correlations", 4, 6)

rm(panel_a, sds, means, autocorrelation_quaterly, corr_table, data_variables)

### Panel B -----------------------------------------------

df_inflation_complete_B <- df_inflation_complete %>%
  filter(FirstDate >= "1986-01-01" & FirstDate <= "2023-10-01")

punew_year <- df_inflation_complete_B %>%
  group_by(group) %>%
  summarise(punew_year = sum(inflation_punew, na.rm = TRUE))
puxhs_year <- df_inflation_complete_B %>%
  group_by(group) %>%
  summarise(puxhs_year = sum(inflation_puxhs, na.rm = TRUE))
puxx_year <- df_inflation_complete_B %>%
  group_by(group) %>%
  summarise(puxx_year = sum(inflation_puxx, na.rm = TRUE))
pce_year <- df_inflation_complete_B %>%
  group_by(group) %>%
  summarise(pce_year = sum(inflation_pce, na.rm = TRUE))

df_inflation_complete_yearly_B <- full_join(punew_year, puxhs_year, by = "group") %>%
  full_join(., puxx_year, by = "group") %>%
  full_join(., pce_year, by = "group") %>%
  mutate(across(everything(), ~na_if(.x, 0)))

rm(pce_year, punew_year, puxhs_year, puxx_year)

data_variables <- select(df_inflation_complete_yearly_B, ends_with('year'))

means <- 100*round(colMeans(data_variables, na.rm = T),4)
sds <- data_variables %>% summarise(across(everything(), ~ 100* sd(., na.rm=T)))
autocorrelation_quaterly <- df_inflation_complete_B %>% 
  select(starts_with("inflation")) %>%
  summarise(across(everything(), ~ cor(., lag(., 4), use='complete.obs'))) %>%
  select("punew_year" = "inflation_punew", "puxhs_year" = "inflation_puxhs", "puxx_year" = "inflation_puxx", "pce_year" = "inflation_pce")
corr_table <- round(cor(data_variables, use = 'complete.obs'), 2)
corr_table[!lower.tri(corr_table)] <- NA

panel_b <- bind_rows(list(means, sds, autocorrelation_quaterly, as.data.frame(corr_table)))

# Adding the new column at the start of the dataframe
panel_b <- panel_b %>%
  mutate(Statistic = statistics_labels) %>%
  select(Statistic, everything()) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Generate the table 1, panel B (latex code)
kable(panel_b, "latex", booktabs = TRUE, align = 'c', col.names = c("", "PUNEW", "PUXHS", "PUXX", "PCE")) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" " = 1, "Panel B: 1986:Q1– 2023:Q4" = 4)) %>%
  pack_rows("Mean", 1, 1) %>%
  pack_rows("Standard deviation", 2, 2) %>%
  pack_rows("Autocorrelation", 3, 3) %>%
  pack_rows("Correlations", 4, 6)

rm(panel_b, sds, means, autocorrelation_quaterly, corr_table, data_variables)

### Panel C -----------------------------------------------

df_inflation_complete_C <- df_inflation_complete %>%
  filter(FirstDate >= "1996-01-01" & FirstDate <= "2023-10-01")

punew_year <- df_inflation_complete_C %>%
  group_by(group) %>%
  summarise(punew_year = sum(inflation_punew, na.rm = TRUE))
puxhs_year <- df_inflation_complete_C %>%
  group_by(group) %>%
  summarise(puxhs_year = sum(inflation_puxhs, na.rm = TRUE))
puxx_year <- df_inflation_complete_C %>%
  group_by(group) %>%
  summarise(puxx_year = sum(inflation_puxx, na.rm = TRUE))
pce_year <- df_inflation_complete_C %>%
  group_by(group) %>%
  summarise(pce_year = sum(inflation_pce, na.rm = TRUE))

df_inflation_complete_yearly_C <- full_join(punew_year, puxhs_year, by = "group") %>%
  full_join(., puxx_year, by = "group") %>%
  full_join(., pce_year, by = "group") %>%
  mutate(across(everything(), ~na_if(.x, 0)))

rm(pce_year, punew_year, puxhs_year, puxx_year)

data_variables <- select(df_inflation_complete_yearly_C, ends_with('year'))

means <- 100*round(colMeans(data_variables, na.rm = T),4)
sds <- data_variables %>% summarise(across(everything(), ~ 100* sd(., na.rm=T)))
autocorrelation_quaterly <- df_inflation_complete_C %>% 
  select(starts_with("inflation")) %>%
  summarise(across(everything(), ~ cor(., lag(., 4), use='complete.obs'))) %>%
  select("punew_year" = "inflation_punew", "puxhs_year" = "inflation_puxhs", "puxx_year" = "inflation_puxx", "pce_year" = "inflation_pce")
corr_table <- round(cor(data_variables, use = 'complete.obs'), 2)
corr_table[!lower.tri(corr_table)] <- NA

panel_c <- bind_rows(list(means, sds, autocorrelation_quaterly, as.data.frame(corr_table)))

# Adding the new column at the start of the dataframe
panel_c <- panel_c %>%
  mutate(Statistic = statistics_labels) %>%
  select(Statistic, everything()) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Generate the table 1, panel C (latex code)
kable(panel_c, "latex", booktabs = TRUE, align = 'c', col.names = c("", "PUNEW", "PUXHS", "PUXX", "PCE")) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" " = 1, "Panel C: 1996:Q1– 2023:Q4" = 4)) %>%
  pack_rows("Mean", 1, 1) %>%
  pack_rows("Standard deviation", 2, 2) %>%
  pack_rows("Autocorrelation", 3, 3) %>%
  pack_rows("Correlations", 4, 6)


# Clear environment
rm(panel_c, sds, means, autocorrelation_quaterly, corr_table, data_variables)
rm(df_inflation_complete_B, df_inflation_complete_C, df_inflation_complete_yearly_B, df_inflation_complete_yearly_C, statistics_labels)


# 03 Load and adjust real activities measures ----------------------------------

## gdpg dataframe
df_gdp_complete <- read.csv("real_measures_data/gdp.csv") %>%
  select("FirstDate" = "DATE", "gdp" = "GDPC1") %>%
  mutate(gdp_lag = lag(gdp, n =1), 
         gdpg = log(gdp / gdp_lag), 
         group = year(FirstDate),
         FirstDate = as.Date(FirstDate)) %>%
  select("FirstDate", "gdpg", "group") %>%
  mutate(quarter = as.yearqtr(FirstDate)) %>%
  na.omit()

## gap1 dataframe ((quadratic) detrended log GDP as a measure of the output gap last period)
df_gap1_complete <- read.csv("real_measures_data/gdp.csv") %>%
  select("FirstDate" = "DATE", "gdp" = "GDPC1") %>%
  mutate(quarter = as.yearqtr(FirstDate), 
         gap1 = log(lag(gdp, n =1))^2,
         group = year(FirstDate),
         FirstDate = as.Date(FirstDate)) %>%
  select("FirstDate", "gap1", "quarter", "group") %>%
  na.omit()

## gap2 dataframe

library(hpfilter) # implements the modified filter for gap2

gap2<- read.csv("real_measures_data/gdp.csv") %>%
  select("gdp" = "GDPC1") 

filter <- hp1(gap2, lambda = 1600)

gap2 <- read.csv("real_measures_data/gdp.csv") %>%
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

a <- readLines("real_measures_data/experimental_leading_indexes/xindex.ASC", skip = 10)
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

a <- readLines("real_measures_data/factor.txt")
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

# Optionally, create a data frame with this date sequence as a column
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
df_unemp_complete <- read.csv("real_measures_data/unemp.csv") %>%
  arrange(DATE) %>%
  mutate(Quarter = as.yearqtr(DATE)) %>%
  group_by(Quarter) %>%
  summarise(
    FirstDate = first(DATE),
    UNRATE = first(UNRATE)) %>%
  mutate(FirstDate = as.Date(FirstDate),
         group = year(FirstDate)) %>%
  select("FirstDate", "unrate" = "UNRATE", "quarter" = "Quarter", "group")

## labor share dataframe
df_lshr_complete <- read.csv("real_measures_data/lshr.csv") %>%
  select("FirstDate" = "DATE", "lshr" = "PRS85006173") %>%
  mutate(group = year(FirstDate),
         FirstDate = as.Date(FirstDate)) %>%
  mutate(quarter = as.yearqtr(FirstDate)) %>%
  select("FirstDate", "lshr", "quarter", "group")

rm(x, xli, xli2, year, a, date_sequence, teste, df_months, factor, line_split)

## join all dataframees
df_realmeasures_complete <- full_join(df_gdp_complete, df_unemp_complete, ) %>%
  full_join(., df_lshr_complete) %>%
  full_join(., df_gap1_complete) %>%
  full_join(., df_gap2_complete) %>%
  full_join(., df_experimentalindex_complete) %>%
  full_join(., df_fac_complete) %>%
  select("FirstDate", "gdpg", "gap1", "gap2", "unrate", "lshr", "xli", "xli2", "fac", "quarter", "group")

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


# 04 Load and adjust survey measures -------------------------------------

# Load survey data from Excel, adjust CPI calculations, and resample bi-quarterly
df_livingston_complete <- read_excel('survey_data/livingston_survey.xlsx', sheet = 'CPI', na = "#N/A") %>%
  mutate(
    liv = if_else(is.na(CPI_12M), NA_real_, (log(CPI_12M) - log(CPI_BP)) * 100),
    Date = as.Date(Date),
    quarter = as.yearqtr(Date)
  ) %>%
  group_by(quarter = as.yearqtr(Date - months(6))) %>% # Direct calculation for bi-quarterly shift
  summarise(liv_year = mean(liv, na.rm = TRUE), .groups = 'drop')

# Load and adjust SPF survey data
df_spf_complete <- read_excel("survey_data/spf_survey.xlsx", na = "#N/A") %>%
  select(year = YEAR, quarter = QUARTER, CPIB) %>%
  unite("quarter", year:quarter, sep = " Q") %>%
  mutate(quarter = as.yearqtr(quarter)) %>%
  rename(spf_year = CPIB)

# Load and adjust Michigan survey data, resample annually
df_michigan_complete <- read.csv("survey_data/michigan_survey_inflation.csv") %>%
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

## 04.1 Recreate figure 1.A -----------------------------

# Multiply the inflation columns by 100

df_livingston_complete <- df_livingston_complete %>%
  mutate(group = year(quarter)) %>%
  group_by(group) %>%
  summarise(liv_year = mean(liv_year, na.rm = TRUE), .groups = 'drop')

data_plot1A <- df_inflation_authors_yearly %>%
  mutate(across(ends_with("year"), ~ .x * 100)) %>%
  full_join(., df_livingston_complete)  %>%
  filter(group < 2003 & group >1951)


# Pivot the data to a long format for plotting with ggplot2
data_plot1A_long <- data_plot1A %>%
  pivot_longer(cols = ends_with("year"), names_to = "inflation_type", values_to = "inflation_value") %>%
  select(group, inflation_type, inflation_value) 

# Define linetypes and shapes based on the provided plot image
line_types <- c("solid", "longdash", "dotted", "dotdash", NA)
shapes <- c(NA, NA, NA, NA, 3) # Only the 'Livingston' series uses a shape, represented by pluses

# Create a named vector to map the inflation types to line1 types
names(line_types) <- unique(data_plot1A_long$inflation_type)
names(shapes) <- unique(data_plot1A_long$inflation_type)

# Plot the data
plot_1a <- ggplot(data_plot1A_long, aes(x = group, y = inflation_value, color = inflation_type, linetype = inflation_type, shape = inflation_type)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = c("black", "black", "black", "black", "black")) +
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shapes) +
  labs(x = "Year", y = "Percentage", title = "Inflation Over Time") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fig1a.pdf", plot_1a, width = 11, height = 8.5)

rm(plot_1a, data_plot1A, data_plot1A_long, line_types, shapes)

## 04.2 Recreate figure 1.B -----------------------------

teste <- df_surveys_complete %>%
  group_by(group) %>%
  summarise(across(everything(), ~ mean(., na.rm=T)))

data_plot1B <- df_inflation_authors_yearly %>%
  mutate(across(ends_with("punew_year"), ~ .x * 100)) %>%
  filter(group >= 1978 & group <= 2002) %>%
  select(group, punew_year) %>%
  left_join(teste)

# Pivot the data to a long format for plotting with ggplot2
data_plot1B_long <- data_plot1B %>%
  pivot_longer(cols = ends_with("year"), names_to = "inflation_type", values_to = "inflation_value") %>%
  select(group, inflation_type, inflation_value)

# Define linetypes and shapes based on the provided plot image
line_types <- c("solid", "longdash", "dotted", "dotdash")
shapes <- c(NA, NA, NA, NA) # Only the 'Livingston' series uses a shape, represented by pluses

# Create a named vector to map the inflation types to linetypes
names(line_types) <- unique(data_plot1B_long$inflation_type)
names(shapes) <- unique(data_plot1B_long$inflation_type)

# Plot the data
plot_1B <- ggplot(data_plot1B_long, aes(x = group, y = inflation_value, color = inflation_type, linetype = inflation_type, shape = inflation_type)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = c("black", "black", "black", "black")) +
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shapes) +
  labs(x = "Year", y = "Percentage", title = "Inflation Over Time") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fig1b.pdf", plot_1B, width = 11, height = 8.5)

rm(plot_1B, data_plot1B, data_plot1B_long, line_types, shapes)



## 04.3 Recreate figure 1.A (extended) -----------------------------

# Multiply the inflation columns by 100

data_plot1A <- df_inflation_complete_yearly %>%
  mutate(across(ends_with("year"), ~ .x * 100)) %>%
  full_join(., df_livingston_complete)


# Pivot the data to a long format for plotting with ggplot2
data_plot1A_long <- data_plot1A %>%
  pivot_longer(cols = ends_with("year"), names_to = "inflation_type", values_to = "inflation_value") %>%
  select(group, inflation_type, inflation_value) 

# Define linetypes and shapes based on the provided plot image
line_types <- c("solid", "longdash", "dotted", "dotdash", NA)
shapes <- c(NA, NA, NA, NA, 3) # Only the 'Livingston' series uses a shape, represented by pluses

# Create a named vector to map the inflation types to linetypes
names(line_types) <- unique(data_plot1A_long$inflation_type)
names(shapes) <- unique(data_plot1A_long$inflation_type)

# Plot the data
plot_1a <- ggplot(data_plot1A_long, aes(x = group, y = inflation_value, color = inflation_type, linetype = inflation_type, shape = inflation_type)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = c("black", "black", "black", "black", "black")) +
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shapes) +
  labs(x = "Year", y = "Percentage", title = "Inflation Over Time") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fig1a_extended.pdf", plot_1a, width = 11, height = 8.5)

rm(plot_1a, data_plot1A, data_plot1A_long, line_types, shapes)


## 04.2 Recreate figure 1.B (extended)-----------------------------

data_plot1B <- df_inflation_complete_yearly %>%
  mutate(across(ends_with("punew_year"), ~ .x * 100)) %>%
  select(group, punew_year) %>%
  left_join(teste)

# Pivot the data to a long format for plotting with ggplot2
data_plot1B_long <- data_plot1B %>%
  pivot_longer(cols = ends_with("year"), names_to = "inflation_type", values_to = "inflation_value") %>%
  select(group, inflation_type, inflation_value)

# Define linetypes and shapes based on the provided plot image
line_types <- c("solid", "longdash", "dotted", "dotdash")
shapes <- c(NA, NA, NA, NA) # Only the 'Livingston' series uses a shape, represented by pluses

# Create a named vector to map the inflation types to linetypes
names(line_types) <- unique(data_plot1B_long$inflation_type)
names(shapes) <- unique(data_plot1B_long$inflation_type)

# Plot the data
plot_1B <- ggplot(data_plot1B_long, aes(x = group, y = inflation_value, color = inflation_type, linetype = inflation_type, shape = inflation_type)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = c("black", "black", "black", "black")) +
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shapes) +
  labs(x = "Year", y = "Percentage", title = "Inflation Over Time") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fig1b_extended.pdf", plot_1B, width = 11, height = 8.5)

rm(plot_1B, data_plot1B, data_plot1B_long, line_types, shapes, teste)

# Save dataframes as csv
write.csv(df_experimentalindex_complete, file = "df_experimentalindex_complete.csv")
write.csv(df_fac_complete, file = "df_fac_complete.csv")
write.csv(df_inflation_authors, file = "df_inflation_authors.csv")
write.csv(df_inflation_authors_yearly, file = "df_inflation_authors_yearly.csv")
write.csv(df_inflation_complete, file = "df_inflation_complete.csv")
write.csv(df_inflation_complete_yearly, file = "df_inflation_complete_yearly.csv")
write.csv(df_livingston_complete, file = "df_livingston_complete.csv")
write.csv(df_michigan_complete, file = "df_michigan_complete.csv")
write.csv(df_realmeasures_authors, file = "df_realmeasures_authors.csv")
write.csv(df_experimentalindex_complete, file = "df_experimentalindex_complete.csv")
write.csv(df_realmeasures_complete, file = "df_realmeasures_complete.csv")
write.csv(df_spf_complete, file = "df_spf_complete.csv")
write.csv(df_surveys_complete, file = "df_surveys_complete.csv")





