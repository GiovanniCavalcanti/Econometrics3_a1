# 01 Environment and packages --------------------------------------------------
# Clear the workspace to ensure a clean environment for the analysis.

# Load necessary R packages for data manipulation, visualization, and analysis.
# These libraries provide a wide range of functions for data processing, financial modeling,
# time series forecasting, working with dates, creating tables, dynamic reporting, and more.
library(tidyverse)    # Collection of data science packages, including dplyr for data manipulation and ggplot2 for plotting.
library(dplyr)        # Data manipulation tools for filtering, selecting, and transforming data.
library(quantmod)     # Tools for quantitative financial modeling and trading strategy development.
library(forecast)     # Functions for forecasting time series data.
library(lubridate)    # Simplifies working with dates and times.
library(gt)           # Enables creation of beautiful and customizable tables.
library(tibble)       # Provides a modern reimagining of data frames.
library(knitr)        # Allows for dynamic report generation in R.
library(kableExtra)   # Enhances knitr::kable() outputs with additional styling and functionality.
library(readxl)       # Enables reading Excel files directly into R.
library(GetBCBData)   # Fetches data from the Brazilian Central Bank (BCB).
library(rbcb)         # Provides access to BCB's web services for economic data.

# 02 Load and adjust inflation data --------------------------------------------

# For retrieving specific series by ID from the Brazilian Central Bank, visit:
# https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries
# Use the provided link to locate series by their code numbers.

## Extract inflation data
# Define IDs for the IPCA and EXFE series.
my.id <- c('ipca' = 433, 'ipca_15' = 7478, 'exfe' = 28751)
# Fetch series data within a 30-year range from today.
df <- gbcbd_get_series(my.id, cache.path = tempdir(),
                       first.date = Sys.Date() - 30 * 365,
                       last.date = Sys.Date())
# Process the fetched data into a cleaner format.
df_inflation_brazil <- df %>%
  select(!id.num) %>%
  pivot_wider(names_from = series.name, values_from = value)

# 02 Load and adjust real economic measures ------------------------------------

# Define IDs for GDP growth, two unemployment measures, and labor participation.
my.id <- c('gdp' = 4380, 'unemp_desat' = 1620, 'unemp_pnad' = 24369, "lbr_part" = 28544)
# Fetch and process real measures data similar to inflation data.
df <- gbcbd_get_series(my.id, cache.path = tempdir(),
                       first.date = Sys.Date() - 30 * 365,
                       last.date = Sys.Date())
df_realmeasures_brazil <- df %>%
  select(!id.num) %>%
  pivot_wider(names_from = series.name, values_from = value) %>%
  rename(FirstDate = ref.date)

# Load the dataset, skipping the first row and excluding the second column
desemprego_pme_descontinuado <- read_delim("brazil_real_measures_data/desemprego_pme_descontinuado.csv", 
                                           delim = ";", 
                                           escape_double = FALSE, 
                                           col_names = FALSE, 
                                           trim_ws = TRUE, 
                                           skip = 1) %>%
  select(-X2) # Exclude the second column right after loading

desemprego_pme_descontinuado <-as.data.frame(t(as.matrix(desemprego_pme_descontinuado)), header = TRUE) %>%
  select(V1, V4)

desemprego_pme_descontinuado <- desemprego_pme_descontinuado %>%
  slice(-1) %>% # Remove the first row which is likely a header or unwanted data
  mutate(V4 = as.numeric(V4), # Convert V4 to numeric
         FirstDate = dmy(paste("01", V1)), # Create a FirstDate column assuming the first day of the month
         quarter = as.yearqtr(FirstDate), # Create a quarter column from FirstDate
         group = year(FirstDate)) %>% # Create a group column based on the year of FirstDate
  select(FirstDate, unrate = V4, quarter, group) # Select and rename columns

df_realmeasures_brazil <- df_realmeasures_brazil%>%
  left_join(desemprego_pme_descontinuado) %>%
  mutate(unrate = coalesce(unemp_desat, unemp_pnad, unrate)) %>%
  select(FirstDate, gdp, unrate, lbr_part) %>%
  mutate(quarter = as.yearqtr(FirstDate),
         group = year(FirstDate)) %>%
  mutate(gdp_lag = lag(gdp, n=1), 
         gdpg = log(gdp / gdp_lag)) %>%
  mutate(gap1 = log(lag(gdp, n =1))^2) %>%
  mutate(lshr = lbr_part/gdp)

library(hpfilter) # implements the modified filter for gap2

gap2 <-  df_realmeasures_brazil%>%
  select(gdp)

filter <- hp1(gap2, lambda = 1600)

gap2 <- df_realmeasures_brazil %>%
  select(FirstDate, gdp)

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

df_realmeasures_brazil <- df_realmeasures_brazil %>%
  left_join(df_gap2_complete) 

df_realmeasures_brazil <- df_realmeasures_brazil %>%
  arrange(FirstDate) %>%
  select(FirstDate, gdpg, gap1, gap2, unrate, lshr, quarter, group) %>%
  group_by(quarter) %>%
  filter(row_number() == 1) %>%
  ungroup()

# 03 Load and adjust FOCUS surveys ---------------------------------------------

# Define the economic indicators of interest.
indic <- c("IPCA", "IPCA-15", "IPCA Alimentação no domicílio")
# Fetch survey data for the defined indicators.
df_surveys_brazil <- get_annual_market_expectations(indic) %>%
  select(indic, FirstDate = date, reference_date, median) %>%
  pivot_wider(names_from = indic, values_from = median, names_prefix = "indic_", values_fn = mean) %>%
  filter(reference_date == year(FirstDate) + 1) %>%
  filter(month(FirstDate) %in% c(1, 4, 7, 10)) %>%
  mutate(quarter = as.yearqtr(FirstDate)) %>%
  group_by(quarter) %>%
  filter(row_number() == 1) %>%
  ungroup()

# 04 save the results as csv files on the appropriate folder --------------------

write.csv(df_inflation_brazil, file = "brazil_data/df_inflation_brazil.csv")
write.csv(df_realmeasures_brazil, file = "brazil_real_measures_data/df_realmeasures_brazil.csv")
write.csv(df_surveys_brazil, file = "brazil_survey_data/df_surveys_brazil.csv")
