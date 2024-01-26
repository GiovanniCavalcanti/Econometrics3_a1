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


# Merge all dataframes on the DATE column
df_inflation_complete <- reduce(list(punew_df, puxhs_df, puxx_df, pce_df), full_join, by = "FirstDate") %>%
  select("FirstDate", contains("inflation")) %>%
  mutate(quarter = paste(year(FirstDate), quarter(FirstDate), sep = "-Q")) %>%
  mutate(group = year(FirstDate))

##___________________________________________________________________##
#   Save the df_inflation_complete as idf_inflation_complete.csv.     #
#   This is the the complete inflation panel!                         #  
##___________________________________________________________________##
# write.csv(df_inflation_complete, file = "df_inflation_complete.csv")

rm(pce_df, punew_df, puxhs_df, puxx_df)

# Now, we agregate inflation by year.

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


# Clear environmnent
rm(panel_c, sds, means, autocorrelation_quaterly, corr_table, data_variables)
rm(df_inflation_authors_B, df_inflation_authors_C, df_inflation_authors_yearly_B, df_inflation_authors_yearly_C, statistics_labels)

## 0.2.2 Recreate figure 1.A -----------------------------

# Multiply the inflation columns by 100
data_plot1A <- df_inflation_authors %>%
  mutate(across(starts_with("inflation"), ~ .x * 100))

# Pivot the data to a long format for plotting with ggplot2
data_plot1A_long <- data_plot1A %>%
  pivot_longer(cols = starts_with("inflation"), names_to = "inflation_type", values_to = "inflation_value")

# Define linetypes and shapes based on the provided plot image
line_types <- c("solid", "longdash", "dotted", "dotdash")
shapes <- c(NA, NA, NA, 3) # Only the 'Livingston' series uses a shape, represented by pluses

# Create a named vector to map the inflation types to linetypes
names(line_types) <- unique(data_plot1A_long$inflation_type)
names(shapes) <- unique(data_plot1A_long$inflation_type)

# Plot the data
plot_1a <- ggplot(data_plot1A_long, aes(x = FirstDate, y = inflation_value, color = inflation_type, linetype = inflation_type, shape = inflation_type)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = c("black", "black", "black", "black")) +
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shapes) +
  labs(x = "Year", y = "Percentage", title = "Inflation Over Time") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") 

ggsave("inflation_time_series.pdf", plot_1a, width = 11, height = 8.5)



