
# missing packages

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
kable(panel_a, "html", booktabs = TRUE, align = 'c', col.names = c("", "PUNEW", "PUXHS", "PUXX", "PCE")) %>%
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
kable(panel_b, "html", booktabs = TRUE, align = 'c', col.names = c("", "PUNEW", "PUXHS", "PUXX", "PCE")) %>%
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
kable(panel_c, "html", booktabs = TRUE, align = 'c', col.names = c("", "PUNEW", "PUXHS", "PUXX", "PCE")) %>%
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
kable(panel_a, "html", booktabs = TRUE, align = 'c', col.names = c("", "PUNEW", "PUXHS", "PUXX", "PCE")) %>%
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
kable(panel_b, "html", booktabs = TRUE, align = 'c', col.names = c("", "PUNEW", "PUXHS", "PUXX", "PCE")) %>%
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
kable(panel_c, "html", booktabs = TRUE, align = 'c', col.names = c("", "PUNEW", "PUXHS", "PUXX", "PCE")) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" " = 1, "Panel C: 1996:Q1– 2023:Q4" = 4)) %>%
  pack_rows("Mean", 1, 1) %>%
  pack_rows("Standard deviation", 2, 2) %>%
  pack_rows("Autocorrelation", 3, 3) %>%
  pack_rows("Correlations", 4, 6)


# Clear environment
rm(panel_c, sds, means, autocorrelation_quaterly, corr_table, data_variables)
rm(df_inflation_complete_B, df_inflation_complete_C, df_inflation_complete_yearly_B, df_inflation_complete_yearly_C, statistics_labels)

## 04.1 Recreate figure 1.A -----------------------------

# Multiply the inflation columns by 100

data_plot1A <- df_inflation_authors_yearly %>%
  mutate(quarter = as.yearqtr(FirstDate)) %>%
  mutate(across(ends_with("year"), ~ .x * 100)) %>%
  full_join(., df_livingston_complete)  %>%
  filter(group < 2003 & group >1951)


# Pivot the data to a long format for plotting with ggplot2
data_plot1A_long <- data_plot1A %>%
  pivot_longer(cols = ends_with("year"), names_to = "inflation_type", values_to = "inflation_value") %>%
  select(FirstDate, inflation_type, inflation_value) %>%
  arrange(FirstDate)

# Define linetypes and shapes based on the provided plot image
line_types <- c("solid", "longdash", "dotted", "dotdash", NA)
shapes <- c(NA, NA, NA, NA, 3) # Only the 'Livingston' series uses a shape, represented by pluses

# Create a named vector to map the inflation types to line1 types
names(line_types) <- unique(data_plot1A_long$inflation_type)
names(shapes) <- unique(data_plot1A_long$inflation_type)

# Plot the data
plot_1a <- ggplot(data_plot1A_long, aes(x = FirstDate, y = inflation_value, color = inflation_type, linetype = inflation_type, shape = inflation_type)) +
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

plot_1a

rm(plot_1a, data_plot1A, data_plot1A_long, line_types, shapes)

## 04.2 Recreate figure 1.B -----------------------------

data_plot1B <- df_inflation_authors_yearly %>%
  mutate(across(ends_with("punew_year"), ~ .x * 100)) %>%
  mutate(quarter = as.yearqtr(FirstDate)) %>%
  filter(group >= 1978 & group <= 2002) %>%
  left_join(df_surveys_complete) %>%
  select(FirstDate, quarter, group, punew_year, liv_year, mich_year, spf_year)

# Pivot the data to a long format for plotting with ggplot2
data_plot1B_long <- data_plot1B %>%
  pivot_longer(cols = ends_with("year"), names_to = "inflation_type", values_to = "inflation_value") %>%
  select(FirstDate, inflation_type, inflation_value)

# Define linetypes and shapes based on the provided plot image
line_types <- c("solid", "dotted", "dotdash", "longdash")
shapes <- c(NA, NA, NA, NA) # Only the 'Livingston' series uses a shape, represented by pluses

# Create a named vector to map the inflation types to linetypes
names(line_types) <- unique(data_plot1B_long$inflation_type)
names(shapes) <- unique(data_plot1B_long$inflation_type)

# Plot the data
plot_1B <- ggplot(data_plot1B_long, aes(x = FirstDate, y = inflation_value, color = inflation_type, linetype = inflation_type, shape = inflation_type)) +
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

plot_1B

rm(plot_1B, data_plot1B, data_plot1B_long, line_types, shapes)

## 04.3 Recreate figure 1.A (extended) -----------------------------

# Multiply the inflation columns by 100

data_plot1A <- df_inflation_authors_yearly %>%
  mutate(quarter = as.yearqtr(FirstDate)) %>%
  mutate(across(ends_with("year"), ~ .x * 100)) %>%
  full_join(., df_livingston_complete)

# Pivot the data to a long format for plotting with ggplot2
data_plot1A_long <- data_plot1A %>%
  pivot_longer(cols = ends_with("year"), names_to = "inflation_type", values_to = "inflation_value") %>%
  select(FirstDate, inflation_type, inflation_value) %>%
  arrange(FirstDate)

# Define linetypes and shapes based on the provided plot image
line_types <- c("solid", "longdash", "dotted", "dotdash", NA)
shapes <- c(NA, NA, NA, NA, 3) # Only the 'Livingston' series uses a shape, represented by pluses

# Create a named vector to map the inflation types to line1 types
names(line_types) <- unique(data_plot1A_long$inflation_type)
names(shapes) <- unique(data_plot1A_long$inflation_type)

# Plot the data
plot_1a <- ggplot(data_plot1A_long, aes(x = FirstDate, y = inflation_value, color = inflation_type, linetype = inflation_type, shape = inflation_type)) +
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

plot_1a

rm(plot_1a, data_plot1A, data_plot1A_long, line_types, shapes)


## 04.2 Recreate figure 1.B (extended)-----------------------------

data_plot1B <- df_inflation_authors_yearly %>%
  mutate(across(ends_with("punew_year"), ~ .x * 100)) %>%
  mutate(quarter = as.yearqtr(FirstDate)) %>%
  left_join(df_surveys_complete) %>%
  select(FirstDate, quarter, group, punew_year, liv_year, mich_year, spf_year)

# Pivot the data to a long format for plotting with ggplot2
data_plot1B_long <- data_plot1B %>%
  pivot_longer(cols = ends_with("year"), names_to = "inflation_type", values_to = "inflation_value") %>%
  select(FirstDate, inflation_type, inflation_value)

# Define linetypes and shapes based on the provided plot image
line_types <- c("solid", "dotted", "dotdash", "longdash")
shapes <- c(NA, NA, NA, NA) # Only the 'Livingston' series uses a shape, represented by pluses

# Create a named vector to map the inflation types to linetypes
names(line_types) <- unique(data_plot1B_long$inflation_type)
names(shapes) <- unique(data_plot1B_long$inflation_type)

# Plot the data
plot_1B <- ggplot(data_plot1B_long, aes(x = FirstDate, y = inflation_value, color = inflation_type, linetype = inflation_type, shape = inflation_type)) +
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

plot_1B

rm(plot_1B, data_plot1B, data_plot1B_long, line_types, shapes)
