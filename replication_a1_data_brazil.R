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
library(GetBCBData)

# 02 Load and adjust inflation data --------------------------------------------

# to check id numbers, go to https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries
# and then insert code number in "Por Código".


## Extract inflation
my.id <- c('ipca' = 433, 'exfe' = 28751)
df <- gbcbd_get_series(my.id, cache.path = tempdir(),
                       first.date = Sys.Date() - 30 * 365,
                       last.date = Sys.Date())
df_inflation_brazil <- df %>%
  select(!id.num) %>%
  pivot_wider(names_from = series.name,
              values_from = value) 

# Extract real measures
my.id <- c('gdpg' = 4380, 'unemp_desat' = 1620,'unemp_pnad' = 24369, "lbr_part" = 28544)
df <- gbcbd_get_series(my.id, cache.path = tempdir(),
                       first.date = Sys.Date() - 30 * 365,
                       last.date = Sys.Date())

df_realmeasures_brazil <- df %>%
  select(!id.num) %>%
  pivot_wider(names_from = series.name,
              values_from = value) 

# Extract surveys





