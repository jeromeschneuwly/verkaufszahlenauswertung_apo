library(shiny)
library(tidyverse)
library(xlsx)
library(openxlsx)
library(shinyWidgets)
library(plotly)
library(shinycssloaders)
library(ggplot2)
library(janitor)
options(scipen = 999)
zentrum_path <- "C:/Analyse_Apotheke/Zentrum_Auswertung/"
glatt_path <- "C:/Analyse_Apotheke/Glatt_Auswertung/"

mapping_kat5 <- read.xlsx("C:/Analyse_Apotheke/verkaufszahlenauswertung_apo/Kategorie_5.xlsx", colNames = T, rowNames = F,
                          na.strings = c('', ' '), detectDates = T)

mapping_kat5_clean <- mapping_kat5 %>% 
  distinct(Pharmacode, Kategorie_5)

kat_changed <- mapping_kat5_clean %>% get_dupes(Pharmacode)

mapping_kat5_clean_no_change <- mapping_kat5_clean %>% 
  filter(!Pharmacode %in% kat_changed$Pharmacode) %>% 
  rename(Kategorie_5_stable = Kategorie_5)

mapping_kat5_changed <- mapping_kat5 %>% 
  filter(Pharmacode %in% kat_changed$Pharmacode) %>% 
  distinct(Jahr, Pharmacode, Kategorie_5) %>% 
  rename(Kategorie_5_changed = Kategorie_5)
