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

# dienstleistungen <- mapping_kat5 %>% filter(Kategorie_5 == 'Dienstleistungen') %>% 
#   distinct(Jahr, Pharmacode) 

mapping_kat5 <- mapping_kat5 %>% 
  distinct()

mapping_expanded <- expand(mapping_kat5, Pharmacode, Jahr)

mapping_new <- mapping_kat5 %>% 
  filter(Jahr != 2021)

mapping_2021 <- mapping_kat5 %>% 
  filter(Jahr == 2021) %>% 
  rename(kat_old = Kategorie_5)

mapping_full <- mapping_expanded %>% 
  left_join(mapping_new) %>% 
  group_by(Pharmacode) %>% 
  fill(Kategorie_5, .direction = "downup") %>% 
  left_join(mapping_2021) %>% 
  mutate(Kategorie_5 = case_when(is.na(Kategorie_5) ~ kat_old,
                                   TRUE ~ Kategorie_5)) %>% 
  fill(Kategorie_5, .direction = "downup") %>% 
  select(-kat_old) %>% 
  ungroup()




# mapping_kat5_clean <- mapping_kat5 %>% 
#   distinct(Pharmacode, Kategorie_5) %>% 
#   anti_join(dienstleistungen, by = "Pharmacode")
# 
# kat_changed <- mapping_kat5_clean %>% 
#   anti_join(dienstleistungen, by = "Pharmacode") %>% 
#   get_dupes(Pharmacode)
# 
# 
# 
# mapping_kat5_clean_no_change <- mapping_kat5_clean %>% 
#   filter(!Pharmacode %in% kat_changed$Pharmacode) %>% 
#   rename(Kategorie_5_stable = Kategorie_5)
# 
# mapping_kat5_changed <- mapping_kat5 %>% 
#   filter(Pharmacode %in% kat_changed$Pharmacode) %>% 
#   distinct(Jahr, Pharmacode, Kategorie_5) %>% 
#   rename(Kategorie_5_changed = Kategorie_5)
# 
# kat_changed_add_23 <- expand(mapping_kat5_changed, Pharmacode, Jahr)
# 
# mapping_kat5_changed_full <- mapping_kat5_changed %>% 
#   full_join(kat_changed_add_23, by = c("Pharmacode", "Jahr")) %>% 
#   arrange(Jahr) %>%
#   group_by(Pharmacode) %>% 
#   fill(Kategorie_5_changed, .direction = "downup")
# 
# mapping_kat5_changed_all <- rbind(mapping_kat5_changed_full, dienstleistungen %>% 
#                                 mutate(Kategorie_5_changed = "Dienstleistungen"))
