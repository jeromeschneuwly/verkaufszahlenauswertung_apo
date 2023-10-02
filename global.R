library(shiny)
library(tidyverse)
library(xlsx)
library(openxlsx)
library(shinyWidgets)
library(plotly)
library(shinycssloaders)
library(ggplot2)
library(janitor)
library(RColorBrewer)
library(scales)
library(shinymanager)
source("credentials.R")
options(scipen = 999)
mapping_kat5 <- read.xlsx("./Kategorie_5.xlsx", colNames = T, rowNames = F,
                          na.strings = c('', ' '), detectDates = T)

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

# Function to format labels with apostrophe as thousand separator
format_with_apostrophe <- function(x) {
  formatted <- format(x, big.mark = "'", scientific = FALSE)
  return(formatted)
}