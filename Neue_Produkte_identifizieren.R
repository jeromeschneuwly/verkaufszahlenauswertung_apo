library(tidyverse)
library(xlsx)
library(openxlsx)
library(stringr)
library(gdata)
library(zoo)
library(janitor)
dat_path <- "C:/Analyse_Apotheke/Zentrum_Auswertung/"
datum_umstellung <- as.Date("01.07.2023", "%d.%m.%Y")


data <- read.delim(paste0(dat_path, "Monatsauswertung_2023.csv"),
                   sep = ';', fileEncoding = 'latin1')

data_date <- data %>% 
  mutate(predate = paste(Jahr, Monat, sep = "-"),
         Date = as.Date(as.yearmon(predate))) %>% 
  filter(Relevant == 'ja')

alle_namen <- data_date %>% 
  distinct(Artikelbezeichnung, Pharmacode) %>% 
  group_by(Pharmacode) %>% 
  slice(1) %>% 
  ungroup()

vorher <- data_date %>% 
  filter(Date < datum_umstellung) %>% 
  distinct(Artikelbezeichnung, Pharmacode, EAN.Code, Kategorie_1, Kategorie_2, 
           Kategorie_3, Kategorie_4, Marke) %>% 
  mutate(Analyse_alt = "bisherig")

nachher <- data_date %>% 
  filter(Date >= datum_umstellung) %>% 
  distinct(Artikelbezeichnung, Pharmacode, EAN.Code, Kategorie_1, Kategorie_2, 
           Kategorie_3, Kategorie_4, Marke) %>% 
  mutate(Analyse_neu = "Neu")

neue <- nachher %>% 
  full_join(vorher, by = c("Pharmacode", "EAN.Code",
                           "Kategorie_1", "Kategorie_2", 
                           "Kategorie_3", "Kategorie_4", "Marke")) %>% 
  filter(is.na(Analyse_alt)) %>% 
  left_join(alle_namen) %>% 
  select(Artikelbezeichnung, Pharmacode, EAN.Code, Marke, Kategorie_1, 
         Kategorie_2, Kategorie_3, Kategorie_4)
  
write.table(neue, file = paste0("C:/Analyse_Apotheke/", "Neue_Artikel_Zentrum_07_08.csv"), 
            sep = ';', fileEncoding = 'latin1', row.names = FALSE)

