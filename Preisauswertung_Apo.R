library(tidyverse)
library(xlsx)
library(openxlsx)
library(stringr)
library(gdata)
dat_path <- "C:/Analyse_Apotheke/"
list_files <- list.files("C:/Analyse_Apotheke/Verkaeufe/Zentrum/", pattern = ".xlsx")
options(scipen = 999)

artikel_raus <- c('METHADON-PAUSCHALE', 'SARS', 'Masken')

data <- data.frame()
for (i in list_files) {
  data_sem <- read.xlsx(paste0("C:/Analyse_Apotheke/Verkaeufe/Zentrum/", i), colNames = T, rowNames = F,
                    na.strings = c('', ' '), detectDates = T)
  data <- rbind(data, data_sem)
}

# Einen falschen Zahlungscode manuell überschreiben:

data2 <- data %>%
  mutate(Zahlungscode = as.numeric(Zahlungscode),
         Zahlungscode = case_when(Artikelbezeichnung == 'DAFALGAN Tabl 500 mg 16 Stk' & Zahlungscode == 4 ~ 1,
                                  TRUE ~ Zahlungscode))

# Topseller einlesen:

tops <- xlsx::read.xlsx(paste0("C:/Analyse_Apotheke/Topseller.xlsx"),
                         sheetIndex = 1, encoding = 'latin1', header = T)
tops <- tops %>%
  mutate(Topseller = 'ja') %>%
  select(Pharmacode = PH.Code, Topseller)

# Masse einlesen:

masse <- xlsx::read.xlsx(paste0("C:/Users/jerome/Analyse_Apotheke/Produkte_Masse.xlsx"),
                         sheetIndex = 1, encoding = 'latin1', header = T)
masse$Witdh <- as.numeric(masse$Witdh)
masse$Height <- as.numeric(masse$Height)
masse$Depth <- as.numeric(masse$Depth)


masse_cm <- masse %>%
  mutate(Width_cm = Witdh/10,
         Height_cm = Height/10,
         Depth_cm = Depth/10) %>%
  select(Pharmacode, Width_cm, Height_cm, Depth_cm)

# Sortimentscode einlesen:
sortimentscode <- xlsx::read.xlsx(paste0("C:/Users/jerome/Analyse_Apotheke/Sortimentscode.xlsx"),
                             sheetIndex = 1, encoding = 'UTF-8', header = F)

sortimentscode$Code <- substring(sortimentscode$X1, 1,12)
sortimentscode$name <- substring(sortimentscode$X1, 13)
sortimentscode$Code <- as.character(sortimentscode$Code)
sortimentscode$name <- str_replace(sortimentscode$name, '', 'ue')
sortimentscode$Code <- trim(sortimentscode$Code)
sortimentscode <- sortimentscode %>%
  select(-X1) 

# Kategorien definieren
list_kat1 <- sortimentscode %>% filter(str_detect(sortimentscode$Code, '00.00.00')) %>%
  mutate(code_kat1 = substring(Code, 1,2)) %>%
  select(code_kat1, Kategorie_1 = name)

list_kat2 <- sortimentscode %>% filter(str_detect(sortimentscode$Code, '00.00') & 
                                         !str_detect(sortimentscode$Code, '00.00.00')) %>%
  mutate(code_kat2 = substring(Code, 1,5)) %>%
  select(code_kat2, Kategorie_2 = name)

list_kat3 <- sortimentscode %>% filter(str_detect(sortimentscode$Code, '00') & 
                                        !str_detect(sortimentscode$Code, '00.00.00') &
                                         ! str_detect(sortimentscode$Code, '00.00')) %>%
  mutate(code_kat3 = substring(Code, 1,8)) %>%
  select(code_kat3, Kategorie_3 = name)


sortimentscode_full <- sortimentscode %>%
  mutate(code_kat1 = substring(Code, 1,2),
         code_kat2 = substring(Code, 1,5),
         code_kat3 = substring(Code, 1,8)) %>%
  left_join(list_kat1, by = 'code_kat1') %>%
  left_join(list_kat2, by = 'code_kat2') %>%
  left_join(list_kat3, by = 'code_kat3') %>%
  select(Code, Kategorie_1, Kategorie_2, Kategorie_3, Kategorie_4 = name)


data_prep <- data2 %>%
  rename(Code = Sortimentscode, Pharmacode = Pharmacode.des.Artikels) %>%
  mutate(tpd_col = str_sub(Artikelbezeichnung, 1,3)) %>%
  filter(!Swissmedic.Kategorie %in% c('A', 'B') & Anzahl.Packungen > 0 & !tpd_col %in% c('TPD', 'WOC', 'H21') & 
           Verkaufspreis > 0 & !str_detect(Artikelbezeichnung, paste(artikel_raus, collapse = "|"))) %>%
  mutate(Lagerort.des.Artikels = case_when(Lagerort.des.Artikels == 'KELL' ~ 'KEL',
                                           TRUE ~ Lagerort.des.Artikels),
         Lagerort.2.des.Artikels = case_when(Lagerort.2.des.Artikels == 'KELL' ~ 'KEL',
                                           TRUE ~ Lagerort.2.des.Artikels),
         Marge = Verkaufspreis-Lagerpreis,
         Marge_perc = Marge/Verkaufspreis*100,
         Abgabedatum = as.Date(Abgabedatum, "%d-%m-%Y"),
         Jahr = format(Abgabedatum, format = "%Y"),
         Monat = format(Abgabedatum, format = "%m"),
         Umsatz = Anzahl.Packungen * Verkaufspreis,
         Kumulierte_Absolute_Marge = Anzahl.Packungen*Marge,
         Doppelplatzierung = case_when(!is.na(Lagerort.des.Artikels) & !is.na(Lagerort.2.des.Artikels) & 
                                         !Lagerort.2.des.Artikels %in% c('KEL', 'ZL') ~ 'ja',
                                       TRUE ~ 'nein'),
         Selbstwahl = case_when(Swissmedic.Kategorie %in% c('C', 'D') ~ 'nein',
                                TRUE ~ 'ja'),
         Ausstellen = case_when(Zahlungscode == 1 ~ 'nein',
                                TRUE ~ 'ja'),
         Relevant = case_when(Ausstellen == 'ja' & Selbstwahl == 'ja' & Verkaufsart == 'Bar' ~ 'ja',
                              TRUE ~ 'nein')) %>%
  drop_na(Pharmacode) %>%
  filter(Jahr %in% c(2021, 2022))

# Monatsauswertung Verkaufszahlen
data_month_sum <- data_prep %>%
  group_by(Jahr, Monat, Pharmacode, Artikelbezeichnung, Code, Verkaufsart, Relevant) %>%
  summarise_at(vars(Kumulierte_Absolute_Marge, Anzahl.Packungen, Umsatz), sum, na.rm = T) %>%
  rename(Packungen = Anzahl.Packungen)
  
# Jahresausertung Verkaufszahlen
data_year_sum <- data_prep %>%
  group_by(Jahr, Pharmacode, Artikelbezeichnung, Code, Verkaufsart, Relevant) %>%
  summarise_at(vars(Kumulierte_Absolute_Marge, Anzahl.Packungen, Umsatz), sum, na.rm = T) %>%
  rename(Packungen = Anzahl.Packungen)

# Gesamtauswertung Verkaufszahlen
data_tot_sum <- data_prep %>%
  group_by(Pharmacode, Artikelbezeichnung, Code, Verkaufsart, Relevant) %>%
  summarise_at(vars(Kumulierte_Absolute_Marge, Anzahl.Packungen, Umsatz), sum, na.rm = T) %>%
  rename(Packungen = Anzahl.Packungen)

# Angaben pro Produkt einmalig heraussuchen
data_allg_ang <- data_prep %>%
  distinct(Jahr, Artikelbezeichnung, Pharmacode, Relevant, Doppelplatzierung, Selbstwahl, Ausstellen, `EAN-Code`,
           Marge, Verkaufspreis, Lagerpreis, Marge_perc, Lagerort.des.Artikels, Lagerort.2.des.Artikels, Verkaufsart, Relevant) %>%
  rename(Marge_Prozent = Marge_perc, Absolute_Marge = Marge)

# Verkaufszahlen pro Monat inkl. Allg. Angaben
monatsauswertung <- data_allg_ang %>%
  left_join(data_month_sum, by = c('Pharmacode', 'Jahr', 'Artikelbezeichnung', 'Verkaufsart', 'Relevant')) %>%
  select(Jahr, Monat, Pharmacode,`EAN-Code`, Artikelbezeichnung, Relevant, 
         Verkaufspreis, Lagerpreis, Marge_Prozent, Absolute_Marge, Packungen, Umsatz, Kumulierte_Absolute_Marge, 
         Lagerort.des.Artikels, Lagerort.2.des.Artikels, Code, Verkaufsart, Doppelplatzierung, Selbstwahl, Ausstellen) %>%
  left_join(sortimentscode_full, by = 'Code') %>%
  left_join(masse_cm, by = 'Pharmacode') %>%
  left_join(tops, by = 'Pharmacode') %>%
  mutate(Topseller = case_when(is.na(Topseller) ~ 'nein',
                               TRUE ~ Topseller)) %>%
  drop_na(Artikelbezeichnung) %>%
  select(-Code) %>%
  mutate(Marke = word(Artikelbezeichnung,1))

# Verkaufszahlen pro Jahr inkl. Allg. Angaben
jahresauswertung <- data_allg_ang %>%
  left_join(data_year_sum, by = c('Pharmacode', 'Jahr', 'Artikelbezeichnung', 'Relevant', 'Verkaufsart')) %>%
  select(Jahr, Pharmacode, `EAN-Code`, Artikelbezeichnung, Relevant,
         Verkaufspreis, Lagerpreis, Marge_Prozent, Absolute_Marge, Packungen, Umsatz, Kumulierte_Absolute_Marge, 
         Lagerort.des.Artikels, Lagerort.2.des.Artikels, Code, Verkaufsart, Doppelplatzierung, Selbstwahl, Ausstellen) %>%
  left_join(sortimentscode_full, by = 'Code') %>%
  left_join(masse_cm, by = 'Pharmacode') %>%
  left_join(tops, by = 'Pharmacode') %>%
  mutate(Topseller = case_when(is.na(Topseller) ~ 'nein',
                               TRUE ~ Topseller)) %>%
  drop_na(Artikelbezeichnung) %>%
  select(-Code) %>%
  arrange(desc(Kumulierte_Absolute_Marge)) %>%
  mutate(Marke = word(Artikelbezeichnung,1))

# Totale Verkaufszahlen inkl. Allg. Angaben
tot_allg_ang <- data_allg_ang %>%
  select(-Jahr) %>%
  distinct(Artikelbezeichnung, Pharmacode, Verkaufsart, Relevant, .keep_all = T)

gesamtauswertung <- tot_allg_ang %>%
  left_join(data_tot_sum, by = c('Pharmacode', 'Artikelbezeichnung', 'Verkaufsart', 'Relevant')) %>%
  select(Pharmacode, `EAN-Code`, Artikelbezeichnung, Relevant,
         Verkaufspreis, Lagerpreis, Marge_Prozent, Absolute_Marge, Packungen, Umsatz, Kumulierte_Absolute_Marge, 
         Lagerort.des.Artikels, Lagerort.2.des.Artikels, Code, Verkaufsart, Doppelplatzierung, Selbstwahl, Ausstellen) %>%
  left_join(sortimentscode_full, by = 'Code') %>%
  left_join(masse_cm, by = 'Pharmacode') %>%
  left_join(tops, by = 'Pharmacode') %>%
  mutate(Topseller = case_when(is.na(Topseller) ~ 'nein',
                               TRUE ~ Topseller)) %>%
  drop_na(Artikelbezeichnung) %>%
  select(-Code) %>%
  arrange(desc(Kumulierte_Absolute_Marge)) %>%
  mutate(Marke = word(Artikelbezeichnung,1))


write.xlsx(gesamtauswertung, file = paste0(dat_path, 'Gesamtauswertung.xlsx'), sep = ';',
            colNames = T, rowNames = F)
write.xlsx(jahresauswertung, file = paste0(dat_path, 'Jahresauswertung.xlsx'), sep = ';',
            colNames = T, rowNames = F)
write.xlsx(monatsauswertung, file = paste0(dat_path, 'Monatsauswertung.xlsx'), sep = ';',
            colNames = T, rowNames = F)



aendert <- data_prep %>%
  distinct(Artikelbezeichnung, Pharmacode, Zahlungscode) %>%
  count(Artikelbezeichnung, Zahlungscode)
