
library(survival)
library(ehdservices)
library(lubridate)
library(dbplyr)
library(tidyverse)


conn <- db2connect::db2_connect(db_name = "POPUM")



icd10h <- tbl(conn, in_schema("KOD", "KODICD10H")) %>% collect()
icd10h <- icd10h %>% 
  mutate(
    icd10h_c = str_extract(ICD10H, "[A-Z]\\d+"),
    icd10_c  = str_extract(ICD10, "[A-Z]\\d+")
  )

icd10h %>% filter(INFANTCAT %in% c("air-borne", "water-food borne")) %>% count(INFANTCAT, HISTCAT, icd10h_c) 

icd10hd <- tbl(conn, in_schema("KOD", "KODICD10HDESCRIPTIONS")) %>% collect()

icd <- left_join(icd10h, icd10hd)

icd %>% filter(INFANTCAT %in% c("air-borne", "water-food borne")) %>% filter(!is.na(DESCRIPTION)) %>% 
  filter(!str_detect(DESCRIPTION,"unspecified")) %>% 
  select(INFANTCAT, icd10h_c, DESCRIPTION) %>% 
  mutate(
    chap = str_extract(icd10h_c, "\\w"),
    numb = str_extract(icd10h_c, "\\d+") %>% as.integer()
  ) %>% 
  group_by(INFANTCAT, chap) %>% 
  summarise(
    desc = paste0(unique(DESCRIPTION), collapse = ", "),
    ICD_c = paste0(unique(numb), collapse = ", ")
  ) %>% 
  flextable::flextable()


icd %>% filter(!INFANTCAT %in% c("air-borne", "water-food borne")) %>% 
  filter(!str_detect(INFANTCAT, "other|stated|blank|unknown")) %>% 
  mutate(
    chap = str_extract(icd10h_c, "\\w"),
    numb = str_extract(icd10h_c, "\\d+") %>% as.integer()
  ) %>% 
  summarise(chaps = paste0(unique(chap), collapse = ", "))
