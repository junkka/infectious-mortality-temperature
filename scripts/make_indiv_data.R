
library(survival)
library(ehdservices)
library(lubridate)
library(dbplyr)
library(tidyverse)


conn <- db2connect::db2_connect(db_name = "POPUM")



links2 <- tbl(conn, in_schema("U21002", "RELIGION")) %>% collect()



barn_raw <- tbl(conn, in_schema("U21002", "BARN")) %>% collect()



load("data-raw/h_l.rda")

code_hisclass_5 <- function(x){
  x <- as.integer(x)
  case_when(
    x == 1 ~ "Upper",
    x == 2 ~ "Upper",
    x == 3 ~ "Middle",
    x == 4 ~ "Middle",
    x == 5 ~ "Middle",
    x == 6 ~ "Middle",
    x == 7 ~ "Worker",
    x == 8 ~ "Farmer",
    x == 9 ~ "Worker",
    x == 10 ~"Farm-worker",
    x == 11 ~ "Worker",
    x == 12 ~ "Farm-worker",
    x == 13 ~ "Worker"
  )
}


socpo_d <- read_csv("data-raw/socpo_hisco.csv") %>% 
  distinct(HISCOKOD, HISCOSTATUS, socpo_label) %>% 
  group_by(HISCOKOD, HISCOSTATUS) %>% 
  slice(1)

hisc <- h_l %>% replace_na(list(status = -9, relation = -9)) %>% 
  mutate(hisclass_5 = code_hisclass_5(hisclass)) %>% 
  left_join(socpo_d, by = c("hisco" = "HISCOKOD", "status" = "HISCOSTATUS"))






diff_days <- function (first, last) 
{
  lubridate::as.duration(lubridate::interval(first, last))/lubridate::ddays(1)
}


diff_months <- function (first, last) 
{
  lubridate::as.duration(lubridate::interval(first, last))/lubridate::dmonths(1)
}


x0 <- barn_raw %>% 
  set_names(str_to_lower(colnames(.))) %>% 
  replace_na(list(
    # foddat = -1,
    bosdat = -1,
    mfoddat = -1,
    mdoddat = -1,
    ffoddat = -1,
    civil = -1,
    interval = -1,
    paritet = 0,
    syskon = 0
  ))

x3 <- x0 %>% 
  filter(fnamnf != "") %>% 
  set_names(str_to_lower(colnames(.))) %>% 
  left_join(links2 %>% select(id = ID, weight = J))

k_2 <- x3 %>% 
  filter(civil %in% c(2,3)) %>% 
  left_join(hisc, by = c("hiscokod" = "hisco", "hiscostatus" = "status", "hiscorelation" = "relation")) %>% 
  mutate(
    bdate = ehdservices::construct_date(foddat) %>% ymd(),
    doddat = ifelse(foddat == 0, 19990101, doddat),
    ddate = ehdservices::construct_date(doddat) %>% ymd(),
    end_a = ehdservices::diff_years(bdate, ddate),
    end_m = diff_months(bdate, ddate),
    durr_d = diff_days(bdate, ddate)
  ) %>% 
  replace_na(list(end_a = 1, durr_d  = 366)) %>% 
  mutate(
    end_a  = end_a + 1/365,
    event = ifelse(end_a < 1, 1, 0),
    neonat = ifelse(durr_d <= 28, 1, 0),
    durr_d = ifelse(durr_d > 366, 366, durr_d),
    end_a = ifelse(end_a > 1, 1, end_a),
    religion = ifelse(is.na(weight), 0, 1),
    socpo_label =  factor(str_to_title(hisclass_5)) %>% fct_explicit_na() %>% fct_relevel("Upper")
  ) %>% 
  filter(bdate >= "1860-01-01", durr_d >= 0, end_a >= 0)


k_3 <- k_2 %>% 
  mutate(
    mbdate = ehdservices::construct_date(mfoddat) %>% ymd(),
    mage = ehdservices::diff_years(mbdate, bdate),
    y = year(bdate),
    y_c = y - median(y),
    durr_d = durr_d+1
  ) %>% 
  filter(!is.na(mage))


cause_d <- tbl(conn, in_schema("U21002", "U21002_FIL2_DODSORSAK_ID")) %>% collect()

cause_d2 <- cause_d

db2connect::db2_close(conn)

cause_d <- cause_d %>% 
  select(ID, DODORSNR, INFANTCAT, HISTCAT) %>% 
  set_names(str_to_lower(colnames(.)))

rank_table <- read_csv("data-raw/rank_table.csv")
rank_table

cause_d <- cause_d %>% left_join(rank_table %>% select(infantcat, rank))

dubls <- cause_d %>%
  filter(rank == 1) %>% 
  distinct(id, infantcat, rank) %>% 
  group_by(id) %>% 
  filter(n() > 1) %>% 
  ungroup() 

library(xlsx)


manual_causes <- read_csv("data-raw/manual-dodorsak_bearbetad primary cause of death.csv")


cause_d3 <- manual_causes %>% 
  set_names(tolower(colnames(.))) %>% 
  select(id, dodorsnr,newrank = rank) %>% 
  left_join(cause_d, .) %>% 
  mutate(rank = ifelse(!is.na(newrank), newrank, rank)) %>% 
  select(-newrank)

c_1 <- cause_d3 %>% 
  arrange(id, rank, dodorsnr) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup()

k_4 <- left_join(k_3, c_1)


kv_r <- k_4 %>% 
  filter(religion == 1) %>% 
  arrange(bdate) %>% 
  group_by(mid) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(
    mid, m_rel = religion, id
  )


k_5 <- left_join(k_4, kv_r) %>% 
  arrange(bdate) %>% 
  group_by(mid) %>% 
  fill(m_rel) %>% 
  ungroup() %>% 
  replace_na(list(m_rel = 0)) %>% 
  filter(civil %in% c(2,3)) %>% 
  mutate(
    religion = factor(religion, labels = c("State", "Free church")),
    m_rel = factor(m_rel, labels = c("State", "Free church")),
    post_rel = m_rel == "Free church" & religion == "State",
    air = infantcat == "air-borne",
    water = infantcat == "water-food borne"
  )

k_6 <- k_5 %>% 
  mutate(parity2 = 1) %>% 
  arrange(mid, bdate) %>% 
  group_by(mid) %>% 
  mutate(
    parity2 = cumsum(parity2) -1 ,
    interval = diff_years(lag(bdate), bdate)
  ) %>% ungroup()


indiv_data <- k_6 %>% 
  replace_na(list(interval = 0))

save(indiv_data, file = "data/indiv_data.rda")
