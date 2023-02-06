library(zoo)
library(lubridate)
library(tidyverse)

# Prepare temperature data --------------


harno <- read_delim("data-raw/smhi-opendata_1_127380_20210823_173023.csv.gz", delim = ";", skip = 9) %>% 
  set_names(c("date", "time", "temperature", "quality", "n1", "n2")) %>% 
  select(date, time, temperature, quality)


h1 <- harno %>% 
  mutate(
    time = as.character(time),
    time_ind = case_when( # temperature are measured in the morning, noon and evening. The exact hour changes over time
      str_detect(time, "06") ~ "morning",
      str_detect(time, "07") ~ "morning",
      str_detect(time, "12") ~ "noon",
      str_detect(time, "13") ~ "noon",
      str_detect(time, "18") ~ "evening",
      str_detect(time, "20") ~ "evening"
    ),
    dt = paste(date, time, sep = " "),
    dt2 = ymd_hms(dt),
    idx = as.numeric(dt2)
  ) %>% 
  filter(date < "1893-01-01") %>%
  mutate(
    idx = as.integer(date)
  ) %>% 
  # Create a full sequence of date and time observations between the first observed date and last.
  complete(idx = min(idx):max(idx), time_ind, fill = list(temperature = NA)) 


# interpolate missing --------
# ?zoo::na.spline

f <- function(d){
  x <- zoo(d$temperature, d$idx)
  na.spline(x, na.rm = F)
}


h0 <- h1 %>% 
  group_by(time_ind) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(
    d2 = map(data, f),
    app = map(d2, as.vector)
  ) %>% 
  unnest(cols = c(data, app)) %>% 
  arrange(date, time) %>% 
  mutate(date= as.Date(idx)) 

# Calculate lag temperature exposures and rolling averages ---------

rol_avg <- function(x, k, a = "right") zoo::rollmean(x, k = k, fill = NA, align = a)

temp_harno <- h0 %>% 
  group_by(date) %>% 
  summarise(
    temp = mean(app, na.rm = T)
    
  ) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(
    y = year(date),
    month = month(date),
    d1 = lubridate::decimal_date(date),
    yday = yday(date),
    temp1 = lag(temp, 1),
    temp2 = lag(temp, 2),
    temp3 = lag(temp, 3),
    temp4 = lag(temp, 4),
    temp5 = lag(temp, 5),
    temp6 = lag(temp, 6),
    temp7 = lag(temp, 7),
    temp8 = lag(temp, 8),
    temp9 = lag(temp, 9),
    temp10 = lag(temp, 10),
    temp11 = lag(temp, 11),
    temp12 = lag(temp, 12),
    temp13 = lag(temp, 13),
    temp14 = lag(temp, 14),
    temp15 = lag(temp, 15),
    temp16 = lag(temp, 16),
    temp17 = lag(temp, 17),
    temp18 = lag(temp, 18),
    temp19 = lag(temp, 19),
    temp20 = lag(temp, 20),
    lag_temp = rol_avg(temp1, 7)
  ) 



save(temp_harno, file="data/temp_harno.rda", compress = "xz")

# Combine death case data with  ------------------

load("data/case_data.rda")

temp_case_data <- left_join(
  case_data, 
  temp_harno %>% select(date, temp, temp1:temp14, lag_temp), 
  by = c("datee" = "date")
) %>% 
  filter(!is.na(temp)) %>% 
  rename(pop = n)

save(temp_case_data, file = "data/temp_case_data.rda", compress = "xz")
