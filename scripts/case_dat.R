library(zoo)
library(lubridate)
library(tidyverse)

#' Day death data
#' Iterate over day, sum by  age

harno <- read_delim("data-raw/smhi-opendata_1_127380_20210823_173023.csv", delim = ";", skip = 9) %>% 
  set_names(c("date", "time", "temperature", "quality", "n1", "n2")) %>% 
  select(date, time, temperature, quality)



# interpolate missing
# ?zoo::na.approx  or na.spline

f <- function(d){
  x <- zoo(d$temperature, d$idx)
  na.spline(x, na.rm = F)
}


h0 <- harno %>% 
  filter(date < "1893-01-01") %>% 
  # filter(date < "1869-11-01", date > "1869-10-15") %>% 
  # slice(1:100) %>% 
  mutate(
    idx = as.integer(date)
  ) %>% 
  complete( time, idx, fill = list(temperature = NA)) %>% 
  group_by(time) %>% 
  nest() %>% 
  mutate(
    d2 = map(data, f),
    app = map(d2, as.vector)
  ) %>% 
  unnest(cols = c(data, app)) %>% 
  arrange(date, time) %>% 
  mutate(date= as.Date(idx)) 

rol_avg <- function(x, k, a = "right") zoo::rollmean(x, k = k, fill = NA, align = a)

h1 <- h0 %>% 
  group_by(date) %>% 
  summarise(
    temp = mean(app, na.rm = T)
    
  ) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(
    y = year(date),
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
    lag_temp = rol_avg(temp1, 7)
  ) 


temp_harno <- h1 %>% ungroup() 

save(temp_harno, file="data/temp_harno.rda")



load("data/case_dat.rda")

# -----------------


case_dat %>% 
  mutate(
    unknown = ((is.na(infantcat) | infantcat == "stated to be 'unknown'")& event == 1),
    other = event - water - air - unknown
  ) %>% 
  filter(water | air | unknown | event == 1 | other == 1) %>% select(water, air, unknown, durr_d, event, other) %>% 
  pivot_longer(cols = c(water, air, unknown, event, other)) %>% filter(value == 1) %>% 
  mutate(name = factor(name, levels = c("event","air", "water", "other","unknown"), labels = c("All-cause","AIR", "WFID", "Other",  "Unknown")))  %>%
  ggplot(aes(durr_d)) +
  geom_histogram(fill = "gray", color = "black", breaks = seq(0,370,7)) +#, boundary = -1) + 
  facet_grid(name~., scales = "free_y") + 
  ggthemes::theme_tufte() +
  theme(
    text = element_text(family  = "sans")
  ) + 
  labs(x = "Day of death", y  = "Frequency")

ggsave("figures/cause-hist.png", height = 8, width = 8)

# -------------------


indxx <- tibble(
  datee = seq(ymd("18600101"), ymd("18930101"), "day"),
  y = year(datee),
  dy = yday(datee)
)

c_2 <- case_dat %>% 
  mutate(
    unknown = ((is.na(infantcat) | infantcat == "stated to be 'unknown'")& event == 1),
    edate = bdate + days(durr_d)
  )

diff_days <- function (first, last) 
{
  lubridate::as.duration(lubridate::interval(first, last))/lubridate::ddays(1)
}

library(parallel)

res <- parallel::mclapply(indxx$datee, mc.cores = 11, function(x, dd = c_2){
  dd %>% filter(bdate <= x, edate >= x) %>% 
    mutate(
      age = diff_days(bdate, x)
    ) %>% 
    # filter(age >= 14) %>% 
    mutate(age_g = cut(age, c(0, 14,31,367), include.lowest = T)) %>% 
    count(age_g) %>% 
    mutate(datee = x)
  
  # nrow()
})

res2 <- map_df(res, ~.)

# indxx$n = res
indxx <- left_join(indxx, res2)

# indxx

res3 <- case_dat %>% 
  filter(event == 1) %>%
  mutate(
    age = diff_days(bdate, ddate),
    age_g = cut(age, c(0, 14,31,367), include.lowest = T)
  ) %>%  
  group_by(ddate, age_g) %>% 
  summarise(dead = n())



res3a <- case_dat %>% 
  filter(air == 1) %>% 
  mutate(
    age = diff_days(bdate, ddate),
    age_g = cut(age, c(0, 14,31,367), include.lowest = T)
  ) %>% 
  group_by(ddate, age_g) %>% 
  summarise(air = n())

res3w <- case_dat %>% 
  filter(water == 1) %>% 
  mutate(
    age = diff_days(bdate, ddate),
    age_g = cut(age, c(0, 14,31,367), include.lowest = T)
  ) %>% 
  group_by(ddate, age_g) %>% 
  summarise(water = n())

res3u <- c_2 %>% 
  filter(unknown) %>% 
  mutate(
    age = diff_days(bdate, ddate),
    age_g = cut(age, c(0, 14,31,367), include.lowest = T)
  ) %>% 
  group_by(ddate, age_g) %>% 
  summarise(unknown = n(), .groups = "drop")



res5 <- indxx %>% 
  left_join(res3, by = c("datee"= "ddate", "age_g")) %>% 
  left_join(res3a, by = c("datee"= "ddate", "age_g")) %>% 
  left_join(res3w, by = c("datee"= "ddate", "age_g")) %>% 
  left_join(res3u, by = c("datee"= "ddate", "age_g")) %>% 
  replace_na(list(dead = 0, air = 0, water = 0, unknown = 0)) %>% 
  mutate(
    month = month(datee),
    wday  = wday(datee),
    st    = paste(y, month, sep = ".")
  )

d3 <- left_join(res5, h1 %>% select(date, temp, temp1:temp14, lag_temp), by = c("datee" = "date")) %>% filter(!is.na(temp)) %>% 
  rename(pop = n)


temp_case_data <- d3
save(temp_case_data, file = "data/temp_case_data.rda")

