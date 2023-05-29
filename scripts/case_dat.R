library(zoo)
library(lubridate)
library(tidyverse)


load("data/indiv_data.rda")

#' Day death data
#' Iterate over day, sum by  age

# -----------------


indiv_data %>% 
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

p_d <- indiv_data %>% 
  mutate(
    urban = ifelse(fodhemfrsnmn == "SUNDSVALL", "Urban", "Rural"),
    unknown = ((is.na(infantcat) | infantcat %in% c("stated to be 'unknown'", "no cause given/blank")) & event == 1),
    other = event - water - air - unknown
  ) %>% 
  filter(water | air | unknown | event == 1 | other == 1) %>% select(water, air, unknown, durr_d, event, other, urban) %>% 
  pivot_longer(cols = c(water, air, unknown, event, other)) %>% filter(value == 1) %>% 
  filter(name != "event") %>% 
  mutate(
    name = factor(name, levels = c("water", "air", "other", "unknown"), labels = c("WFID", "AID", "Other", "Unknown"))
  ) %>% 
  group_by(urban, name) %>% 
  summarise(durrs = sum(durr_d), n = n()) %>% 
  mutate(p = n /sum(n)) #%>% 

ggplot(p_d, aes(urban, n, group = name, fill = name)) + geom_col(position = position_stack()) +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_classic() + 
  labs(x = NULL, y = "Deaths", fill = NULL)

ggsave("figures/death_counts.png", height = 4, width = 8)
# -------------

indxx <- tibble(
  datee = seq(ymd("18600101"), ymd("18930101"), "day"),
  y = year(datee),
  dy = yday(datee)
)

c_2 <- indiv_data %>% 
  mutate(
    unknown = ((is.na(infantcat) | infantcat == "stated to be 'unknown'")& event == 1),
    edate = bdate + days(durr_d)
  )

diff_days <- function (first, last) 
{
  lubridate::as.duration(lubridate::interval(first, last))/lubridate::ddays(1)
}

number_of_cores <- parallel::detectCores() - 1 

library(parallel)

res <- parallel::mclapply(indxx$datee, mc.cores = number_of_cores, function(x, dd = c_2){
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


indxx <- left_join(indxx, res2)

# indxx

res3 <- indiv_data %>% 
  filter(event == 1) %>%
  mutate(
    age = diff_days(bdate, ddate),
    age_g = cut(age, c(0, 14,31,367), include.lowest = T)
  ) %>%  
  group_by(ddate, age_g) %>% 
  summarise(dead = n())



res3a <- indiv_data %>% 
  filter(air == 1) %>% 
  mutate(
    age = diff_days(bdate, ddate),
    age_g = cut(age, c(0, 14,31,367), include.lowest = T)
  ) %>% 
  group_by(ddate, age_g) %>% 
  summarise(air = n())

res3w <- indiv_data %>% 
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

case_data <- res5 %>% select(datee, y, dy, month, wday, age_g, n, dead, air, water, unknown)

save(case_data, file = "data/case_data.rda", compress = "xz")
