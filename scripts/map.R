#map.R

# map.R

library(ggspatial)
library(tidyverse)

# load("data/rawdata.rda")


epsg_ref <- 3014 # Sundsvall

par_samp <- tribble(
  ~geom_id,~par_name, ~region,
  #----|--------|----------
  2752,"Hässjö","Rural",
  1503,"Tynderö","Rural",
  3819,"Ljustorp","Rural",
  1413,"Indals","Rural",
  3195,"Sättna","Rural",
  2454,"Selånger","Rural",
  1392,"Tuna","Rural",
  2661,"Attamar","Rural",
  3944,"Njurunda","Industrial",
  3365,"Timrå","Industrial",
  2228,"Sköns","Industrial",
  2539,"Gustav Adolfs","Urban",
  2840,"Skönsmons","Industrial",
  3028,"Alnö","Industrial",
)


library(histmaps)
library(sf)


# samp_par <- read_csv("data/par_info2.csv")


par_d <- histmaps::geom_sp %>% filter(geom_id %in% par_samp$geom_id) %>% st_transform(crs = epsg_ref)   # filter(start <= 1900, end >= 1900)

e1900 <- histmaps::e1900 %>% st_as_sf()

# st_geometry(par_d) %>% plot()


bbox <- st_bbox(par_d %>% 
                  filter(start <= 1880, end >= 1880) %>% st_transform(crs = epsg_ref))


pad <- 1e4

bbox <- bbox + c(-pad*3, -pad,pad+pad,pad)

x <- 1890

# bkg <- geom_ %>% filter(type_id == "county", start <= x, end >= x)  %>% st_transform(crs = epsg_ref)

# bkg <- st_crop(bkg, bbox)

load("../histmaps-v2/data/e1900.rda")
load("../histmaps-v2/data/e1900bounds.rda")

load("../histmaps-v2/data/geom_borders.rda")

e1900 <- st_as_sf(e1900)

bkg <- e1900 %>% filter(COUNTRY != 20) %>%  st_transform(crs = epsg_ref) %>% 
  st_crop(bbox)

swe <- st_as_sf(sweden) %>% st_transform(crs = epsg_ref) %>% 
  st_crop(bbox)

cnt <- geom_borders %>% filter(type_id == "county", start <= x, end >= x)  %>% st_transform(crs = epsg_ref)
cnt <- st_crop(cnt, bbox)

p_d <- par_d %>% 
  left_join(par_samp %>% select(geom_id, par_name, region))%>%
  st_transform(crs = epsg_ref) %>%
  st_crop(bbox) 

all_p <- geom_borders %>% 
  filter(type_id == "parish",start <= x, end >= x)  %>% st_transform(crs = epsg_ref) %>% 
  st_crop(bbox)





par_cents <- st_coordinates(st_centroid(p_d)) %>%
  as_tibble()
par_cents$name <- p_d %>% pluck("par_name") %>%
  str_replace("församling", "") %>%
  str_trim() %>%
  str_replace(" ", "\n")
# 
# lab_data <- p_d %>% #filter(location != "Piteå") %>%
#   select(name = location2, X = lng, Y = lat) %>%
#   mutate(alpha_val = 1) %>%
#   histmaps::st_as_data_frame() %>%
#   bind_rows(par_cents %>% mutate(alpha_val = .999))


p_d <- p_d %>% 
  mutate(name = name %>% str_replace("församling", "") %>%
           str_trim() %>%
           str_replace(" ", "\n"))


eur_poly <- st_as_sf(e1900)
eur_line <- st_as_sf(e1900bounds)

bb1 <- st_bbox(st_transform(p_d, crs=  st_crs(eur_poly)))+ c(-pad*3, -pad,pad,pad)
obb <- st_bbox(eur_poly)


over_map <- ggplot() + 
  geom_sf(data = eur_poly, color = NA) + 
  geom_sf(data = eur_line, color = "gray70", size = .3) + 
  geom_rect(aes(xmin = bb1[1] - 70000, ymin = bb1[2] -70000, xmax = bb1[3] + 50000, ymax = bb1[4] + 50000), fill = NA, color = "black") +
  coord_sf(xlim = c(obb[1], obb[3]- 200e4), ylim = c(obb[2] + 2e5, obb[4] - 140e4))+
  ggthemes::theme_map() +
  theme(
    panel.background = element_rect(fill = "#b6c9d6"),
    panel.grid.major = element_blank(),
    legend.position = c(.02,.02),
    legend.background = element_rect(color = "gray50", size = .1)
  )

over_map

over_g <- ggplotGrob(over_map)

bb <- st_bbox(p_d)

x_min <- bbox[1] - 0
y_min <- bbox[4] - 40000

ndg <- rep(0, 14)

ndg[9] <- ndg[9] + 2000

b_map <- ggplot() +
  geom_sf(data = bkg, color = NA, fill = "#e5f5f9") +
  geom_sf(data = swe, color = NA, fill = "#e5f5f9") + 
  geom_sf(data = all_p, size = .2, color = "gray80") +
  # geom_sf(data = brdr, color = "gray80") +
  geom_sf(data = cnt,  color = "gray60", size = .4) +
  geom_sf(data = p_d, aes(fill = region), size = .3, color = NA) + # fill = "#2ca25f"
  geom_sf(data = geom_borders %>% filter(geom_id %in% p_d$geom_id)  %>% st_transform(crs = epsg_ref), size = .2, color = "gray20") +
  coord_sf(xlim = bbox[c(1,3)], ylim = bbox[c(2,4)], expand = FALSE) +
  # ggrepel::geom_text_repel(data = p_d, 
  #                           size = 2.2,
  #                           aes(label = str_to_upper(par_name), geometry = geometry),  
  #                           stat = "sf_coordinates",
  #                           alpha = .8
  # ) + 
  geom_text(
    data = par_cents, 
    size = 2,
    aes(label = name, x = X, y = Y),
    nudge_x = ndg
  ) +
  ggthemes::scale_fill_tableau() +
  ggthemes::theme_map() +
  theme(
    panel.background = element_rect(fill = "#87ceeb"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.justification = c(0, 1),
    panel.grid.major = element_line(color = alpha("white", .5)),
    legend.position = c(0.01,0.3)#bb[c(1,4)] + c(100000, -100000)
    # legend.position = "none"
  )+
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)



b_map




# library(ggspatial)
b_map +
  annotation_custom(
    grob = over_g, 
    xmin = x_min,
    ymin = y_min,
    xmax = x_min + 35000,
    ymax = y_min + 40000
  )



ggsave("figures/map-samp-mig.png", width = 6.1, heigh= 6, scale = 1.4)


# Adding härnösand -------

harno_d <- tibble(
  place = "Härnösand",
  lbl = "Härnösand\nWeather station",
  lat = 62.6364,
  lon =  17.9214
) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(epsg_ref)


b_map2 <- b_map +
  geom_sf(data = harno_d, size = 4) +
  geom_sf(data = harno_d, size = 3.2, color = "white") +
  geom_sf(data = harno_d, size = 1) +
  geom_sf_label(data = harno_d, aes(label = lbl), nudge_y = 5000, size = 2.5,) +
  coord_sf(xlim = bbox[c(1,3)], ylim = bbox[c(2,4)], expand = FALSE) +
  # ggrepel::geom_text_repel(data = p_d, 
  #                           size = 2.2,
  #                           aes(label = str_to_upper(par_name), geometry = geometry),  
  #                           stat = "sf_coordinates",
  #                           alpha = .8
  # ) +
  ggthemes::theme_map() +
  labs(fill = NULL) +
  theme(
    panel.background = element_rect(fill = "#87ceeb"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.justification = c(0, 1),
    panel.grid.major = element_line(color = alpha("white", .5)),
    legend.position = c(0.01,0.3),#bb[c(1,4)] + c(100000, -100000)
    legend.background = element_blank(),
    # legend.position = "none"
  )+
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)



b_map2




# library(ggspatial)
b_map2 +
  annotation_custom(
    grob = over_g, 
    xmin = x_min,
    ymin = y_min,
    xmax = x_min + 35000,
    ymax = y_min + 40000
  )



ggsave("figures/map-samp-mig2.png", width = 6.6, heigh= 6, scale = 1.4)

b_map2
 
# geom_sf(data = bkg, color = NA, fill = "#e5f5f9") +
#   geom_sf(data = swe, color = NA, fill = "#e5f5f9") + 
#   geom_sf(data = all_p, size = .2, color = "gray80") +


leaf_init(crs = epsg_ref) %>% 
  # leaf_background() %>% 
  # leaf_polygon(bkg) %>% 
  # leaf_polygon(swe) %>% 
  # leaf_polygon(all_p) %>% 
  leaf_polygon(d,  interactive = T, lbl = "name")



data("movement", package = "movement")

m <- as_tibble(movement)


m %>% filter(str_detect(forsnamn, "Sundsv"))


# nms <- c(par_samp$par_name[-12], "Sundsvall", "Indal") %>% str_sub(1,6) %>% paste(collapse = "|")
fcodes <- c(228105, 228108, 228109, 228110,228104,228103,226201,228111,226202,226204,228101)

pop_mov <- m %>% 
  filter(forkod %in% fcodes) %>% 
  filter(orgtypn == "frik", year < 1893) %>% 
  group_by(forsnamn, lon, lat) %>% 
  summarise(ymin = min(year), memb = max(medl, na.rm = T)) %>% 
  ungroup()


library(sp)

sp_p <- st_as_sf(pop_mov, coords = c("lon", "lat"), crs = 2400) %>% 
  st_transform(epsg_ref)


# m %>%filter(lanskod == 22) %>%  filter(str_detect(forsnamn, nms)) %>% distinct(forsnamn, forkod, lanskod)

# m %>%filter(lanskod == 22) %>%   distinct(forsnamn, forkod, lanskod) %>% View()

# m %>% filter(str_detect(forsnamn, "Sundsv"))



ggplot() +
  geom_sf(data = bkg, color = NA, fill = "#e5f5f9") +
  geom_sf(data = swe, color = NA, fill = "#e5f5f9") + 
  geom_sf(data = all_p, size = .2, color = "gray80") +
  # geom_sf(data = brdr, color = "gray80") +
  geom_sf(data = cnt,  color = "gray60", size = .4) +
  geom_sf(data = p_d, fill = "gray90", size = .3, color = NA) + # fill = "#2ca25f"
  geom_sf(data = geom_borders %>% filter(geom_id %in% p_d$geom_id)  %>% st_transform(crs = epsg_ref), size = .2, color = "gray40") +
  geom_sf(data = sp_p, aes(size = memb), alpha = .5, color = "red") +
  coord_sf(xlim = bbox[c(1,3)], ylim = bbox[c(2,4)], expand = FALSE) +
  ggthemes::theme_map() +
  theme(
    panel.background = element_rect(fill = "#87ceeb"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.justification = c(0, 1),
    panel.grid.major = element_line(color = alpha("white", .5)),
    legend.position = c(0.01,0.3)#bb[c(1,4)] + c(100000, -100000)
    # legend.position = "none"
  )+
  annotation_scale(location = "bl") 
  # annotation_north_arrow(location = "bl", which_north = "true",
  #                        pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
  #                        style = north_arrow_fancy_orienteering)

ggsave("figures/map-free-chuch.png", width = 6.1, heigh= 6, scale = 1.4)
