# read the data
library(tidyverse)
library(readr)
library(absmapsdata)
library(ggplot2)
library(sf)
library(lubridate)
library(RColorBrewer)
library(ggsn)
library(plotly)
library(gganimate)

x <- read_csv("data/H08_20200101_0000_1MWLFbet_FLDK.06001_06001.csv")

states <- absmapsdata::state2016 %>% 
  filter(state_code_2016 %in% c("1", "2"))

ggplot(states) + 
  geom_sf() +
  geom_point(aes(x = lon, y = lat), data = x)

jan18 <- x %>% 
  # aus points
  filter(lon > -112, lon < 155, lat > -44, lat <-10) %>% 
  mutate(date = ymd_hms(`#obstime`)) %>%
  filter(date > ymd("2020-01-18")) %>%
  filter(date < ymd("2020-01-19")) %>% 
  filter(lat < -32)

fi <- jan18 %>% filter(lat < -38) %>% 
  mutate(step = rep(c(1,2),2))
# Map of VIC and NSW


# French Island Fire Jan 18 2020 
fi_map <- absmapsdata::sa22016 %>% 
  filter(sa2_name_2016 == "French Island")


ggplot(fi_map) + 
  geom_sf() +
  geom_point(aes(x = lon, y = lat), 
    data = fi, colour = "red", alpha = 0.7) +
  geom_path(aes(x = lon, y = lat, group = date), 
    data = fi)




vic_fires <- jan18 %>% 
  filter(lat < -37, lat > -37.4, 
    lon < 142.5) %>% 
    mutate(time = substr(dettime, 14, 20),
          size = (firepower/10000))

g1 <- ggplot(data = vic_fires) + 
  geom_sf(aes(size = size, colour = size), 
    alpha = 0.7) + 
  scale_color_distiller(type = "seq", palette = "YlOrRd", 
    direction = 1) +
  north(vic_fires) +
  scalebar(vic_fires, dist = 4, dist_unit = "km",
    transform = TRUE, model = "WGS84", st.size = 3, 
    location = "bottomleft") + 
  geom_sf_text(aes(label = time), nudge_x = -0.01, nudge_y = 0.007) +
  guides(size = FALSE)



anim <- ggplot(data = vic_fires) + 
  geom_point(aes(x = lon, y = lat, size = size, fill = size), colour = "grey", shape = 21) + 
  scale_fill_distiller(type = "seq", palette = "YlOrRd", 
    direction = 1) + 
  labs(title = "Time: {frame_time}") +
  # geom_text(aes(x = lon, y = lat, label = time), nudge_x = -0.01, nudge_y = 0.007) +
  guides(size = FALSE) + 
  transition_time(date) + 
  #shadow_trail(distance = 1, max_frames = 10) + 
  # shadow_wake(wake_length = 0.3) +
  shadow_mark(alpha = alpha/2, size = size/2) +
  exit_fade()

animate(anim, nframe = 50, duration = 10)

# Interaction

plotly::ggplotly(g1)

## Buffer
vic_points <- jan18 %>%
  filter(lat < -37, lat > -37.4,
    lon < 142.5) %>%
  mutate(time = substr(dettime, 14, 20),
    size = (firepower)/10000) %>%
  mutate(geometry = map2(lon, lat, 
    ~st_point(x = c(.x, .y)))) %>%
  st_as_sf()

buffer <- st_buffer(vic_points$geometry, vic_points$size)

vic_buffer <- vic_points %>% 
  mutate(geometry = buffer)

ggplot() +
  geom_sf(data = vic_buffer, aes(fill = size), alpha = 0.7) + 
  geom_sf(data = vic_points) + 
  scale_fill_distiller(type = "seq", palette = "YlOrRd", 
    direction = 1) 
