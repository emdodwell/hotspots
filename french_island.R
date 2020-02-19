# read the data
library(tidyverse)
library(readr)
library(absmapsdata)
library(ggplot2)
library(sf)
library(lubridate)

x <- read_csv("data/H08_20200101_0000_1MWLFbet_FLDK.06001_06001.csv")

jan18 <- x %>% 
  mutate(date = ymd_hms(`#obstime`)) %>%
  filter(date > ymd("2020-01-18")) %>%
  filter(date < ymd("2020-01-19")) %>% 
  filter(lat < -32)

fi <- jan18 %>% filter(lat < -38)
# Map of VIC and NSW

states <- absmapsdata::state2016 %>% 
  filter(state_code_2016 %in% c("1", "2"))

ggplot(states) + 
  geom_sf() +
  geom_point(aes(x = lon, y = lat), data = fi)
  

# French Island Fire Jan 18 2020 
fi_map <- absmapsdata::sa22016 %>% 
  filter(sa2_name_2016 == "French Island")


ggplot(fi_map) + 
  geom_sf() +
  geom_point(aes(x = lon, y = lat), 
    data = fi, colour = "red", alpha = 0.7)

