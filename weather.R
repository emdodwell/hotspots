# Extracting weather data
library(bomrang)

station_id <- sweep_for_stations(latlon = c(-37.8136, 144.9631)) %>%
  filter(state == "VIC")
station_id %>% count(name)

ggplot(station_id, aes(x=lon, y=lat)) + geom_point()

station_id %>% filter(lon==147.6008, lat==-37.1017)
omeo <- station_id %>% filter(name=="OMEO") %>% pull(site)

omeo <- get_historical(stationid = omeo)
