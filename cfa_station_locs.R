library(sf)
library(dplyr)
library(ggplot2)
library(data.table)

# Shapefile of VIC topographic features
# Source: https://discover.data.vic.gov.au/dataset/cfa-fire-station-vmfeat-geomark_point
x <- read_sf("data/cfa_stations/geomark_point.shp")
head(x)

# Subset to CFA Fire Stations
stations <- x %>% filter(FEATSUBTYP == "fire station")

station_locs <- stations %>% 
  mutate(lon = st_coordinates(stations)[, 1],
         lat = st_coordinates(stations)[, 2]) %>%
  dplyr::select(name = NAME, lon, lat) %>%
  filter(!is.na(name)) %>% data.table()

saveRDS(station_locs[, .(name, lon, lat)], "data/cfa_station_locations.RDS")

# Shapefile of VIC CFA regions
regions <- read_sf("data/cfa_regions/cfa_region.shp")

test <- filter(regions, CFA_REGION == "South West")

# ggplot(regions) + 
#   geom_sf() + 
#   geom_point(aes(x = lon, y = lat), data = station_locs)
