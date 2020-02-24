library(tidyverse)
library(readr)
library(absmapsdata)
library(ggplot2)
library(sf)
library(lubridate)
library(tsibble)

# Himawari-8 Hotspots
x <- read_csv("data/H08_20200101_0000_1MWLFbet_FLDK.06001_06001.csv")
x <- x %>% mutate(obstime = `#obstime`)

# Polygon for VIC
states <- absmapsdata::state2016 %>% 
  filter(state_code_2016 %in% c("2"))

vic_fires <- x %>% 
  mutate(t = ymd_hms(obstime)) %>%
  filter(t > ymd("2020-01-18")) %>%
  filter(t < ymd("2020-01-21")) %>% 
  filter(lat < -35)

ggplot(states) + 
  geom_sf() +
  geom_point(aes(x = lon, y = lat), data = vic_fires)

################################################################################
## Assign fires to clusters and update at each time interval (currently hour) ##
################################################################################

# Subset of fires for code writing
test <- vic_fires %>% 
  mutate(day = date(t), hour = hour(t)) %>% 
  arrange(hour) %>%
  filter(lon > 144, lat > -36.8, hour > 4) %>%
  select(lon, lat, hour)
  # as_tsibble(index = hour) # eventually

# Progression of fires by hour
ggplot() +
  xlim(146.5, 148.5) + ylim(-36.8, -35.4) +
  geom_point(aes(x = lon, y = lat), data = test[test$hour == 5,], alpha = .3) +
  geom_point(aes(x = lon, y = lat), data = test[test$hour == 6,], color = "green", alpha = .5) +
  geom_point(aes(x = lon, y = lat), data = test[test$hour == 7,], color = "red", alpha = .5) +
  geom_point(aes(x = lon, y = lat), data = test[test$hour == 8,], color = "blue", alpha = .3) +
  geom_point(aes(x = lon, y = lat), data = test[test$hour == 9,], color = "black", alpha = .3)

test <- data.table(test)

# Function that returns:
# 1) Distance matrix 
# 2) Assign cluster membership (based on pairwise connected components; distance - .05)
# 3) Number of clusters created

dist_matrix <- function(dt) {
  
  # calculate distances between each pair of points
  d <- as.matrix(dist(dt[, .(lon, lat)]))
  
  # identify pairs of points within .05 of each other (connected components)
  neighbors <- which(d < .05, arr.ind=T) # 0.05 is relatively arbitrary
  neighbors <- neighbors[!duplicated(t(apply(neighbors, 1, sort))),]
  
  # create graph (clusters) from pairwise connected components
  g <- graph_from_data_frame(neighbors, directed = FALSE)
  
  # identify cluster membership
  a <- data.frame(index = as.integer(names(clusters(g)$membership)), cluster = clusters(g)$membership)
  nclust <- clusters(g)$no
  
  return(list(m = d, mapping = a, nclust = nclust))
}

### Following loop identifies clusters of fires in each hour, 
# calculates their centers, updates cluster membership and centers as new fires 
# appear in subsequent hours

### Initialize at t0 
# (*eventually update for any/every hour i -- index starting at 0)

  t0 <- 5
  start <- test[hour == t0]
  
  # Calculate distance matrix
  l <- dist_matrix(start)
  
  # Assign cluster membership
  start[l$mapping$index, cluster := l$mapping$cluster]

# Two lists: 
# final is cluster centers by hour (and number of points originally observed in cluster)
# point_clusters is cluster membership of individual points by hour

  final <- list()
  final[[t0]] <- start[, .(lon = mean(lon), lat = mean(lat), points = .N), .(hour, cluster)]
  
  point_clusters <- list()
  point_clusters[[t0]] <- start

# Code test
# dt <- data.table(name = c("a", "b", "c", "d"))
# indices <- data.frame(position = c(4, 2, 1, 3), value = c(1, 2, 3, 4))
# dt[indices$position, cluster := indices$value]
# dt

ggplot() +
  xlim(146.5, 148.5) + ylim(-36.8, -35.4) +
  geom_point(aes(x = lon, y = lat, color = as.factor(cluster)), data = start) +
  geom_point(aes(x = lon, y = lat, color = as.factor(cluster)), shape = 4, data = final[[t0]])

# Run loop to update recursively for subsequent hours

# Testing:
# i <- 6
for (i in c(6:9)) {
  
  
  # Create data.table that combines cluster centers at time i-1 and points at i
  focus <- rbindlist(list(final[[i-1]][hour == i-1], test[hour == i]), use.names = TRUE, fill = TRUE)
  
  # Calculate distance matrix and clusters
  l <- dist_matrix(focus[, .(lon, lat)])
  focus[l$mapping$index, assigned := l$mapping$cluster]
  
  # Number of clusters observed at t-1
  prev_clust <- nrow(final[[i-1]][hour == (i-1)])
  
  # Append columns from distance matrix that capture distance between each 
  # new point at i and previous clusters' centers at i-1
  for(j in seq(prev_clust)) {
    focus[, paste0("c", j) := dist_get(l$m, j, c(1:nrow(focus)))]
  }
  
  # For each new point, identify closest of existing cluster centers and whether
  # it's within radius of 0.06
  mycols <- names(focus)[grepl("^c\\d+", names(focus))]
  focus[, ':=' (closest_clust = apply(.SD, 1, which.min),
                within_radius = ifelse(apply(.SD, 1, min) < .06, 1, 0)), .SDcols = mycols]
  
  # If point is within radius of existing cluster, assign it to that one;
  # otherwise assign to new cluster identified previously
  focus[, final_clust := ifelse(within_radius == 1, closest_clust, assigned)]
  
  # Note: rows that are missing values for number of points are new at time i
  # (i.e. they did not represent cluster centers)
  y <- focus[is.na(points), .(lon, lat, hour, cluster = final_clust)]
  point_clusters[[i]] <- y
  
  # Note that cluster centers are preserved for each time i (even if fire "died out")
  # When points = 1, the cluster did not observe any new points at time i
  # (i.e. only point is cluster center)
  z <- focus[, .(hour = i, lon = mean(lon), lat = mean(lat), points = .N), .(cluster = final_clust)]
  final[[i]] <- z
  
  print(ggplot() +
    xlim(146.5, 148.5) + ylim(-36.8, -35.4) +
    geom_point(aes(x = lon, y = lat, color = as.factor(cluster)), data = y) +
    geom_point(aes(x = lon, y = lat, color = as.factor(cluster)), shape = 4, data = z))
  
}

yy <- rbindlist(point_clusters, use.names = TRUE)
yy

zz <- rbindlist(final, use.names = TRUE)
zz