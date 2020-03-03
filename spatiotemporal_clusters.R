library(tidyverse)
library(readr)
library(absmapsdata)
library(ggplot2)
library(sf)
library(lubridate)
library(tsibble)
library(data.table)
library(igraph)
library(usedist)
library(plotly)

options(tibble.width = Inf)

# Himawari-8 Hotspots
x <- fread("data/H08_20200101_0000_1MWLFbet_FLDK.06001_06001.csv", quote = "'")
setnames(x, "#obstime", "obstime")

# Create date and hour variables
x[, ':=' (t = ymd_hms(obstime), h = ymd_h(substr(obstime, 1, 13)))]
vic_fires <- x[(lon > 141 & lon < 149) & (lat > -39 & lat < -34)]

# Polygon for VIC
states <- absmapsdata::state2016 %>% 
  filter(state_code_2016 %in% c("2"))

# To Dos:
# Apply filter for firepower intensity
# Incorporate logic for new clusters to form if fire not seen for 24 hours
# Index by hour - add back hours with no data (use of tsibble?)
# Note: Oz boundaries (lon > 112 & lon < 155) & (lat > -44 & lat < -10)

################################################################################
## Assign fires to clusters and update at each time interval (currently hour) ##
################################################################################

# Subset of days for code writing
test <- vic_fires[t > ymd("2020-01-01") & t < ymd("2020-01-15"), .(lon, lat, h, firepower, t07, ref3)]
test[, ind := as.integer(factor(h))]

ggplot(states) + 
  geom_sf() +
  geom_point(aes(x = lon, y = lat), data = test)

# Progression of fires by hour
ggplot() +
  xlim(146.5, 148.5) + ylim(-36.8, -35.4) +
  geom_point(aes(x = lon, y = lat), data = test[ind == 1], alpha = .3) +
  geom_point(aes(x = lon, y = lat), data = test[ind == 2], color = "green", alpha = .5) +
  geom_point(aes(x = lon, y = lat), data = test[ind == 3], color = "red", alpha = .5) +
  geom_point(aes(x = lon, y = lat), data = test[ind == 4], color = "blue", alpha = .3) +
  geom_point(aes(x = lon, y = lat), data = test[ind == 5], color = "black", alpha = .3)


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

### Initialize at hour index = 1
init <- test[ind == 1]

# Calculate distance matrix between all hotspots
l <- dist_matrix(init)

# Assign cluster membership
init[l$mapping$index, cluster := l$mapping$cluster]

# Two lists: 
# final is cluster centers by hour (and number of points originally observed in cluster)
# point_clusters is cluster membership of individual points by hour

final <- list()
final[[1]] <- init[, .(lon = mean(lon), lat = mean(lat), points = .N), .(ind, h, cluster)] #[, last_seen := max(h), cluster] # include last hour

point_clusters <- list()
point_clusters[[1]] <- init[, .(lon, lat, h, ind, cluster)]

# Code test
# dt <- data.table(name = c("a", "b", "c", "d"))
# indices <- data.frame(position = c(4, 2, 1, 3), value = c(1, 2, 3, 4))
# dt[indices$position, cluster := indices$value]
# dt

ggplot() +
  xlim(146.5, 148.5) + ylim(-36.8, -35.4) +
  geom_point(aes(x = lon, y = lat, color = as.factor(cluster)), data = init) +
  geom_point(aes(x = lon, y = lat, color = as.factor(cluster)), shape = 4, data = final[[1]])

# Run loop to update recursively for subsequent hours


for (i in c(2:test[, uniqueN(ind)])) {
  
  #i <- 2
  # Create data.table that combines cluster centers at time i-1 and points at i
  focus <- rbindlist(list(final[[i-1]][ind == (i-1)], test[ind == i]), use.names = TRUE, fill = TRUE)
  
  # Calculate distance matrix and clusters
  l <- dist_matrix(focus[, .(lon, lat)])
  focus[l$mapping$index, assigned := l$mapping$cluster]
  
  # Number of clusters observed at previous time
  prev_clust <- nrow(final[[i-1]][ind == (i-1)])
  
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
  # include last_seen hour of final_cluster -- if h-last_seen > 24, update cluster assignment
  
  # Note: rows that are missing values for number of points are new at time i
  # (i.e. they did not represent cluster centers)
  y <- focus[is.na(points), .(lon, lat, h, ind, cluster = final_clust)]
  point_clusters[[i]] <- y
  
  # Note that cluster centers are preserved for each time i (even if fire "died out")
  # When points = 1, the cluster did not observe any new points at time i
  # (i.e. only point is cluster center)
  # Captures how cluster exists in that hour (i.e. point == 1 if no new information since last time period)
  z <- focus[, .(ind = i, h = focus[ind == i, unique(h)], lon = mean(lon), lat = mean(lat), points = .N), .(cluster = final_clust)]
  final[[i]] <- z
  
  
}

# Combine hourly point cluster assignments and cluster summaries
yy <- rbindlist(point_clusters, use.names = TRUE)
yy

zz <- rbindlist(final, use.names = TRUE)
zz

# saveRDS(yy, "data/point_clusters_20200101_20200110_test.RDS")
# saveRDS(zz, "data/clusters_20200101_20200110_test.RDS")

################################################################################
######################## Assign each fire to CFA Fire Station ##################
################################################################################

# library(raster)
# maxs <- pointDistance(first_fire[, .(lon, lat)], stations[, .(lon,lat)], lonlat = TRUE)

# Identify first instance of each fire
first_fire <- zz[, head(.SD, 1), by = "cluster"]

# CFA Fire Station locations
stations <- readRDS("data/cfa_stations/cfa_station_locations.RDS")

a <- first_fire[, .(paste0(c(lon, lat), collapse = ",")), by = cluster]
b <- stations[, .(paste0(c(name, lon, lat), collapse = ",")), by = name]

# Find Euclidean distance between all stations and all fires (note: super hacky)
c <- CJ(a[, V1], b[, V1])

c[, ':=' (flon = as.numeric(sapply(V1, function(x) strsplit(x, ",")[[1]][1])),
          flat = as.numeric(sapply(V1, function(x) strsplit(x, ",")[[1]][2])),
          station = sapply(V2, function(x) strsplit(x, ",")[[1]][1]),
          slon = as.numeric(sapply(V2, function(x) strsplit(x, ",")[[1]][2])),
          slat = as.numeric(sapply(V2, function(x) strsplit(x, ",")[[1]][3])))]
c[, dist := sqrt((slon - flon)^2 + (slat - flat)^2)]
c[, closest := ifelse(dist == min(dist), 1, 0), V1]
c <- unique(c[closest == 1][, .(V1, station, slon, slat)])

setkey(c, V1)
setkey(a, V1)
a <- c[a]
a[, V1 := NULL]


# Assign stations to clusters
setkey(a, cluster)
setkey(zz, cluster)
zz <- a[zz]


# for (i in nrow(first:fire)){
#   for j in nrow()
#   
# }
# 
# mycols <- "cluster"
# zz[, difference := lapply(.SD, function(h) difftime(h, lag(h))), .SDcols = mycols]

################################################################################
################ Add back features - averages by hour and cluster ##############
################################################################################

setkey(yy, ind, h, lon, lat)
setkey(test, ind, h, lon, lat)

bypoint <- test[yy]
avgs <- bypoint[, .(firepower = round(mean(firepower, na.rm = TRUE), 1), 
                    t07 = round(mean(t07, na.rm = TRUE), 1), 
                    ref3 = round(mean(ref3, na.rm = TRUE), 3)), .(ind, h, cluster)]
avgs

setkey(avgs, ind, h, cluster)
setkey(zz, ind, h, cluster)
byclust <- avgs[zz]

#saveRDS(byclust, "data/data_20200101_20200110_test.RDS")
