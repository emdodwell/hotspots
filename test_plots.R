library(data.table)
library(ggplot2)
library(plotly)

# Polygon for VIC
states <- absmapsdata::state2016 %>% 
  filter(state_code_2016 %in% c("2"))

yy <- readRDS("data/point_clusters_20200101_20200110_test.RDS")
zz <- readRDS("data/clusters_20200101_20200110_test.RDS")

yy[, cluster := as.factor(cluster)]
zz[, cluster := as.factor(cluster)]


# p <- ggplot() +
#   xlim(146.5, 148.5) + ylim(-36.8, -35.4) +
#   geom_point(aes(x = lon, y = lat, color = cluster, frame = ind), data = yy) +
#   geom_point(aes(x = lon, y = lat, color = cluster, frame = ind), shape = 4, data = zz) 
# 
# ggplotly(p) %>% animation_opts(transition = 0)

# ggplot(states) +
#   geom_sf() +
#   #xlim(146.5, 148.5) + ylim(-36.8, -35.4) +
#   geom_point(aes(x = lon, y = lat, color = cluster), data = yy) +
#   geom_point(aes(x = lon, y = lat, color = cluster), shape = 4, data = zz) +
#   facet_wrap(~ ind, ncol = 3)
# 

p <- ggplot(states) +
  geom_sf() + 
  geom_point(aes(x = lon, y = lat, color = cluster, frame = ind), shape = 4, data = zz[1:5000])

ggplotly(p) %>% animation_opts(transition = 0)

