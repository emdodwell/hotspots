# Reading CFA info
library(sf)
library(tidyverse)
library(rmapshaper)
shape <- st_read("SDM687777/ll_gda94/sde_shape/whole/VIC/VMFEAT/layer/geomark_point.shp")
str(shape$geometry)
shape$geometry[1:5, ]
st_geometry(shape) %>% plot()
stations <- st_geometry(shape)

ggplot(data=stations) + geom_sf()
ggplot(data=shape) + geom_sf()

boundaries <- st_read("SDM688983/ll_gda94/sde_shape/whole/VIC/VMADMIN/layer/cfa_region.shp")
ggplot(data=boundaries) + geom_sf()

bound_sm <- ms_simplify(boundaries, keep_shapes = TRUE)
ggplot(data=bound_sm) + geom_sf()

