# Reading CFA info
library(sf)
library(tidyverse)
library(rmapshaper)
shape <- st_read(file.choose())
str(shape$geometry)
sshape$geometry[1:5, ]
st_geometry(shape) %>% plot()
stations <- st_geometry(shape)

ggplot(data=stations) + geom_sf()
ggplot(data=shape) + geom_sf()

boundaries <- st_read(file.choose())
ggplot(data=boundaries) + geom_sf()

bound_sm <- ms_simplify(boundaries, keep_shapes = TRUE)
ggplot(data=bound_sm) + geom_sf()

