library(raster)
library(fields)
rm(list = ls())

# Input
distance.file <- "../../../Data/Primary/Hydro1kHydroSHEDS/DRT_12th_FDISTANCE_globe.asc"
distance.out <- "../../../Data/Transformed/Routing/distance_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

extent.out <- extent(min(out.lons) - resolution / 2, 
                     max(out.lons) + resolution / 2, 
                     min(out.lats) - resolution / 2, 
                     max(out.lats) + resolution / 2)

# Load
distance <- raster(distance.file)
distance <- crop(distance, extent.out)
distance <- as.matrix(distance)
distance <- t(distance[dim(distance)[1]:1,])
image.plot(distance)

# Save
dir.create(dirname(distance.out))
saveRDS(distance, distance.out)
