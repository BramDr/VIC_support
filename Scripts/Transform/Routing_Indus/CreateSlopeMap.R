library(ncdf4)
library(raster)
library(fields)
rm(list = ls())

slope.file <- "../../../Data/Primary/worldBank/Slope/slope.tif"
slope.angle.out <- "../../../Data/Transformed/Routing/slope_angle_5min_Indus.RDS"
slope.out <- "../../../Data/Transformed/Routing/slope_5min_Indus.RDS"
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

slope <- raster(slope.file)
extent(slope) <- c(-180, 180, -60, 90)
slope <- crop(slope, extent.out)
slope <- as.matrix(slope)
slope <- t(slope[dim(slope)[1]:1,])
image.plot(slope)

slope.fin <- tan(slope * (pi / 180))
image.plot(slope.fin)

dir.create(dirname(slope.angle.out))
saveRDS(slope, slope.angle.out)
dir.create(dirname(slope.out))
saveRDS(slope.fin, slope.out)

