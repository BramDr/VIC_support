library(fields)
library(raster)
rm(list = ls())

depth.file <- "../../../../Data/Primary/Pelletier2016/average_soil_and_sedimentary-deposit_thickness.tif"
depth.out <- "../../../../Data/Transformed/Soil/Pelletier2016/depth_5min_Indus.RDS"

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
depth <- raster(depth.file)

# Setup
depth <- crop(depth, extent.out)
depth[depth == 255] = NA
plot(depth)

# Calculate
depth.agg = aggregate(x = depth, fact = 10, FUN = max)
depth.agg = as.matrix(depth.agg)
depth.agg = t(depth.agg[nrow(depth.agg):1,])
image.plot(depth.agg)

# Save
dir.create(dirname(depth.out))
saveRDS(depth.agg, depth.out)
