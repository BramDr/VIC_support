rm(list = ls())
library(raster)
library(fields)

height.file = "../../../../Data/Transformed/DEM/DEM_30m_Indus.tif"
height.5min.out = "./Saves/height_5min_Indus.RDS"
height.15min.out = "./Saves/height_15min_Indus.RDS"
height.30min.out = "./Saves/height_30min_Indus.RDS"
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
height = raster(height.file)
height = crop(height, extent.out)

# Calculate
height.5min.agg = aggregate(x = height, fact = 100, FUN = mean, na.rm = T)
plot(height.5min.agg)
height.15min.agg = aggregate(x = height, fact = 300, FUN = mean, na.rm = T)
plot(height.15min.agg)
height.30min.agg = aggregate(x = height, fact = 600, FUN = mean, na.rm = T)
plot(height.30min.agg)

height.15min.disagg = raster(nrow = length(out.lats), ncol = length(out.lons),
                           xmn = extent.out[1], xmx = extent.out[2],
                           ymn = extent.out[3], ymx = extent.out[4])
height.15min.disagg = resample(height.15min.agg, height.15min.disagg, method = "ngb")
plot(height.15min.disagg)
height.30min.disagg = raster(nrow = length(out.lats), ncol = length(out.lons),
                           xmn = extent.out[1], xmx = extent.out[2],
                           ymn = extent.out[3], ymx = extent.out[4])
height.30min.disagg = resample(height.30min.agg, height.30min.disagg, method = "ngb")
plot(height.30min.disagg)

height.5min = as.matrix(height.5min.agg)
height.5min = t(height.5min[nrow(height.5min):1,])
image.plot(height.5min)
height.15min = as.matrix(height.15min.disagg)
height.15min = t(height.15min[nrow(height.15min):1,])
image.plot(height.15min)
height.30min = as.matrix(height.30min.disagg)
height.30min = t(height.30min[nrow(height.30min):1,])
image.plot(height.30min)

# Save
dir.create(dirname(height.5min.out))
saveRDS(height.5min, height.5min.out)
dir.create(dirname(height.15min.out))
saveRDS(height.15min, height.15min.out)
dir.create(dirname(height.30min.out))
saveRDS(height.30min, height.30min.out)
