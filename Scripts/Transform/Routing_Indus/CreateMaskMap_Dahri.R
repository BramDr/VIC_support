library(fields)
library(raster)
rm(list = ls())

# Input
direction.file <- "../../../Data/Primary/Dahri2020/rout_in/fdr_uib.asc"
mask.out <- "../../../Data/Transformed/Routing/mask_Dahri_5min_Indus.RDS"
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
direction <- raster(direction.file)
direction <- crop(direction, extent.out)
direction <- extend(direction, extent.out)
direction <- as.matrix(direction)
direction <- t(direction[dim(direction)[1]:1,])
image.plot(direction)

# Calculate
mask <- direction
mask[!is.na(mask)] = 1
image.plot(mask)

# Save
dir.create(dirname(mask.out))
saveRDS(mask, mask.out)
