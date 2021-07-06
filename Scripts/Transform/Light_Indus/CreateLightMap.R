library(fields)
library(raster)
rm(list = ls())

# Input
light.file <- "../../../Data/Primary/NASA/BlackMarble_2016_01deg_gray_geo.tif"
light.out <- "../../../Data/Transformed/Light/light_5min_Indus.RDS"

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
light <- raster(light.file)
light <- crop(light, extent.out)
plot(light)

# Calculate
light.adj = raster(nrows = length(out.lats), ncols = length(out.lons),
                   xmn = extent.out[1], xmx = extent.out[2], 
                   ymn = extent.out[3], ymx = extent.out[4])
light.adj = resample(light, light.adj)
plot(light.adj)

light.mat = as.matrix(light.adj)
light.mat = t(light.mat[nrow(light.mat):1,])
image.plot(light.mat)

# Save
dir.create(dirname(light.out))
saveRDS(light.mat, light.out)
