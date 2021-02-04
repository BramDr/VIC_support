library(raster)
library(fields)
rm(list = ls())

# Input
bin.support.file <- "./binFunctions.R"
grid.file <- "../../../Data/Primary/LPJmL_Indus/Total/grid.bin"
drainage.file <- "../../../Data/Primary/LPJmL_Indus/Total/drainage.bin"
distance.out <- "../../../Data/Transformed/Routing/distance_5min_Indus.LPJmL.RDS"
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
source(bin.support.file)

grid = loadBin(grid.file, "integer", 2)
drainage = loadBin(drainage.file, "integer", 4)
distance = mapBin(grid, drainage)[[1]][[2]]
plot(distance)

# Set extent to output extent
distance = extend(distance, extent.out)
distance = crop(distance, extent.out)
plot(distance)

# Set to matrix
distance = as.matrix(t(distance))
distance = distance[,ncol(distance):1]
image.plot(distance)

# Save
dir.create(dirname(distance.out))
saveRDS(distance, distance.out)
