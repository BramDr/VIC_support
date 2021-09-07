library(fields)
library(raster)
library(rgdal)
library(rgeos)
rm(list = ls())

# Input
country.file <- "../../../Data/Primary/ThematicMapping"
country.out <- "../../../Data/Transformed/Country/country_5min_Indus.RDS"
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
country.s <- readOGR(dsn = country.file, layer = "TM_WORLD_BORDERS")
plot(country.s)

# Setup
country.r <- raster(nrows = length(out.lats), ncols = length(out.lons),
                    xmn = extent.out[1], xmx = extent.out[2], 
                    ymn = extent.out[3], ymx = extent.out[4])
country.r <- rasterize(country.s, country.r, "UN")
plot(country.r)

# Calculate
country.m <- as.matrix(country.r)
country.m <- t(country.m[nrow(country.m):1, ])
image.plot(country.m)

# Save
dir.create(dirname(country.out))
saveRDS(country.m, country.out)
