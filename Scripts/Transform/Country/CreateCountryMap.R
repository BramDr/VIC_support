library(fields)
library(raster)
library(rgdal)
library(rgeos)
rm(list = ls())

# Input
country.file <- "../../../Data/Primary/ThematicMapping"
country.out <- "../../../Data/Transformed/Country/country_6min_global.RDS"

# Load
country.s <- readOGR(dsn = country.file, layer = "TM_WORLD_BORDERS")
plot(country.s)

# Setup
country.r <- raster(nrow = 360 * 5, ncol = 720 * 5)
extent(country.r) <- c(-180, 180, -90, 90)
country.r <- rasterize(country.s, country.r, "UN")
plot(country.r)

# Calculate
country.m <- as.matrix(country.r)
country.m <- t(country.m[nrow(country.m):1, ])
image.plot(country.m)

# Save
dir.create(dirname(country.out))
saveRDS(country.m, country.out)
