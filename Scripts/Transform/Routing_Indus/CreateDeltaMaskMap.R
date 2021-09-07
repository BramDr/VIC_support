library(ncdf4)
library(fields)
rm(list = ls())

# Input
path.basin <- "../../../Data/Transformed/Delta/deltaBasins_5min_Indus.RDS"
mask.out <- "../../../Data/Transformed/Routing/deltaMask_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
basin <- readRDS(path.basin)

# Setup
points <- data.frame(lat = numeric(), lon = numeric(), name = character(), stringsAsFactors = F)
points[nrow(points) + 1, ] <- c(29.75, 70.75, "Indus")

points$lat <- as.numeric(points$lat)
points$lon <- as.numeric(points$lon)

# Calculate
for (i in 1:nrow(points)) {
  x <- which.min(abs(out.lons - points$lon[i]))
  y <- which.min(abs(out.lats - points$lat[i]))
  
  id <- basin[x, y]
  
  if (!is.na(id)) {
    points$basin[i] <- id
    points$size[i] <- sum(na.omit(c(basin)) == id)
  } else {
    print(paste0("Point ", points[i, ], " falls outside of basin mask"))
  }
}

mask <- array(NA, dim = c(dim(basin)))
for (x in 1:dim(basin)[1]) {
  for (y in 1:dim(basin)[2]) {
    if (!is.na(basin[x, y]) && basin[x, y] == points$basin[i]) {
      mask[x, y] <- 1
    }
  }
}
image.plot(mask)

# Save
dir.create(dirname(mask.out))
saveRDS(mask, mask.out)
