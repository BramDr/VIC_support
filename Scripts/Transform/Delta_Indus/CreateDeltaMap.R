library(fields)
library(raster)
rm(list = ls())

# Input
delta.file <- "../../../Data/Primary/Tessler2015/global_map_2.5min.tiff"
delta.out <- "../../../Data/Transformed/Delta/delta_5min_Indus.RDS"
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
delta <- raster(delta.file)
delta <- crop(delta, extent.out)

# Setup
delta.function <- function(x, na.rm = na.rm) {
  nodata.sum <- sum(is.na(x))
  data.sum <- sum(!is.na(x))

  if (nodata.sum / (nodata.sum + data.sum) > 0.5) {
    return(0)
  }

  data <- x[!is.na(x)]
  data.t <- table(data)
  data.t <- names(data.t[order(data.t, decreasing = T)])

  return(as.numeric(data.t[1]))
}

# Calculate
delta.agg <- aggregate(x = delta, fact = 2, FUN = delta.function, na.rm = T)
delta.agg <- as.matrix(delta.agg)
delta.agg <- t(delta.agg[dim(delta.agg)[1]:1,])
delta.agg[delta.agg == 0] <- NA
image.plot(delta.agg)

# Save
dir.create(dirname(delta.out))
saveRDS(delta.agg, delta.out)
