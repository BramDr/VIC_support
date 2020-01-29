library(fields)
library(raster)
rm(list = ls())

# Input
delta.file <- "../../../Data/Primary/Tessler2015/global_map_2.5min.tiff"
delta.out <- "../../../Data/Transformed/Delta/delta_30min_global.RDS"

# Load
delta <- raster(delta.file)
delta[delta == 0] <- NA

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
delta.agg <- aggregate(x = delta, fact = 12, fun = delta.function, na.rm = T)
delta.agg[delta.agg == 0] <- NA
plot(delta.agg)

delta.m <- as.matrix(delta.agg)
delta.m <- t(delta.m[nrow(delta.m):1, ])
image.plot(delta.m)

# Save
dir.create(dirname(delta.out))
saveRDS(delta.m, delta.out)
