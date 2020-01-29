library(ncdf4)
library(fields)
rm(list = ls())

# Input
distance.file <- "../../../Data/Primary/VIC/RVIC_input_global.nc"
mask.file <- "../../../Data/Transformed/Routing/mask_30min_global.RDS"
distance.out <- "../../../Data/Transformed/Routing/distance_30min_global.RDS"

# Load
mask <- readRDS(mask.file)

nc <- nc_open(filename = distance.file)
distance <- ncvar_get(nc = nc, "flow_distance")
nc_close(nc = nc)
image.plot(distance)

# Calculate
for (x in 1:dim(mask)[1]) {
  for (y in 1:dim(mask)[2]) {
    if (is.na(mask[x, y])) {
      distance[x, y] <- NA
    } else if (is.na(distance[x, y])) {
      distance[x, y] <- max(distance[, y], na.rm = T)

      if (is.infinite(distance[x, y])) {
        distance[x, y] <- mean(distance[is.finite(distance)], na.rm = T)
      }
    }
  }
}
image.plot(distance)

# Save
dir.create(dirname(distance.out))
saveRDS(distance, distance.out)
