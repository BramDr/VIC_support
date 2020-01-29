library(fields)
library(ncdf4)
rm(list = ls())

# Input
gw.out <- "../../../Data/Transformed/Energy/energyGroundwaterFraction_30min_global.RDS"

# Setup
lats <- seq(-89.75, 89.75, by = 0.5)
lons <- seq(-179.75, 179.75, by = 0.5)

# Calculate
gw.map <- array(NA, dim = c(length(lons), length(lats)))
for (x in 1:dim(gw.map)[1]) {
  for (y in 1:dim(gw.map)[2]) {
    gw.map[x, y] <- 0
  }
}
image.plot(gw.map)

# Save
dir.create(dirname(gw.out))
saveRDS(gw.map, gw.out)
