library(fields)

rm(list = ls())

# Input
cons.out <- "../../../Data/Transformed/Domestic/domesticConsumptionFraction_30min_global.RDS"

# Setup
lats <- seq(-89.75, 89.75, by = 0.5)
lons <- seq(-179.75, 179.75, by = 0.5)

# Calculate
cons.map <- array(NA, dim = c(length(lons), length(lats)))
for (x in 1:dim(cons.map)[1]) {
  for (y in 1:dim(cons.map)[2]) {
    cons.map[x, y] <- 0.15
  }
}
image.plot(cons.map)

# Save
dir.create(dirname(cons.out))
saveRDS(cons.map, cons.out)
