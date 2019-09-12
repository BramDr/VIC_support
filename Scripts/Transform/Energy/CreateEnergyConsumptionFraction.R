library(fields)

rm(list = ls())

# Input
ene.file <- "../../../Data/Transformed/Energy/energyWithdrawal_location.csv"
cons.out <- "../../../Data/Transformed/Energy/energyConsumptionFraction_30min_global.RDS"

# Load
ene <- read.csv(ene.file)

# Setup
lats <- seq(-89.75, 89.75, by = 0.5)
lons <- seq(-179.75, 179.75, by = 0.5)

# Calculate
with.map <- array(0, dim = c(length(lons), length(lats)))
cons.map <- array(0, dim = c(length(lons), length(lats)))
cons.frac.map <- array(0, dim = c(length(lons), length(lats)))

for (i in 1:nrow(ene)) {
  x <- which(lons == ene$Lon_cel[i])
  y <- which(lats == ene$Lat_cel[i])

  with.map[x, y] <- with.map[x, y] + ene$Withdrawal[i]
  cons.map[x, y] <- cons.map[x, y] + ene$Consumption[i]
}

sel <- !is.na(with.map) & with.map > 0
cons.frac.map[sel] <- cons.map[sel] / with.map[sel]
image.plot(cons.frac.map)

# Save
dir.create(dirname(cons.out))
saveRDS(cons.frac.map, cons.out)
