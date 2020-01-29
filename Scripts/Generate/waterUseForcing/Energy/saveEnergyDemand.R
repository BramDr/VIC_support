library(fields)
library(ncdf4)
rm(list = ls())

# Input
ene.file <- "../../../../Data/Transformed/Energy/energyWithdrawal_location.csv"
mask.file <- "../../../../Data/Transformed/Routing/mask_30min_global.RDS"
area.file <- "../../../../Data/Transformed/Routing/area_30min_global.RDS"
dem.out <- "Saves/energyDemand_30min_global.RDS"
years <- 1979:2016

# Load
mask <- readRDS(mask.file)
area <- readRDS(area.file)
ene <- read.csv(ene.file)

# Setup
lats <- seq(-89.75, 89.75, by = 0.5)
lons <- seq(-179.75, 179.75, by = 0.5)

# Calculate
dem.map <- array(NA, dim = c(length(lons), length(lats), length(years) * 12))
for (x in 1:dim(dem.map)[1]) {
  for (y in 1:dim(dem.map)[2]) {
    if (is.na(mask[x, y])) {
      next
    }

    dem.map[x, y, ] <- 0
  }
}

for (z in 1:length(years)) {
  year <- years[z]
  print(paste0("Working on year ", year))

  ene.sel <- ene[ene$Year_operational <= year, ]

  for (i in 1:nrow(ene.sel)) {
    x <- which(lons == ene.sel$Lon_cel[i])
    y <- which(lats == ene.sel$Lat_cel[i])

    if (is.na(mask[x, y])) {
      next
    }

    dem.map[x, y, ((z - 1) * 12 + 1):(z * 12)] <- dem.map[x, y, ((z - 1) * 12 + 1):(z * 12)] + (ene.sel$Withdrawal[i] / area[x, y] * 1e3)
  }
}

# Save
dir.create(dirname(dem.out))
saveRDS(dem.map, dem.out)
