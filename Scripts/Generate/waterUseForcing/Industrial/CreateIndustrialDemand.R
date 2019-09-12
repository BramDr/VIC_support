library(fields)
library(ncdf4)
rm(list = ls())

# Input
ind.tmp <- "Saves/industrialDemandSpread_30min_global.RDS"
mask.file <- "../../../../Data/Primary/VIC/domain_global.nc"
area.file <- "../../../../Data/Transformed/Routing/area_30min_global.RDS"
ind.out <- "Saves/industrialDemand_30min_global.RDS"
years <- 1979:2016

# Load
ind.agg <- readRDS(ind.tmp)
area <- readRDS(area.file)

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

# Calculate
ind.int <- ind.agg
for (x in 1:dim(ind.int)[1]) {
  for (y in 1:dim(ind.int)[2]) {
    if (is.na(mask[x, y]) || mask[x, y] == 0) {
      ind.int[x, y, ] <- NA
      next
    }

    for (z in 1:dim(ind.int)[3]) {
      if (!is.na(ind.agg[x, y, z])) {
        next
      }

      ind.int[x, y, z] <- 0
    }
  }
}

ind.dem <- array(NA, dim = c(dim(ind.int)[1:2], dim(ind.int)[3] * 12))
for (z in 1:dim(ind.int)[3]) {
  year <- years[z]
  print(paste0("Working on year ", year))

  for (m in 1:12) {
    ind.dem[, , (z - 1) * 12 + m] <- ind.int[, , z] / 365 / area * 1e3
  }
}

# Save
dir.create(dirname(ind.out))
saveRDS(ind.dem, ind.out)
