library(fields)
library(ncdf4)
rm(list = ls())

# Input
ind.tmp <- "Saves/industrialDemandSpread_30min_global_total.RDS"
mask.file <- "../../../../Data/Transformed/Routing/mask_30min_global.RDS"
ind.out <- "Saves/industrialDemand_30min_global_total.RDS"
years <- 1979:2016

# Load
ind.agg <- readRDS(ind.tmp)
mask <- readRDS(mask.file)

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

ind.dem <- array(NA, dim = c(dim(ind.int)[1:2], dim(ind.int)[3]))
for (z in 1:dim(ind.int)[3]) {
  year <- years[z]
  print(paste0("Working on year ", year))

  for (m in 1:12) {
    ind.dem[, , z] <- ind.int[, , z]
  }
}

# Save
dir.create(dirname(ind.out))
saveRDS(ind.dem, ind.out)
