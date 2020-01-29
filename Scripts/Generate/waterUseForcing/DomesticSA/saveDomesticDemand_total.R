library(fields)
library(ncdf4)
rm(list = ls())

# Input
dom.tmp <- "Saves/domesticDemandPC_30min_global_total.RDS"
mask.file <- "../../../../Data/Transformed/Routing/mask_30min_global.RDS"
pop.file <- "../../../../Data/Transformed/Population/population_30min_global.RDS"
dom.out <- "Saves/domesticDemand_30min_global_total.RDS"
years <- 1979:2016

# Load
dom.agg <- readRDS(dom.tmp)
pop <- readRDS(pop.file)
mask <- readRDS(mask.file)

# Setup
getNearest <- function(x, y, z, data) {
  for (dis in 1:1000) {
    x.min <- max(x - dis, 1)
    y.min <- max(y - dis, 1)
    x.max <- min(x + dis, dim(data)[1])
    y.max <- min(y + dis, dim(data)[2])

    val <- mean(data[x.min:x.max, y.min:y.max, z], na.rm = T)

    if (!is.na(val)) {
      return(mean(val))
    }
  }

  warning("Nearest out of iterations")
  return(NA)
}

# Calculate
dom.int <- dom.agg
for (x in 1:dim(dom.int)[1]) {
  for (y in 1:dim(dom.int)[2]) {
    if (is.na(mask[x, y]) || mask[x, y] == 0) {
      next
    }

    for (z in 1:dim(dom.int)[3]) {
      if (!is.na(dom.agg[x, y, z])) {
        next
      }

      dom.int[x, y, z] <- getNearest(x, y, z, dom.agg)
    }
  }
}

dom.dem <- array(NA, dim = c(dim(dom.int)[1:2], dim(dom.int)[3]))
for (z in 1:dim(dom.int)[3]) {
  year <- years[z]
  print(paste0("Working on year ", year))

  for (m in 1:12) {
    dom.dem[, , z] <- dom.int[, , z] * pop[, , z]
  }
}

# Save
dir.create(dirname(dom.out))
saveRDS(dom.dem, dom.out)
