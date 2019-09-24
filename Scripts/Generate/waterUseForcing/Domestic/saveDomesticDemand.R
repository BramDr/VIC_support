library(fields)
library(ncdf4)
rm(list = ls())

# Input
dom.tmp <- "Saves/domesticDemandPC_30min_global.RDS"
temp.dir <- "../../../../Data/Transformed/WFDEI/"
mask.file <- "../../../../Data/Transformed/Routing/mask_30min_global.RDS"
area.file <- "../../../../Data/Transformed/Routing/area_30min_global.RDS"
pop.file <- "../../../../Data/Transformed/Population/population_30min_global.RDS"
dom.out <- "Saves/domesticDemand_30min_global.RDS"
years <- 1979:2016

# Load
dom.agg <- readRDS(dom.tmp)
pop <- readRDS(pop.file)
area <- readRDS(area.file)
mask <- readRDS(mask.file)

tmon.files <- list.files(temp.dir, pattern = "monthly_", full.names = T)
tyear.files <- list.files(temp.dir, pattern = "yearly_", full.names = T)
tyear.max.files <- list.files(temp.dir, pattern = "yearlyMonthmax_", full.names = T)
tyear.min.files <- list.files(temp.dir, pattern = "yearlyMonthmin_", full.names = T)

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

dom.dem <- array(NA, dim = c(dim(dom.int)[1:2], dim(dom.int)[3] * 12))
for (z in 1:dim(dom.int)[3]) {
  year <- years[z]
  print(paste0("Working on year ", year))

  tmon.file <- grep(tmon.files, pattern = year, value = T)
  tyear.file <- grep(tyear.files, pattern = year, value = T)
  tyear.max.file <- grep(tyear.max.files, pattern = year, value = T)
  tyear.min.file <- grep(tyear.min.files, pattern = year, value = T)

  nc <- nc_open(tmon.file)
  tmon <- ncvar_get(nc, "Tair")
  nc_close(nc)
  nc <- nc_open(tyear.file)
  tyear <- ncvar_get(nc, "Tair")
  nc_close(nc)
  nc <- nc_open(tyear.max.file)
  tyear.max <- ncvar_get(nc, "Tair")
  nc_close(nc)
  nc <- nc_open(tyear.min.file)
  tyear.min <- ncvar_get(nc, "Tair")
  nc_close(nc)

  for (m in 1:12) {
    frac <- 1 + (tmon[, , m] - tyear) / (tyear.max - tyear.min) * 0.1
    dom.dem[, , (z - 1) * 12 + m] <- dom.int[, , z] * pop[, , z] / 365 / area * 1e3 * frac
  }
}

# Save
dir.create(dirname(dom.out))
saveRDS(dom.dem, dom.out)
