library(fields)
library(raster)
library(ncdf4)
rm(list = ls())

# Input
pop.dir <- "../../../Data/Primary/Hyde/"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
pop.out <- "../../../Data/Transformed/Population/population_30min_global.RDS"
years <- 1979:2016

# Load
pop.files <- list.files(pop.dir, pattern = "popc_", full.names = T)
pop.years <- gsub(pop.files, pattern = ".*popc_", replacement = "")
pop.years <- as.numeric(gsub(pop.years, pattern = "AD.asc", replacement = ""))

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

# Calculate
pop <- array(NA, dim = c(dim(mask), length(years)))
for (z in 1:length(years)) {
  year <- years[z]
  print(paste0("Working on year ", year))

  start <- which.min(pop.years - year > 0)
  s.year <- pop.years[start]

  s.pop <- as.matrix(read.table(pop.files[start], skip = 6))
  s.pop <- t(s.pop[nrow(s.pop):1, ])
  s.pop[s.pop == -9999] <- NA

  s.pop.r <- raster(s.pop)
  extent(s.pop.r) <- c(-90, 90, -180, 180)
  s.pop.agg <- aggregate(s.pop.r, fact = 6, fun = sum, na.rm = T)
  s.pop.agg <- as.matrix(s.pop.agg)

  if (s.year == year) {
    pop[, , z] <- s.pop.agg
  } else {
    end <- which.min(abs(pop.years - year)) + 1
    e.year <- pop.years[end]
    e.pop <- as.matrix(read.table(pop.files[end], skip = 6))
    e.pop <- t(e.pop[nrow(e.pop):1, ])
    e.pop[e.pop == -9999] <- NA

    e.pop.r <- raster(e.pop)
    extent(e.pop.r) <- c(-90, 90, -180, 180)
    e.pop.agg <- aggregate(e.pop.r, fact = 6, fun = sum, na.rm = T)
    e.pop.agg <- as.matrix(e.pop.agg)

    year.frac <- (year - s.year) / (e.year - s.year)
    pop[, , z] <- s.pop.agg + year.frac * (e.pop.agg - s.pop.agg)
  }
}

for (x in 1:dim(mask)[1]) {
  for (y in 1:dim(mask)[2]) {
    if (is.na(mask[x, y])) {
      pop[x, y, ] <- NA
      next
    }

    if (is.na(pop[x, y, 1])) {
      pop[x, y, ] <- 0
    }
  }
}

for (z in 1:dim(pop)[3]) {
  image.plot(pop[, , z], main = z)
}

# Save
dir.create(dirname(pop.out))
saveRDS(pop, pop.out)
