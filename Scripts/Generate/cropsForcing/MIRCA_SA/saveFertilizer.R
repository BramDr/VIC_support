library(fields)
library(ncdf4)
rm(list = ls())

# Input
fert.dir <- "../../../../Data/Primary/GGCMI/Phase3/n-fertilizer/histsoc"
crop.file <- "./Saves/crop_mapping.csv"
fertilizer.out <- "./Saves/fertilizer_30min_global.RDS"
years <- 1979:2016

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
fert.files <- list.files(fert.dir, pattern = "fertilizer_application", full.names = T)
man.file <- list.files(fert.dir, pattern = "manure_application", full.names = T)

nc <- nc_open(man.file)
nc.years <- nc$dim$time$vals
man.n <- ncvar_get(nc, nc$var$manurerate)
nc_close(nc)
man.n[is.na(man.n)] <- 0

# Setup
lons <- seq(-179.75, 179.75, by = 0.5)
lats <- seq(-89.75, 89.75, by = 0.5)

# Calculate
fertilizer.n <- array(0, dim = c(length(lons), length(lats), nrow(crops), length(years)))
fertilizer.p <- array(0, dim = c(length(lons), length(lats), nrow(crops), length(years)))
fertilizer.k <- array(0, dim = c(length(lons), length(lats), nrow(crops), length(years)))
fertilizer.dvs <- array(0, dim = c(length(lons), length(lats), nrow(crops), length(years)))

i <- 5
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  if (is.na(crops$fertilizer[i])) {
    next
  }

  fert.file <- grep(fert.files, pattern = paste0("_", crops$fertilizer[i], "_"), value = T)

  nc <- nc_open(fert.file)
  fert.n <- ncvar_get(nc, nc$var$fertrate)
  nc_close(nc)
  fert.n[is.na(fert.n)] <- 0

  factor <- 0.66

  fertilizer.n[, , i, 1:(dim(fertilizer.n)[4] - 1)] <- (fert.n[, dim(fert.n)[2]:1, nc.years %in% years] + man.n[, dim(man.n)[2]:1, nc.years %in% years]) * factor
  fertilizer.n[, , i, dim(fertilizer.n)[4]] <- fertilizer.n[, , i, (dim(fertilizer.n)[4] - 1)] # add 2016
}

# Save
fertilizer.out.tmp <- gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerDVS_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.dvs, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerN_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.n, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerP_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.p, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerK_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.k, fertilizer.out.tmp)
