library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Nijssen2001/0.5deg/global_soil_param_new"
header.file <- "../../../Data/Primary/Nijssen2001/2deg/world.soil.parameter.hdr"
soil.out <- "../../../Data/Transformed/Parameters/soil_Nijssen30min_30min_global.RDS"

# Load
soil <- read.table(soil.file)
header <- read.table(header.file, stringsAsFactors = F)
header <- c(unlist(header[, 1:27]), paste0("bubble[", 1:3, "]"), paste0("quartz[", 1:3, "]"), unlist(header[, 30:48]), "frozen_flag")

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
nlayers <- 3

expt.idx <- which(header == "N[1]")
ksat.idx <- which(header == "Ksat[1]")
phi_s.idx <- which(header == "phi_s[1]")
bubble.idx <- which(header == "bubble[1]")
quartz.idx <- which(header == "quartz[1]")
bulk_density.idx <- which(header == "bulk_density[1]")
soil_density.idx <- which(header == "soil_density[1]")
wcr.idx <- which(header == "Wcr[1]")
wp.idx <- which(header == "Wp[1]")
residual.idx <- which(header == "residual[1]")
init_moist.idx <- which(header == "init_moist[1]")

# Calculate
expt.map <- array(NA, dim = c(length(lons), length(lats), nlayers))
ksat.map <- array(NA, dim = c(length(lons), length(lats), nlayers))
phi_s.map <- array(NA, dim = c(length(lons), length(lats), nlayers))
bubble.map <- array(NA, dim = c(length(lons), length(lats), nlayers))
quartz.map <- array(NA, dim = c(length(lons), length(lats), nlayers))
bulk_density.map <- array(NA, dim = c(length(lons), length(lats), nlayers))
soil_density.map <- array(NA, dim = c(length(lons), length(lats), nlayers))
wcr.map <- array(NA, dim = c(length(lons), length(lats), nlayers))
wp.map <- array(NA, dim = c(length(lons), length(lats), nlayers))
residual.map <- array(NA, dim = c(length(lons), length(lats), nlayers))
init_moist.map <- array(NA, dim = c(length(lons), length(lats), nlayers))

for (i in 1:nrow(soil)) {
  x <- which(lons == soil[i, 4])
  y <- which(lats == soil[i, 3])

  expt.map[x, y, 1] <- soil[i, expt.idx]
  expt.map[x, y, 2] <- soil[i, expt.idx + 1]
  expt.map[x, y, 3] <- soil[i, expt.idx + 2]
  ksat.map[x, y, 1] <- soil[i, ksat.idx]
  ksat.map[x, y, 2] <- soil[i, ksat.idx + 1]
  ksat.map[x, y, 3] <- soil[i, ksat.idx + 2]
  phi_s.map[x, y, 1] <- soil[i, phi_s.idx]
  phi_s.map[x, y, 2] <- soil[i, phi_s.idx + 1]
  phi_s.map[x, y, 3] <- soil[i, phi_s.idx + 2]
  bubble.map[x, y, 1] <- soil[i, bubble.idx]
  bubble.map[x, y, 2] <- soil[i, bubble.idx + 1]
  bubble.map[x, y, 3] <- soil[i, bubble.idx + 2]
  quartz.map[x, y, 1] <- soil[i, quartz.idx]
  quartz.map[x, y, 2] <- soil[i, quartz.idx + 1]
  quartz.map[x, y, 3] <- soil[i, quartz.idx + 2]
  bulk_density.map[x, y, 1] <- soil[i, bulk_density.idx]
  bulk_density.map[x, y, 2] <- soil[i, bulk_density.idx + 1]
  bulk_density.map[x, y, 3] <- soil[i, bulk_density.idx + 2]
  soil_density.map[x, y, 1] <- soil[i, soil_density.idx]
  soil_density.map[x, y, 2] <- soil[i, soil_density.idx + 1]
  soil_density.map[x, y, 3] <- soil[i, soil_density.idx + 2]
  wcr.map[x, y, 1] <- soil[i, wcr.idx]
  wcr.map[x, y, 2] <- soil[i, wcr.idx + 1]
  wcr.map[x, y, 3] <- soil[i, wcr.idx + 2]
  wp.map[x, y, 1] <- soil[i, wp.idx]
  wp.map[x, y, 2] <- soil[i, wp.idx + 1]
  wp.map[x, y, 3] <- soil[i, wp.idx + 2]
  residual.map[x, y, 1] <- soil[i, residual.idx]
  residual.map[x, y, 2] <- soil[i, residual.idx + 1]
  residual.map[x, y, 3] <- soil[i, residual.idx + 2]
  init_moist.map[x, y, 1] <- soil[i, init_moist.idx]
  init_moist.map[x, y, 2] <- soil[i, init_moist.idx + 1]
  init_moist.map[x, y, 3] <- soil[i, init_moist.idx + 2]
}
for (i in 1:nlayers) {
  image.plot(wp.map[, , i], main = i)
}

# Save
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilExpt")
dir.create(dirname(soil.out.tmp))
saveRDS(expt.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilKsat")
dir.create(dirname(soil.out.tmp))
saveRDS(ksat.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilPhis")
dir.create(dirname(soil.out.tmp))
saveRDS(phi_s.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilBubble")
dir.create(dirname(soil.out.tmp))
saveRDS(bubble.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilQuartz")
dir.create(dirname(soil.out.tmp))
saveRDS(quartz.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilBulkDensity")
dir.create(dirname(soil.out.tmp))
saveRDS(bulk_density.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilSoilDensity")
dir.create(dirname(soil.out.tmp))
saveRDS(soil_density.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilWcr")
dir.create(dirname(soil.out.tmp))
saveRDS(wcr.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilWp")
dir.create(dirname(soil.out.tmp))
saveRDS(wp.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilResidual")
dir.create(dirname(soil.out.tmp))
saveRDS(residual.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilInitMoist")
dir.create(dirname(soil.out.tmp))
saveRDS(init_moist.map, soil.out.tmp)
