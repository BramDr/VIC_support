library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Dahri2020/vic_in/soilparams.txt"
header.file <- "../../../Data/Primary/Nijssen2001/2deg/world.soil.parameter.hdr"
soil.out <- "../../../Data/Transformed/Parameters/soil_Dahri_5min_Indus.RDS"

# Load
soil <- read.table(soil.file)
header <- read.table(header.file, stringsAsFactors = F)
header <- c(unlist(header[, 1:27]), paste0("bubble[", 1:3, "]"), paste0("quartz[", 1:3, "]"), unlist(header[, 30:48]), "frozen_flag")

# Setup
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]
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
surface_roughness.idx <- which(header == "surface_roughness")
snow_roughness.idx <- which(header == "snow_roughness")
frozen_flag.idx <- which(header == "frozen_flag")
dp.idx <- which(header == "dp")

# Calculate
expt.map <- array(NA, dim = c(length(out.lons), length(out.lats), nlayers))
ksat.map <- array(NA, dim = c(length(out.lons), length(out.lats), nlayers))
phi_s.map <- array(NA, dim = c(length(out.lons), length(out.lats), nlayers))
bubble.map <- array(NA, dim = c(length(out.lons), length(out.lats), nlayers))
quartz.map <- array(NA, dim = c(length(out.lons), length(out.lats), nlayers))
bulk_density.map <- array(NA, dim = c(length(out.lons), length(out.lats), nlayers))
soil_density.map <- array(NA, dim = c(length(out.lons), length(out.lats), nlayers))
wcr.map <- array(NA, dim = c(length(out.lons), length(out.lats), nlayers))
wp.map <- array(NA, dim = c(length(out.lons), length(out.lats), nlayers))
residual.map <- array(NA, dim = c(length(out.lons), length(out.lats), nlayers))
init_moist.map <- array(NA, dim = c(length(out.lons), length(out.lats), nlayers))
surface_roughness.map <- array(NA, dim = c(length(out.lons), length(out.lats)))
snow_roughness.map <- array(NA, dim = c(length(out.lons), length(out.lats)))
frozen_flag.map <- array(NA, dim = c(length(out.lons), length(out.lats)))
dp.map <- array(NA, dim = c(length(out.lons), length(out.lats)))

for (i in 1:nrow(soil)) {
  x.diff = abs(out.lons - soil[i, 4])
  y.diff = abs(out.lats - soil[i, 3])
  if(min(x.diff) > resolution / 2 || min(y.diff) > resolution / 2){
    next
  }
  
  x = which.min(x.diff)
  y = which.min(y.diff)

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
  surface_roughness.map[x, y] <- soil[i, surface_roughness.idx]
  snow_roughness.map[x, y] <- soil[i, snow_roughness.idx]
  frozen_flag.map[x, y] <- soil[i, frozen_flag.idx]
  dp.map[x, y] <- soil[i, dp.idx]
}
for (i in 1:nlayers) {
  image.plot(bulk_density.map[, , i], main = i)
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
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilSurfaceRoughness")
dir.create(dirname(soil.out.tmp))
saveRDS(surface_roughness.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilSnowRoughness")
dir.create(dirname(soil.out.tmp))
saveRDS(snow_roughness.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilFrozenFlag")
dir.create(dirname(soil.out.tmp))
saveRDS(frozen_flag.map, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilDp")
dir.create(dirname(soil.out.tmp))
saveRDS(dp.map, soil.out.tmp)
