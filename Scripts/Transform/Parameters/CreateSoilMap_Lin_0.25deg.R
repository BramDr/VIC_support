library(fields)
# library(raster)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Lin2019/global_VIC_soil_0.25d.txt"
header.file <- "../../../Data/Primary/Nijssen2001/2deg/world.soil.parameter.hdr"
soil.out <- "../../../Data/Transformed/Parameters/soil_Lin15min_30min_global.RDS"

# Load
soil <- read.table(soil.file)
soil[soil[, 4] > 180, 4] <- soil[soil[, 4] > 180, 4] - 360

header <- read.table(header.file, stringsAsFactors = F)
header <- c(unlist(header[, 1:27]), paste0("bubble[", 1:3, "]"), paste0("quartz[", 1:3, "]"), unlist(header[, 30:48]), "frozen_flag")

# Setup
lons.15min <- seq(from = -179.875, to = 179.875, by = 0.25)
lats.15min <- seq(from = -89.875, to = 89.875, by = 0.25)
lons.30min <- seq(from = -179.75, to = 179.75, by = 0.5)
lats.30min <- seq(from = -89.75, to = 89.75, by = 0.5)
nlayers <- 3

mapping.map <- array(NA, dim = c(length(lons.15min), length(lats.15min), 2))
for (x in 1:dim(mapping.map)[1]) {
  for (y in 1:dim(mapping.map)[2]) {
    x.map <- which.min(abs(lons.30min - lons.15min[x]))
    y.map <- which.min(abs(lats.30min - lats.15min[y]))
    mapping.map[x, y, 1] <- x.map
    mapping.map[x, y, 2] <- y.map
  }
}
# image.plot(mapping.map[, , 1])
# image.plot(mapping.map[, , 2])

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

averageMap <- function(map, count) {
  for (i in 1:dim(map)[3]) {
    mapi <- map[, , i]
    mapi <- mapi / count
    mapi[count == 0] <- NA
    map[, , i] <- mapi
  }
  return(map)
}

# Calculate
expt.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nlayers))
ksat.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nlayers))
phi_s.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nlayers))
bubble.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nlayers))
quartz.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nlayers))
bulk_density.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nlayers))
soil_density.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nlayers))
wcr.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nlayers))
wp.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nlayers))
residual.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nlayers))
init_moist.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nlayers))
count.map <- array(0, dim = c(length(lons.30min), length(lats.30min)))

for (i in 1:nrow(soil)) {
  print(i)

  x.15min <- which(lons.15min == soil[i, 4])
  y.15min <- which(lats.15min == soil[i, 3])

  x <- mapping.map[x.15min, y.15min, 1]
  y <- mapping.map[x.15min, y.15min, 2]

  expt.map[x, y, 1] <- expt.map[x, y, 1] + soil[i, expt.idx]
  expt.map[x, y, 2] <- expt.map[x, y, 2] + soil[i, expt.idx + 1]
  expt.map[x, y, 3] <- expt.map[x, y, 3] + soil[i, expt.idx + 2]
  ksat.map[x, y, 1] <- ksat.map[x, y, 1] + soil[i, ksat.idx]
  ksat.map[x, y, 2] <- ksat.map[x, y, 2] + soil[i, ksat.idx + 1]
  ksat.map[x, y, 3] <- ksat.map[x, y, 3] + soil[i, ksat.idx + 2]
  phi_s.map[x, y, 1] <- phi_s.map[x, y, 1] + soil[i, phi_s.idx]
  phi_s.map[x, y, 2] <- phi_s.map[x, y, 2] + soil[i, phi_s.idx + 1]
  phi_s.map[x, y, 3] <- phi_s.map[x, y, 3] + soil[i, phi_s.idx + 2]
  bubble.map[x, y, 1] <- bubble.map[x, y, 1] + soil[i, bubble.idx]
  bubble.map[x, y, 2] <- bubble.map[x, y, 2] + soil[i, bubble.idx + 1]
  bubble.map[x, y, 3] <- bubble.map[x, y, 3] + soil[i, bubble.idx + 2]
  quartz.map[x, y, 1] <- quartz.map[x, y, 1] + soil[i, quartz.idx]
  quartz.map[x, y, 2] <- quartz.map[x, y, 2] + soil[i, quartz.idx + 1]
  quartz.map[x, y, 3] <- quartz.map[x, y, 3] + soil[i, quartz.idx + 2]
  bulk_density.map[x, y, 1] <- bulk_density.map[x, y, 1] + soil[i, bulk_density.idx]
  bulk_density.map[x, y, 2] <- bulk_density.map[x, y, 2] + soil[i, bulk_density.idx + 1]
  bulk_density.map[x, y, 3] <- bulk_density.map[x, y, 3] + soil[i, bulk_density.idx + 2]
  soil_density.map[x, y, 1] <- soil_density.map[x, y, 1] + soil[i, soil_density.idx]
  soil_density.map[x, y, 2] <- soil_density.map[x, y, 2] + soil[i, soil_density.idx + 1]
  soil_density.map[x, y, 3] <- soil_density.map[x, y, 3] + soil[i, soil_density.idx + 2]
  wcr.map[x, y, 1] <- wcr.map[x, y, 1] + soil[i, wcr.idx]
  wcr.map[x, y, 2] <- wcr.map[x, y, 2] + soil[i, wcr.idx + 1]
  wcr.map[x, y, 3] <- wcr.map[x, y, 3] + soil[i, wcr.idx + 2]
  wp.map[x, y, 1] <- wp.map[x, y, 1] + soil[i, wp.idx]
  wp.map[x, y, 2] <- wp.map[x, y, 2] + soil[i, wp.idx + 1]
  wp.map[x, y, 3] <- wp.map[x, y, 3] + soil[i, wp.idx + 2]
  residual.map[x, y, 1] <- residual.map[x, y, 1] + soil[i, residual.idx]
  residual.map[x, y, 2] <- residual.map[x, y, 2] + soil[i, residual.idx + 1]
  residual.map[x, y, 3] <- residual.map[x, y, 3] + soil[i, residual.idx + 2]
  init_moist.map[x, y, 1] <- init_moist.map[x, y, 1] + soil[i, init_moist.idx]
  init_moist.map[x, y, 2] <- init_moist.map[x, y, 2] + soil[i, init_moist.idx + 1]
  init_moist.map[x, y, 3] <- init_moist.map[x, y, 3] + soil[i, init_moist.idx + 2]
  count.map[x, y] <- count.map[x, y] + 1
}

expt.map.adj <- averageMap(expt.map, count.map)
ksat.map.adj <- averageMap(ksat.map, count.map)
phi_s.map.adj <- averageMap(phi_s.map, count.map)
bubble.map.adj <- averageMap(bubble.map, count.map)
quartz.map.adj <- averageMap(quartz.map, count.map)
bulk_density.map.adj <- averageMap(bulk_density.map, count.map)
soil_density.map.adj <- averageMap(soil_density.map, count.map)
wcr.map.adj <- averageMap(wcr.map, count.map)
wp.map.adj <- averageMap(wp.map, count.map)
residual.map.adj <- averageMap(residual.map, count.map)
init_moist.map.adj <- averageMap(init_moist.map, count.map)

for (i in 1:nlayers) {
  image.plot(wp.map.adj[, , i], main = i)
}

# Save
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilExpt")
dir.create(dirname(soil.out.tmp))
saveRDS(expt.map.adj, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilKsat")
dir.create(dirname(soil.out.tmp))
saveRDS(ksat.map.adj, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilPhis")
dir.create(dirname(soil.out.tmp))
saveRDS(phi_s.map.adj, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilBubble")
dir.create(dirname(soil.out.tmp))
saveRDS(bubble.map.adj, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilQuartz")
dir.create(dirname(soil.out.tmp))
saveRDS(quartz.map.adj, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilBulkDensity")
dir.create(dirname(soil.out.tmp))
saveRDS(bulk_density.map.adj, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilSoilDensity")
dir.create(dirname(soil.out.tmp))
saveRDS(soil_density.map.adj, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilWcr")
dir.create(dirname(soil.out.tmp))
saveRDS(wcr.map.adj, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilWp")
dir.create(dirname(soil.out.tmp))
saveRDS(wp.map.adj, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilResidual")
dir.create(dirname(soil.out.tmp))
saveRDS(residual.map.adj, soil.out.tmp)
soil.out.tmp <- gsub(x = soil.out, pattern = "soil", replacement = "soilInitMoist")
dir.create(dirname(soil.out.tmp))
saveRDS(init_moist.map.adj, soil.out.tmp)
