library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Nijssen2001/2deg/soil_cal_tran.merged.txt"
mapping.file <- "../../../Data/Primary/Nijssen2001/2deg/soil_cal_tran.merged.txt"
header.file <- "../../../Data/Primary/Nijssen2001/2deg/world.soil.parameter.hdr"
soil.out <- "./Saves/soil_Nijssen120min_30min_global.RDS"

# Load
soil <- read.table(soil.file)
header <- read.table(header.file)
mapping <- read.table(file = mapping.file)
mapping <- mapping[, 2:4]

# Setup
lons.120min <- seq(from = -179, to = 179, by = 2)
lats.120min <- seq(from = -89, to = 89, by = 2)
lons.30min <- seq(from = -179.75, to = 179.75, by = 0.5)
lats.30min <- seq(from = -89.75, to = 89.75, by = 0.5)
nlayers <- 3

mapping.map <- array(NA, dim = c(length(lons.30min), length(lats.30min), 2))
for (x in 1:dim(mapping.map)[1]) {
  for (y in 1:dim(mapping.map)[2]) {
    x.map <- which.min(abs(lons.120min - lons.30min[x]))
    y.map <- which.min(abs(lats.120min - lats.30min[y]))
    mapping.map[x, y, 1] <- x.map
    mapping.map[x, y, 2] <- y.map
  }
}
image.plot(mapping.map[, , 1])
image.plot(mapping.map[, , 2])

expt.idx <- which(header == "N[1]")
ksat.idx <- which(header == "Ksat[1]")
phi_s.idx <- which(header == "phi_s[1]")
bubble.idx <- which(header == "bubble")
quartz.idx <- which(header == "quartz")
bulk_density.idx <- which(header == "bulk_density[1]")
soil_density.idx <- which(header == "soil_density[1]")
wcr.idx <- which(header == "Wcr[1]")
wp.idx <- which(header == "Wp[1]")
residual.idx <- which(header == "residual[1]")
init_moist.idx <- which(header == "init_moist[1]")

# Calculate
expt.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
ksat.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
phi_s.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
bubble.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
quartz.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
bulk_density.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
soil_density.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
wcr.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
wp.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
residual.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
init_moist.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))

for (i in 1:nrow(soil)) {
  x <- which(lons.120min == mapping[i, 3])
  y <- which(lats.120min == mapping[i, 2])

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
  bubble.map[x, y, 2] <- soil[i, bubble.idx]
  bubble.map[x, y, 3] <- soil[i, bubble.idx]
  quartz.map[x, y, 1] <- soil[i, quartz.idx]
  quartz.map[x, y, 2] <- soil[i, quartz.idx]
  quartz.map[x, y, 3] <- soil[i, quartz.idx]
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
  image.plot(expt.map[, , i], main = i)
}

expt.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
ksat.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
phi_s.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
bubble.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
quartz.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
bulk_density.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
soil_density.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
wcr.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
wp.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
residual.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
init_moist.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
for (x in 1:dim(expt.map.adj)[1]) {
  for (y in 1:dim(expt.map.adj)[2]) {
    x.map <- mapping.map[x, y, 1]
    y.map <- mapping.map[x, y, 2]

    expt.map.adj[x, y, ] <- expt.map[x.map, y.map, ]
    ksat.map.adj[x, y, ] <- ksat.map[x.map, y.map, ]
    phi_s.map.adj[x, y, ] <- phi_s.map[x.map, y.map, ]
    bubble.map.adj[x, y, ] <- bubble.map[x.map, y.map, ]
    quartz.map.adj[x, y, ] <- quartz.map[x.map, y.map, ]
    bulk_density.map.adj[x, y, ] <- bulk_density.map[x.map, y.map, ]
    soil_density.map.adj[x, y, ] <- soil_density.map[x.map, y.map, ]
    wcr.map.adj[x, y, ] <- wcr.map[x.map, y.map, ]
    wp.map.adj[x, y, ] <- wp.map[x.map, y.map, ]
    residual.map.adj[x, y, ] <- residual.map[x.map, y.map, ]
    init_moist.map.adj[x, y, ] <- init_moist.map[x.map, y.map, ]
  }
}
for (i in 1:nlayers) {
  image.plot(expt.map.adj[, , i], main = i)
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
