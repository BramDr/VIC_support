library(fields)
rm(list = ls())

# Input
calib.file <- "../../../Data/Primary/Nijssen2001/2deg/soil_cal_tran.merged.txt"
uncalib.file <- "../../../Data/Primary/Nijssen2001/2deg/soil_uncal.txt"
mapping.file <- "../../../Data/Primary/Nijssen2001/2deg/soil_cal_tran.merged.txt"
header.file <- "../../../Data/Primary/Nijssen2001/2deg/world.soil.parameter.hdr"
factor.out <- "./Saves/factor_Nijssen120min_30min_global.RDS"

# Load
calib <- read.table(calib.file)
uncalib <- read.table(uncalib.file)
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

# Calculate
calib.expt.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
calib.ksat.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
uncalib.expt.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
uncalib.ksat.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))

for (i in 1:nrow(calib)) {
  x <- which(lons.120min == mapping[i, 3])
  y <- which(lats.120min == mapping[i, 2])

  calib.expt.map[x, y, 1] <- calib[i, expt.idx]
  calib.expt.map[x, y, 2] <- calib[i, expt.idx + 1]
  calib.expt.map[x, y, 3] <- calib[i, expt.idx + 2]
  calib.ksat.map[x, y, 1] <- calib[i, ksat.idx]
  calib.ksat.map[x, y, 2] <- calib[i, ksat.idx + 1]
  calib.ksat.map[x, y, 3] <- calib[i, ksat.idx + 2]
}

for (i in 1:nrow(uncalib)) {
  x <- which(lons.120min == mapping[i, 3])
  y <- which(lats.120min == mapping[i, 2])

  uncalib.expt.map[x, y, 1] <- uncalib[i, expt.idx]
  uncalib.expt.map[x, y, 2] <- uncalib[i, expt.idx + 1]
  uncalib.expt.map[x, y, 3] <- uncalib[i, expt.idx + 2]
  uncalib.ksat.map[x, y, 1] <- uncalib[i, ksat.idx]
  uncalib.ksat.map[x, y, 2] <- uncalib[i, ksat.idx + 1]
  uncalib.ksat.map[x, y, 3] <- uncalib[i, ksat.idx + 2]
}

factor.expt.map <- calib.expt.map / uncalib.expt.map
factor.ksat.map <- calib.ksat.map / uncalib.ksat.map
layout(mat = matrix(1:3, nrow = 3))
for (i in 1:nlayers) {
  image.plot(uncalib.ksat.map[, , i], main = i)
  image.plot(calib.ksat.map[, , i], main = i)
  image.plot(factor.ksat.map[, , i], main = i)
}
for (i in 1:nlayers) {
  image.plot(uncalib.expt.map[, , i], main = i)
  image.plot(calib.expt.map[, , i], main = i)
  image.plot(factor.expt.map[, , i], main = i)
}
layout(mat = matrix(1))

factor.expt.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
factor.ksat.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
for (x in 1:dim(factor.expt.map.adj)[1]) {
  for (y in 1:dim(factor.expt.map.adj)[2]) {
    x.map <- mapping.map[x, y, 1]
    y.map <- mapping.map[x, y, 2]

    factor.expt.map.adj[x, y, ] <- factor.expt.map[x.map, y.map, ]
    factor.ksat.map.adj[x, y, ] <- factor.ksat.map[x.map, y.map, ]
  }
}
for (i in 1:nlayers) {
  image.plot(factor.ksat.map.adj[, , i], main = i)
}

# Save
factor.out.tmp <- gsub(x = factor.out, pattern = "factor", replacement = "factorExpt")
dir.create(dirname(factor.out.tmp))
saveRDS(factor.expt.map.adj, factor.out.tmp)
factor.out.tmp <- gsub(x = factor.out, pattern = "factor", replacement = "factorKsat")
dir.create(dirname(factor.out.tmp))
saveRDS(factor.ksat.map.adj, factor.out.tmp)
