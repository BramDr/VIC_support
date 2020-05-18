library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Nijssen2001/2deg/soil_cal_tran.merged.txt"
mapping.file <- "../../../Data/Primary/Nijssen2001/2deg/soil_cal_tran.merged.txt"
depth.out <- "./Saves/depth_Nijssen120min_30min_global.RDS"

# Load
soil <- read.table(file = soil.file)
mapping <- read.table(file = mapping.file)
mapping <- mapping[, 2:4]
nlayers <- 3

# Setup
lons.120min <- seq(from = -179, to = 179, by = 2)
lats.120min <- seq(from = -89, to = 89, by = 2)
lons.30min <- seq(from = -179.75, to = 179.75, by = 0.5)
lats.30min <- seq(from = -89.75, to = 89.75, by = 0.5)

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

# Calculate
depth.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nlayers))
for (i in 1:nrow(soil)) {
  x <- which(lons.120min == mapping[i, 3])
  y <- which(lats.120min == mapping[i, 2])

  depth.map[x, y, 1] <- soil[i, 23]
  depth.map[x, y, 2] <- soil[i, 24]
  depth.map[x, y, 3] <- soil[i, 25]
}
image.plot(depth.map[, , 2])

depth.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nlayers))
for (x in 1:dim(depth.map.adj)[1]) {
  for (y in 1:dim(depth.map.adj)[2]) {
    x.map <- mapping.map[x, y, 1]
    y.map <- mapping.map[x, y, 2]

    depth.map.adj[x, y, ] <- depth.map[x.map, y.map, ]
  }
}
image.plot(depth.map.adj[, , 2])

# Save
dir.create(dirname(depth.out))
saveRDS(depth.map.adj, depth.out)
